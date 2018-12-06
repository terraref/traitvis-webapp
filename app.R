library(shiny)
library(ggplot2)
library(lubridate)
library(timevis)
library(leaflet)
library(cronR)
library(shinythemes)
library(scales)

source('render-site-map.R')

# schedule daily execution of cache refresh
cache_update_cmd <- cron_rscript('cache-refresh.R')

cron_clear(ask = FALSE)
cron_add(command = cache_update_cmd, frequency = 'daily', 
         id = 'cache-update', description = 'daily update of BETYdb cache')

if (!file.exists('cache.RData')){
  message("Calling cache-refresh.R to create cache.RData for the first time.")
  source('cache-refresh.R')
}

# set page UI
ui <- fluidPage(theme = shinytheme('flatly'),
  
  tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
  title = 'TERRA-REF Experiment Data',
  
  tags$img(src = 'logo.png', class = 'push-out'),
  
  # destination for all dynamic UI elements
  uiOutput('page_content')
)

# render UI for a given subexperiment
render_subexp_ui <- function(subexp_name) {
  
  tabPanel(subexp_name,
    
    sidebarPanel(class = 'push-down',
      uiOutput(paste0('variable_select_', subexp_name)),
      uiOutput(paste0('cultivar_select_', subexp_name))
    ),
     
    uiOutput(paste0(paste0('plot_hover_info_', subexp_name))),
    
    mainPanel(class = 'main-panel',
    
    tabsetPanel(
        tabPanel('Plot',
          div(class = 'push-down',
            plotOutput(paste0('trait_plot_', subexp_name), 
                        hover = hoverOpts(id = paste0('plot_hover_', subexp_name)))
          ),
          hr(),
          uiOutput(paste0('mgmt_select_info_', subexp_name)),
          timevisOutput(paste0('mgmt_timeline_', subexp_name))
        ),
        tabPanel('Map',
          div(class = 'map-container push-out',
            uiOutput(paste0('map_date_slider_', subexp_name)),
            leafletOutput(paste0('site_map_', subexp_name), width = '600px', height = '600px')
          )
        )
      )
    )
  )
}

render_experiment_ui <- function(exp_name, full_cache_data) {
  
  subexp_tabs <- lapply(names(full_cache_data[[ exp_name ]]), render_subexp_ui)
  
  do.call(tabPanel, list(exp_name, div(class = 'push-down'),
    do.call(tabsetPanel, subexp_tabs))
  )
}

# render selection menu from available variables in a given subexperiment
render_variable_menu <- function(subexp_name, output, full_cache_data) {
  
  variable_names <- names(full_cache_data[[ subexp_name ]][[ 'trait_data' ]])
  
  output[[ paste0('variable_select_', subexp_name) ]] <- renderUI({
    selectInput(paste0('selected_variable_', subexp_name), 'Variable', variable_names)
  })
}

# render selection menu from available cultivars in a given subexperiment, for the selected variable
render_cultivar_menu <- function(subexp_name, input, output, full_cache_data) {
  
  output[[ paste0('cultivar_select_', subexp_name) ]] <- renderUI({
    
    req(input[[ paste0('selected_variable_', subexp_name) ]])
    
    trait_records <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ input[[ paste0('selected_variable_', subexp_name) ]] ]][[ 'traits' ]]
    unique_cultivars <- unique(trait_records[[ 'cultivar_name' ]])
    
    selectInput(paste0('selected_cultivar_', subexp_name), 'Cultivar', c('None', unique_cultivars))
  })
}

# render box plot time series from trait records in a given subexperiment, for the selected variable
# if a cultivar is selected, render line plot from trait records for that cultivar
render_trait_plot <- function(subexp_name, input, output, full_cache_data) {
  
  output[[ paste0('trait_plot_', subexp_name) ]] <- renderPlot({
    
    req(input[[ paste0('selected_variable_', subexp_name) ]])
    req(input[[ paste0('selected_cultivar_', subexp_name) ]])
    
    selected_subexp_data <- full_cache_data[[ subexp_name ]]
    selected_variable <- input[[ paste0('selected_variable_', subexp_name) ]]
    selected_cultivar <- input[[ paste0('selected_cultivar_', subexp_name) ]]
    
    plot_data <- selected_subexp_data[[ 'trait_data' ]][[ selected_variable ]][[ 'traits' ]]
    data_max <- max(plot_data[[ 'mean' ]])
    
    units <- selected_subexp_data[[ 'trait_data' ]][[ selected_variable ]][[ 'units' ]]
    title <- ifelse(units == '', selected_variable, paste0(selected_variable, ' (', units, ')'))
    
    unique_vals <- unique(plot_data[[ 'mean' ]])
    all_vals_integers <- all(unique_vals %% 1 == 0)
    num_unique_vals <- length(unique_vals)

    plot_data$method[is.na(plot_data$method) | plot_data$method == 'NA'] <- 'Not Provided'

    trait_plot <- ggplot(data = plot_data, aes(x = as.Date(date), y = mean, color = method)) +
      scale_colour_brewer(palette = "Set1")

    {
        if ((num_unique_vals < 20) & (max(unique_vals) < 30) & all_vals_integers) {
          trait_plot <- trait_plot + 
            geom_count() + 
            geom_vline(aes(xintercept = as.numeric(as.Date(date))),
                       color = 'grey')
        } else {
          trait_plot <- trait_plot + 
            geom_violin(scale = 'width', width = 1, aes(group = as.Date(date))) +
            geom_boxplot(outlier.alpha = 0.25, width = 0.2, aes(group = as.Date(date)))
        }
      }
      
    if (selected_cultivar != 'None') {
        title <- paste0(title, '\nCultivar ', selected_cultivar, ' in red')
        trait_plot <- trait_plot + 
          geom_point(data = subset(plot_data, cultivar_name == selected_cultivar), 
                     aes(x = as.Date(date), y = mean, group = site_id)) +
          geom_line(data = subset(plot_data, cultivar_name == selected_cultivar), 
                     size = 0.5, alpha = 0.5, aes(x = as.Date(date), y = mean, group = site_id)) 
    }
    
    trait_plot + 
      labs(
        title = paste0(title, '\n'),
        x = "Date",
        y = units
      ) +
      theme_bw() + 
      theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlim(as.Date(selected_subexp_data[[ 'start_date' ]]), as.Date(selected_subexp_data[[ 'end_date' ]])) +
      ylim(0, data_max)
  })
}

# render timeline from management records in a given subexperiment
render_mgmt_timeline <- function(subexp_name, input, output, full_cache_data) {
  
  output[[ paste0('mgmt_timeline_', subexp_name) ]] <- renderTimevis({
    
    management_data <- full_cache_data[[ subexp_name ]][[ 'managements' ]]
    
    if (nrow(management_data) > 0) {
      
      types <- management_data[[ 'mgmttype' ]]
      dates <- management_data[[ 'date' ]]
      
      timeline_data <- data.frame(
        id = 1:nrow(management_data),
        content = types,
        start = dates
      )
      
      timevis(
        timeline_data,
        options = list(zoomable = FALSE)
      )
    }
  })
}

# render info box for date and value of cursor when hovering box/line plot
render_plot_hover <- function(subexp_name, input, output, full_cache_data) {
  
  output[[ paste0('plot_hover_info_', subexp_name) ]] <- renderUI({
    
    req(input[[ paste0('plot_hover_', subexp_name) ]])
    
    hover <- input[[ paste0('plot_hover_', subexp_name) ]]
    
    wellPanel(class = 'plot-hover-info push-down',
      HTML(paste0(
        'Date', '<br>',
        toString(
          as.Date(hover$x, origin = lubridate::origin)
        ),
        '<br><br>',
        'Value', '<br>',
        format(round(hover$y, 2))
      ))
    )
  })
}

# render info box for date, type, and notes of selected (clicked) timeline item
render_timeline_hover <- function(subexp_name, input, output, full_cache_data) {
  
  output[[ paste0('mgmt_select_info_', subexp_name) ]] <- renderUI({
    
    req(input[[ paste0('mgmt_timeline_', subexp_name, '_selected') ]])
    selected <- input[[ paste0('mgmt_timeline_', subexp_name, '_selected') ]]
    
    management_data <- full_cache_data[[ subexp_name ]][[ 'managements' ]]
    selected_record <- management_data[ as.numeric(selected), ]
    
    formatted_notes <- ''
    if (selected_record[[ 'notes' ]] != '') {
      formatted_notes <- paste0('<br><br>', selected_record[[ 'notes' ]])
    }
      
    
    wellPanel(class = 'mgmt-select-info',
      HTML(paste0(
        selected_record[[ 'mgmttype' ]], '<br>',
        selected_record[[ 'date' ]],
        formatted_notes
      ))
    )
  })
}

render_map <- function(subexp_name, input, output, full_cache_data) {
  
  # render slider input from dates in a given subexperiment
  output[[ paste0('map_date_slider_', subexp_name) ]] <- renderUI({
    sliderInput(paste0('map_date_', subexp_name), 'Date', 
                as.Date(full_cache_data[[ subexp_name ]][[ 'start_date']]), 
                as.Date(full_cache_data[[ subexp_name ]][[ 'end_date' ]]),
                as.Date(full_cache_data[[ subexp_name ]][[ 'end_date' ]]))
  })
  
  # render heat map of sites from trait records in a given subexperiment, for the selected date, variable and cultivar
  output[[ paste0('site_map_', subexp_name) ]] <- renderLeaflet({
    
    req(input[[ paste0('selected_variable_', subexp_name) ]])
    req(input[[ paste0('selected_cultivar_', subexp_name) ]])
    req(input[[ paste0('map_date_', subexp_name) ]])
    
    selected_variable <- input[[ paste0('selected_variable_', subexp_name) ]]
    selected_cultivar <- input[[ paste0('selected_cultivar_', subexp_name) ]]
    render_date <- input [[ paste0('map_date_', subexp_name) ]]
    
    traits <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ selected_variable ]][[ 'traits' ]]
    
    if (selected_cultivar != 'None'){
      traits <- subset(traits, cultivar_name == selected_cultivar)
    }
      
    
    units <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ selected_variable ]][[ 'units' ]]
    if (units != '') {
      units <- paste0('(', units, ')')
    }
      
    legend_title <- paste0(selected_variable, ' ', units)
    
    render_site_map(traits, render_date, legend_title)
  })
}

# render outputs for a given subexperiment
render_subexp_output <- function(subexp_name, input, output, full_cache_data) {
  
  render_variable_menu(subexp_name, output, full_cache_data)
  
  render_cultivar_menu(subexp_name, input, output, full_cache_data)
  
  render_trait_plot(subexp_name, input, output, full_cache_data)
  
  render_plot_hover(subexp_name, input, output, full_cache_data)
  
  if (!is.null(full_cache_data[[ subexp_name ]][[ 'managements' ]])) {
    
    render_mgmt_timeline(subexp_name, input, output, full_cache_data)
    
    render_timeline_hover(subexp_name, input, output, full_cache_data)
    
  }

  render_map(subexp_name, input, output, full_cache_data)
}

render_experiment_output <- function(experiment_name, input, output, full_cache_data) {
  
  lapply(names(full_cache_data[[ experiment_name ]]), render_subexp_output, input, output, full_cache_data[[ experiment_name ]])
}

server <- function(input, output) {
  
  # load 'full_cache_data' object from cache file

  load('cache.RData')
  
  # render UI for all available experiments
  output$page_content <- renderUI({
    subexp_tabs <- lapply(names(full_cache_data), render_experiment_ui, full_cache_data)
    do.call(tabsetPanel, subexp_tabs)
  })
  
  # render outputs for all available experiments
  lapply(names(full_cache_data), render_experiment_output, input, output, full_cache_data)
}

shinyApp(ui = ui, server = server)
