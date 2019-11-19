library(shiny)
library(ggplot2)
library(lubridate)
library(timevis)
library(leaflet)
library(cronR)
library(shinythemes)
library(scales)
library(stringr)
library(kableExtra)
library(heritability)

source('render-site-map.R')

# create cache folder in same directory as this script (will do nothing if already exists)
dir.create("./cache", showWarnings = FALSE)
cache_path <- "./cache/cache.RData"
cache_mod_time <- file.mtime(cache_path)
full_cache_data <- NA

load_cache <- function(full_cache_data) {
    curr_mod_time <- file.mtime(cache_path)

    if (curr_mod_time > cache_mod_time || is.na(full_cache_data)) {
        cat(file=stderr(), "Loading cache.RData file.", "\n")
        load(cache_path)
        # TODO: See if it works if we omit this line
        # full_cache_data <- full_cache_data[c("Danforth Sorghum Pilot", "KSU 2016", "MAC Season 1", "MAC Season 2", "MAC Season 3", "MAC Season 4", "MAC Season 6")]
        
        # Sort the tabs alphabetically / numerically 
        full_cache_data <- full_cache_data[sort(names(full_cache_data))]
        cache_mod_time <- curr_mod_time
        cat(file=stderr(), "Loading cache.RData file completed.", "\n")
        return(full_cache_data)
    } else {
        cat(file=stderr(), "Latest cache.RData file already loaded.", "\n")
        return(full_cache_data)
    }
}

# directory containing full-field images
image_dir <- '~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs'

if(dir.exists(image_dir)){
# dates that have full-field images
  image_dates <- as.Date(unique(unlist(str_extract_all(list.files(image_dir),'[0-9]{4}-[0-9]{2}-[0-9]{2}'))))
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
render_subexp_ui <- function(subexp_name, exp_name) {
  
  id_str <- sort(paste0(exp_name, '_', subexp_name))
  
  tabPanel(subexp_name,
    
    sidebarPanel(class = 'push-down',
      uiOutput(paste0('variable_select_', id_str)),
      uiOutput(paste0('cultivar_select_', id_str))
    ),
    
    uiOutput(paste0('plot_hover_info_', id_str)),
    
    mainPanel(class = 'main-panel',
    
    tabsetPanel(
        tabPanel('Plot',
          div(class = 'push-down',
            plotOutput(paste0('trait_plot_', id_str), 
                        hover = hoverOpts(id = paste0('plot_hover_', id_str)))
          ),
          hr(),
          plotOutput(paste0('rep_plot_', id_str)),
          htmlOutput(paste0('variable_table_', id_str)),
          htmlOutput(paste0('method_table_', id_str)),
          hr(),
          uiOutput(paste0('mgmt_select_info_', id_str)),
          timevisOutput(paste0('mgmt_timeline_', id_str))
        ),
        if(subexp_name != 'Drought Tolerance' & exp_name != 'Danforth Sorghum Pilot'){
          tabPanel('Map',
                   div(class = 'map-container push-out',
                       uiOutput(paste0('map_date_slider_', id_str)),
                       htmlOutput(paste0('scan_options_table_', id_str)),
                       leafletOutput(paste0('site_map_', id_str), width = '350px', height = '700px')
                   )
          )
        }else{
          tabPanel('Map',
                   br(),
                   'No map data available for Danforth Sorghum Pilot: Drought Tolerance')
        },
        tabPanel('Download',
                 htmlOutput(paste0('download_info_', id_str)))
      )
    )
  )
}

render_experiment_ui <- function(exp_name, full_cache_data) {
  
  subexp_tabs <- lapply(names(full_cache_data[[ exp_name ]]), render_subexp_ui, exp_name)
  
  do.call(tabPanel, list(exp_name, div(class = 'push-down'),
    do.call(tabsetPanel, subexp_tabs))
  )
}

# render selection menu from available variables in a given subexperiment
render_variable_menu <- function(subexp_name, id_str, output, full_cache_data) {
  
  # sort dropdown menu of variable names alphabetically 
  variable_names <- sort(names(full_cache_data[[ subexp_name ]][[ 'trait_data' ]]))
  
  output[[ paste0('variable_select_', id_str) ]] <- renderUI({
    selectInput(paste0('selected_variable_', id_str), 'Variable', variable_names)
  })
}

# render selection menu from available cultivars in a given subexperiment, for the selected variable
render_cultivar_menu <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  output[[ paste0('cultivar_select_', id_str) ]] <- renderUI({
    
    req(input[[ paste0('selected_variable_', id_str) ]])
    
    trait_records <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ input[[ paste0('selected_variable_', id_str) ]] ]][[ 'traits' ]]
    # sort dropdown menu of cultivar names alphabetically, starting with "None"
    unique_cultivars <- sort(unique(trait_records[[ 'cultivar_name' ]]))
    
    selectInput(paste0('selected_cultivar_', id_str), 'Cultivar', c('None', unique_cultivars))
  })
}

# render box plot time series from trait records in a given subexperiment, for the selected variable
# if a cultivar is selected, render line plot from trait records for that cultivar
render_trait_plot <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  output[[ paste0('trait_plot_', id_str) ]] <- renderPlot({
    
    req(input[[ paste0('selected_variable_', id_str) ]])
    req(input[[ paste0('selected_cultivar_', id_str) ]])
    
    selected_subexp_data <- full_cache_data[[ subexp_name ]]
    selected_variable <- input[[ paste0('selected_variable_', id_str) ]]
    selected_cultivar <- input[[ paste0('selected_cultivar_', id_str) ]]
    
    plot_data <- selected_subexp_data[[ 'trait_data' ]][[ selected_variable ]][[ 'traits' ]]
    data_max <- max(plot_data[[ 'mean' ]])
    
    units <- selected_subexp_data[[ 'trait_data' ]][[ selected_variable ]][[ 'units' ]]
    
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
        title <- paste0(selected_variable, '\nCultivar ', selected_cultivar, ' in black')
        trait_plot <- trait_plot + 
          geom_point(data = subset(plot_data, cultivar_name == selected_cultivar),
                     color = 'black', aes(x = as.Date(date), y = mean, group = site_id)) +
          geom_line(data = subset(plot_data, cultivar_name == selected_cultivar), 
                     size = 0.5, alpha = 0.5, color = 'black', aes(x = as.Date(date), 
                                                                 y = mean, group = site_id)) 
    } else {
        title <- selected_variable
    }
    
    trait_plot + 
      labs(
        title = paste0(title, '\n'),
        caption = paste0('
The above plot is a time series consisting of violin plots that contain a box
plot and outliers. Below you will find a table that defines the phenotype
(variable) being plotted and the method used to measure the phenotype. The box
represents the interquartile range (IQR), and the line in the middle of the box
represents the median. The whiskers of the box extend from the lower (Q1) or
upper (Q3) quartile to the minimum or maximum data point that is not considered
an outlier.  Outliers are data points that are either less than Q1 - 1.5*IQR or
greater than Q3 + 1.5*IQR. Outliers are the points that lie above or below the
whiskers. If a cultivar is selected, a black line will connect the cultivar
specific ndata points.\n'),
        x = "Date",
        y = units
      ) +
      theme_bw() + 
      theme(text = element_text(size = 20), 
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.caption = element_text(hjust = 0, size = 10), 
                  legend.position = 'bottom') +
      xlim(as.Date(selected_subexp_data[[ 'start_date' ]]), as.Date(selected_subexp_data[[ 'end_date' ]])) +
      ylim(0, data_max)
  })
}

# render repeatibility plot for a given subexperiment
render_rep_plot <- function(subexp_name, id_str, input, output, full_cache_data){
  
  output[[ paste0('rep_plot_', id_str) ]] <- renderPlot({
    
    req(input[[ paste0('selected_variable_', id_str) ]])
    selected_variable <- input[[ paste0('selected_variable_', id_str) ]]
    
    selected_subexp_data <- full_cache_data[[ subexp_name ]]
    trait_data <-  selected_subexp_data[[ 'trait_data' ]][[ selected_variable ]][[ 'traits' ]]
    trait_data$date <- as.Date(trait_data$date)
    rep_data <- data.frame()
    
    for(sel_date in as.list(unique(trait_data$date))){
      date_subset <- trait_data[trait_data$date == sel_date,]
      rep <- repeatability(data.vector = date_subset$mean,
                           geno.vector = date_subset$cultivar_name)
      new_row <- data.frame(date = sel_date, repeatability = rep$repeatability)
      rep_data <- rbind(rep_data, new_row)
    }
    
    rep_plot <- ggplot(data = rep_data, aes(x = date, y = repeatability)) +
      geom_point(size = 1.0) + 
      geom_line(size = 0.5, alpha = 0.5, color = 'red') +
      ylim(c(0,1))
    
    rep_plot + 
      labs(
        title = 'Within season heritability',
        x = "Date",
        y = "Repeatibility"
      ) +
      theme_bw() + 
      theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1)) + 
      xlim(as.Date(selected_subexp_data[[ 'start_date' ]]), as.Date(selected_subexp_data[[ 'end_date' ]]))
    
  })
  
}

# render timeline from management records in a given subexperiment
render_mgmt_timeline <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  output[[ paste0('mgmt_timeline_', id_str) ]] <- renderTimevis({
    
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
render_plot_hover <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  output[[ paste0('plot_hover_info_', id_str) ]] <- renderUI({
    
    req(input[[ paste0('plot_hover_', id_str) ]])
    
    hover <- input[[ paste0('plot_hover_', id_str) ]]
    
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

# render table containing variable name and link to BETYdb variable page
render_variable_table <- function(subexp_name, id_str, input, output, full_cache_data){
  
  output[[ paste0('variable_table_', id_str) ]] <- renderText({
    
    req(input[[ paste0('selected_variable_', id_str) ]])
    req(input[[ paste0('selected_cultivar_', id_str) ]])
    
    selected_variable <- input[[ paste0('selected_variable_', id_str) ]]
    selected_cultivar <- input[[ paste0('selected_cultivar_', id_str) ]]
    
    variable_name <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ selected_variable ]][[ 'name' ]]
    variable_id <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ selected_variable ]][[ 'id' ]]
    description <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ selected_variable ]][[ 'description' ]]
    
    
    variable_df <- data.frame('variable' = text_spec(variable_name,
                                                 link = paste0("https://terraref.ncsa.illinois.edu/bety/variables/",
                                                               variable_id)),
                              'description' = description)
    
    variable_kable <- kable(variable_df,
                            format = 'html',
                            escape = FALSE, 
                            padding=-1L) %>% 
      kable_styling(bootstrap_options = 'hover', font_size = 12)
    
#    variable_kable_updated <- column_spec(variable_kable,
#                                          column = 1,
#                                          width = '10cm')
    
  })
  
}

# render table containing method name and link to BETYdb method page
render_method_table <- function(subexp_name, id_str, input, output, full_cache_data){
  
  output[[ paste0('method_table_', id_str) ]] <- renderText({
    
    req(input[[ paste0('selected_variable_', id_str) ]])
    req(input[[ paste0('selected_cultivar_', id_str) ]])
    
    selected_variable <- input[[ paste0('selected_variable_', id_str) ]]
    selected_cultivar <- input[[ paste0('selected_cultivar_', id_str) ]]
    
    traits <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ selected_variable ]][[ 'traits' ]]
    
    methods <- traits %>%
      distinct(method, method_id, method_description) %>%
      mutate(bety_link = paste0("https://terraref.ncsa.illinois.edu/bety/methods/",
                                method_id))
    
    method_df <- data.frame('method' = text_spec(methods$method,
                                               link = methods$bety_link),
                            'description' = methods$method_description)
    
    method_kable <- kable(method_df,
                          format = 'html',
                          escape = FALSE) %>% 
      kable_styling(bootstrap_options = 'hover', font_size = 12)
    
#    method_kable_updated <- column_spec(method_kable)
    
  })
  
}

# render info box for date, type, and notes of selected (clicked) timeline item
render_timeline_hover <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  output[[ paste0('mgmt_select_info_', id_str) ]] <- renderUI({
    
    req(input[[ paste0('mgmt_timeline_', id_str, '_selected') ]])
    selected <- input[[ paste0('mgmt_timeline_', id_str, '_selected') ]]
    
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

render_map <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  # render slider input from dates in a given subexperiment
  output[[ paste0('map_date_slider_', id_str) ]] <- renderUI({
    
    sliderInput(paste0('map_date_', id_str), 'Date',
                as.Date(full_cache_data[[ subexp_name ]][[ 'start_date']]),
                as.Date(full_cache_data[[ subexp_name ]][[ 'end_date' ]]),
                as.Date(full_cache_data[[ subexp_name ]][[ 'end_date' ]]))
    
  })
  
  output[[ paste0('scan_options_table_', id_str) ]] <- renderText({
  
    req(input[[ paste0('map_date_', id_str) ]]) 
    render_date <- input[[ paste0('map_date_', id_str) ]]
    
    # display scan options table for selected date if thumbs exist for date
    if(dir.exists(image_dir) & (render_date %in% image_dates)){
      
      image_paths <- list.files(image_dir, pattern = as.character(render_date))
      scan_matches <- unlist(str_match_all(image_paths,
                                           'rgb_fullfield_L2_ua-mac_[0-9]{4}-[0-9]{2}-[0-9]{2}_(.*)_rgb_thumb\\.tif'))
      scan_names <- str_replace_all(scan_matches[-grep('\\.tif', scan_matches)],
                                    '_',
                                    ' ')
      scan_names_df <- data.frame(scan_number = 1:length(scan_names),
                                  scan_name = scan_names)
      scan_names_kable <- kable(scan_names_df) %>% kable_styling()
      
    }else{
      return()
    }
    
  })
  
  # render heat map of sites from trait records in a given subexperiment, for the selected date, variable and cultivar
  output[[ paste0('site_map_', id_str) ]] <- renderLeaflet({
    
    req(input[[ paste0('selected_variable_', id_str) ]])
    req(input[[ paste0('selected_cultivar_', id_str) ]])
    req(input[[ paste0('map_date_', id_str) ]])
    
    selected_variable <- input[[ paste0('selected_variable_', id_str) ]]
    selected_cultivar <- input[[ paste0('selected_cultivar_', id_str) ]]
    render_date <- input [[ paste0('map_date_', id_str) ]]
    
    traits <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ selected_variable ]][[ 'traits' ]]
    
    if (selected_cultivar != 'None'){
      traits <- subset(traits, cultivar_name == selected_cultivar)
    }
    
    if(dir.exists(image_dir) & (render_date %in% image_dates)){
      # render site map with fullfield image if thumbs available for selected date
      overlay_image <- 1
      render_site_map(selected_variable, traits, render_date, selected_variable, overlay_image)
    }else{
      render_site_map(selected_variable, traits, render_date, selected_variable)
    }
    
  })
}

render_download_info <- function(exp_name, subexp_name, id_str, input, output, full_cache_data){
  
  output[[ paste0('download_info_', id_str) ]] <- renderUI({
    
    req(input[[ paste0('selected_variable_', id_str) ]])
    selected_variable <- input[[ paste0('selected_variable_', id_str) ]]
    
    trait_name <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ selected_variable ]][[ 'name' ]]
    
    if(exp_name == 'Danforth Sorghum Pilot'){
      site <- 'Danforth Plant Science Center Bellweather Phenotyping Facility'
    }else{
      site <- paste0('~', gsub('MAC ', '', exp_name))
    }
    
    text_1 <- "<h1 style='font-size:30px;'>How to access data</h1>"
    text_2 <- paste0("You can access <strong><em>",
                     selected_variable,
                     "</strong></em> data for <strong><em>",
                     exp_name,
                     "</strong></em>, using either:<br>")
    text_3 <- "1.) BETYdb API<br>2.) R traits package"
    text_4 <- "<br><h2 style='font-size:25px;'>API key</h2>"
    text_5 <- paste0("Both methods will require an <strong>API key</strong>.<br><br>",
                     "You can get an API key by signing up for the TERRA Ref BETYdb traits database. ",
                     "Register for BETYdb at ",
                     "<a href='https://terraref.ncsa.illinois.edu/bety/signup'>",
                     "https://terraref.ncsa.illinois.edu/bety/signup</a>.<hr>")
    text_6 <- "<h2 style='font-size:25px;'>R traits package</h2><br>"
    text_7 <- "Install the traits package from CRAN using: <code>install.packages('traits')</code>.<br><br>"
    text_8 <- paste0("You can then use the following chunk of R code to access the data: <br><br>",
                     "<code>library(traits)</code><br><br>",
                     "<code>options(betydb_url = ",
                     "'https://terraref.ncsa.illinois.edu/bety/'",
                     ", betydb_api_version = 'beta', ",
                     "betydb_key = 'YOUR_API_KEY')</code><br><br>")
    text_9 <- paste0("<code>",
                     trait_name)
    text_10 <- paste0(" <- betydb_query(trait = '", trait_name, "', ")
    text_11 <- paste0("sitename = '", site, "', ")
    text_12 <- paste0("limit = 'none')</code>")
    text_13 <- paste0("<hr>",
                      "<h2 style='font-size:25px;'>BETYdb API</h2><br>",
                      "You can also access the data at this URL:<br>")
    text_14 <- paste0("<a>https://terraref.ncsa.illinois.edu/bety/api/v1/search?",
                      "trait=",
                      trait_name,
                      "&sitename=",
                      site,
                      "&limit=none&key=YOUR_API_KEY</a>.<br><br>",
                      "See API <a href='https://www.betydb.org/api/docs'>documentation</a> for details.")
    text_15 <- paste0("<hr>",
                      "<h1 style='font-size:30px;'>TERRA REF Tutorials</h1><br>",
                      "For more detailed information on how to access data, take a look at our tutorials.<br><br>",
                      "GitHub repository: <a href='https://github.com/terraref/tutorials'>",
                      "https://github.com/terraref/tutorials</a><br>",
                      "How to access data using R traits package: ",
                      "<a href='https://github.com/terraref/tutorials/tree/master/traits/03-access-r-traits.Rmd'>",
                      "traits/03-access-r-traits.Rmd</a><br>",
                      "How to access data using BETYdb API: ",
                      "<a href='https://github.com/terraref/tutorials/tree/master/traits/02-betydb-api-access.Rmd'>",
                      "traits/02-betydb-api-access.Rmd</a><br>")
    
    download_text <- HTML(paste(text_1, text_2, text_3,
                                text_4, text_5, text_6,
                                text_7, text_8, text_9,
                                text_10, text_11, text_12,
                                text_13, text_14, text_15))
  })
  
}

# render outputs for a given subexperiment
render_subexp_output <- function(subexp_name, exp_name, input, output, full_cache_data) {
  
  id_str <- paste0(exp_name, '_', subexp_name)
  
  render_variable_menu(subexp_name, id_str, output, full_cache_data)
  
  render_cultivar_menu(subexp_name, id_str, input, output, full_cache_data)
  
  render_trait_plot(subexp_name, id_str, input, output, full_cache_data)
  
  render_plot_hover(subexp_name, id_str, input, output, full_cache_data)
  
  render_rep_plot(subexp_name, id_str, input, output, full_cache_data)
  
  render_variable_table(subexp_name, id_str, input, output, full_cache_data)  
  
  render_method_table(subexp_name, id_str, input, output, full_cache_data)
  
  if (!is.null(full_cache_data[[ subexp_name ]][[ 'managements' ]])) {
    
    render_mgmt_timeline(subexp_name, id_str, input, output, full_cache_data)
    
    render_timeline_hover(subexp_name, id_str, input, output, full_cache_data)
    
  }
  
  if(subexp_name != 'Drought Tolerance' & exp_name != 'Danforth Sorghum Pilot'){
    
    render_map(subexp_name, id_str, input, output, full_cache_data)
    
  }
  
  render_download_info(exp_name, subexp_name, id_str, input, output, full_cache_data)
  
}

render_experiment_output <- function(experiment_name, input, output, full_cache_data) {
  lapply(names(full_cache_data[[ experiment_name ]]), render_subexp_output, experiment_name, input, output, full_cache_data[[ experiment_name ]])
}

server <- function(input, output) {
  
  # load 'full_cache_data' object from cache file
  full_cache_data <- load_cache(full_cache_data)
  
  # render UI for all available experiments
  output$page_content <- renderUI({
    subexp_tabs <- lapply(names(full_cache_data), render_experiment_ui, full_cache_data)
    do.call(tabsetPanel, subexp_tabs)
  })
  
  # render outputs for all available experiments
  lapply(names(full_cache_data), render_experiment_output, input, output, full_cache_data)
}

shinyApp(ui = ui, server = server)
