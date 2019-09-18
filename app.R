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
library(shiny.router)

source('render-site-map.R')
source('render-trait-plot.R')
source('render-mgmt-timeline.R')
source('render-home-outputs.R')
source('render-search-outputs.R')

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
        cache_mod_time <- curr_mod_time
        cat(file=stderr(), "Loading cache.RData file completed.", "\n")
        return(full_cache_data)
    } else {
        cat(file=stderr(), "Latest cache.RData file already loaded.", "\n")
        return(full_cache_data)
    }
}

# render UI for a given subexperiment for home page
render_subexp_ui <- function(subexp_name, exp_name) {
  
  id_str <- paste0(exp_name, '_', subexp_name)
  
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

# render outputs for a given subexperiment for home page
render_subexp_output <- function(subexp_name, exp_name, input, output, full_cache_data) {
  
  id_str <- paste0(exp_name, '_', subexp_name)
  
  render_variable_menu(subexp_name, id_str, output, full_cache_data)
  
  render_cultivar_menu(subexp_name, id_str, input, output, full_cache_data)
  
  render_home_plot(subexp_name, id_str, input, output, full_cache_data)
  
  render_home_plot_hover(subexp_name, id_str, input, output, full_cache_data)
  
  if (!is.null(full_cache_data[[ subexp_name ]][[ 'managements' ]])) {
    
    render_home_mgmt_timeline(subexp_name, id_str, input, output, full_cache_data)
    
    render_home_timeline_hover(subexp_name, id_str, input, output, full_cache_data)
    
  }
  
  if(subexp_name != 'Drought Tolerance' & exp_name != 'Danforth Sorghum Pilot'){
    
    render_home_map(subexp_name, id_str, input, output, full_cache_data)
    
  }
  
  render_download_info(exp_name, subexp_name, id_str, input, output, full_cache_data)
  
}

render_experiment_output <- function(experiment_name, input, output, full_cache_data) {
  lapply(names(full_cache_data[[ experiment_name ]]), render_subexp_output, experiment_name, input, output, full_cache_data[[ experiment_name ]])
}

# render ui for search page
render_search_ui <- function(){
  fluidRow(
    column(4, leafletOutput('search_map', width = '350px', height = '700px')),
    column(8, fluidRow(uiOutput('search_plot_hover_box')),
           plotOutput('search_plot',
                      hover = hoverOpts(id = 'search_plot_hover')),
           uiOutput('search_timeline_info'),
           timevisOutput('search_timeline'))
  )
}

# render outputs for search page
render_search_output <- function(full_cache_data, input, output, exp_name, subexp_name, var, cultivar, date){
  
  render_search_map(full_cache_data, input, output,
                    exp_name, subexp_name, var,
                    cultivar, date)
  
  render_search_plot(full_cache_data, input, output,
                     exp_name, subexp_name, var,
                     cultivar)
  
  output$search_plot_hover_box <- renderUI({
    render_plot_hover('search_plot_hover', input)
  })
  
  render_search_mgmt_timeline(full_cache_data, output, exp_name, subexp_name)
  
  render_search_timeline_hover(full_cache_data, input, output,
                               exp_name, subexp_name, 'search_timeline_selected')
  
}

# home page ui 
home_page <- fluidPage(theme = shinytheme('flatly'),
                       tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
                       title = 'TERRA-REF Experiment Data',
                       tags$img(src = 'logo.png', class = 'push-out'),
                       # destination for all dynamic UI elements
                       uiOutput('home_page_content')
)

# search page ui
search_page <- fluidPage(theme = shinytheme('flatly'),
                         tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
                         title = 'TERRA-REF Experiment Data',
                         tags$img(src = 'logo.png', class = 'push-out'),
                         # destination for all UI elements
                         uiOutput('search_page_content')
)

# home page server
home_server <- function(input, output, session) {
  
  # load 'full_cache_data' object from cache file
  full_cache_data <- load_cache(full_cache_data)
  
  # render UI for all available experiments
  output$home_page_content <- renderUI({
    subexp_tabs <- lapply(names(full_cache_data), render_experiment_ui, full_cache_data)
    do.call(tabsetPanel, subexp_tabs)
  })
  
  # render outputs for all available experiments
  lapply(names(full_cache_data), render_experiment_output, input, output, full_cache_data)
}

# search page server
search_server <- function(input, output, session){
  
  # load 'full_cache_data' object from cache file
  full_cache_data <- load_cache(full_cache_data)
  
  # get url parameters
  exp_name <- reactive({ ifelse(is.null(get_query_param()$exp_name),
                                NULL, as.character(get_query_param()$exp_name))})
  
  subexp_name <- reactive({ ifelse(is.null(get_query_param()$subexp_name),
                                   NULL, as.character(get_query_param()$subexp_name)) })
  
  var <- reactive({ ifelse(is.null(get_query_param()$var),
                           NULL, as.character(get_query_param()$var)) })
  
  cultivar <- reactive({ ifelse(is.null(get_query_param()$cultivar),
                                NULL, as.character(get_query_param()$cultivar)) })
  
  date <- reactive({ ifelse(is.null(get_query_param()$date), 
                            NULL, as.Date(get_query_param()$date)) })

  # render search page ui
  output$search_page_content <- renderUI({
    render_search_ui()
  })
  
  # render search page output
  render_search_output(full_cache_data, input, output,
                       exp_name(), subexp_name(), var(), cultivar(), date())

}

# create routing
# pass in ui and server for each page
router <- make_router(
  route("/", home_page, home_server),
  route("search", search_page, search_server)
)

# Create output for home page route
ui <- shinyUI(fluidPage(
  router_ui()
))

# Add router to the server
server <- shinyServer(function(input, output, session) {
  router(input, output, session)
})

shinyApp(ui = ui, server = server)
