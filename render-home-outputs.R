# directory containing full-field images
image_dir <- '~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs'

if(dir.exists(image_dir)){
  # dates that have full-field images
  image_dates <- as.Date(unique(unlist(str_extract_all(list.files(image_dir),'[0-9]{4}-[0-9]{2}-[0-9]{2}'))))
} 

# render functions for home page

# render selection menu from available variables in a given subexperiment
render_variable_menu <- function(subexp_name, id_str, output, full_cache_data) {
  
  variable_names <- names(full_cache_data[[ subexp_name ]][[ 'trait_data' ]])
  
  output[[ paste0('variable_select_', id_str) ]] <- renderUI({
    selectInput(paste0('selected_variable_', id_str), 'Variable', variable_names)
  })
}

# render selection menu from available cultivars in a given subexperiment, for the selected variable
render_cultivar_menu <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  output[[ paste0('cultivar_select_', id_str) ]] <- renderUI({
    
    req(input[[ paste0('selected_variable_', id_str) ]])
    
    trait_records <- full_cache_data[[ subexp_name ]][[ 'trait_data' ]][[ input[[ paste0('selected_variable_', id_str) ]] ]][[ 'traits' ]]
    unique_cultivars <- unique(trait_records[[ 'cultivar_name' ]])
    
    selectInput(paste0('selected_cultivar_', id_str), 'Cultivar', c('None', unique_cultivars))
  })
}

render_home_plot <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  output[[ paste0('trait_plot_', id_str) ]] <- renderPlot({
    
    req(input[[ paste0('selected_variable_', id_str) ]])
    req(input[[ paste0('selected_cultivar_', id_str) ]])
    
    selected_subexp_data <- full_cache_data[[ subexp_name ]]
    selected_variable <- input[[ paste0('selected_variable_', id_str) ]]
    selected_cultivar <- input[[ paste0('selected_cultivar_', id_str) ]]
    
    plot_data <- selected_subexp_data[[ 'trait_data' ]][[ selected_variable ]][[ 'traits' ]]
    units <- selected_subexp_data[[ 'trait_data' ]][[ selected_variable ]][[ 'units' ]]
    start_date <- as.Date(selected_subexp_data[[ 'start_date' ]])
    end_date <- as.Date(selected_subexp_data[[ 'end_date' ]])
    
    render_trait_plot(plot_data, selected_variable, selected_cultivar, units, start_date, end_date)
    
  })
}

render_home_mgmt_timeline <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  output[[ paste0('mgmt_timeline_', id_str) ]] <- renderTimevis({
    
    management_data <- full_cache_data[[ subexp_name ]][[ 'managements' ]]
    
    render_mgmt_timeline(management_data)
    
  })
}

render_home_plot_hover <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  output[[ paste0('plot_hover_info_', id_str) ]] <- renderUI({
    
    render_plot_hover(paste0('plot_hover_', id_str), input)
    
  })
}


render_home_timeline_hover <- function(subexp_name, id_str, input, output, full_cache_data) {
  
  output[[ paste0('mgmt_select_info_', id_str) ]] <- renderUI({
    
    subexp_data <- full_cache_data[[ subexp_name ]]
    
    render_timeline_hover(paste0('mgmt_timeline_', id_str, '_selected'), subexp_data, input)
    
  })
}

render_home_map <- function(subexp_name, id_str, input, output, full_cache_data) {
  
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
