# directory containing full-field images
image_dir <- '~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs'

if(dir.exists(image_dir)){
  # dates that have full-field images
  image_dates <- as.Date(unique(unlist(str_extract_all(list.files(image_dir),'[0-9]{4}-[0-9]{2}-[0-9]{2}'))))
} 

# render functions for search page
render_search_map <- function(full_cache_data, input, output, exp_name,
                              subexp_name, var,
                              cultivar = 'None', date){
  
  output$search_map <- renderLeaflet({
    
    traits <- full_cache_data[[ exp_name ]][[ subexp_name ]][[ 'trait_data' ]][[ var ]][[ 'traits' ]]
    if (cultivar != 'None'){
      traits <- subset(traits, cultivar_name == cultivar)
    }
    if(dir.exists(image_dir) & (date %in% image_dates)){
      # render site map with fullfield image if thumbs available for selected date
      overlay_image <- 1
      render_site_map(var, traits, date, var, overlay_image)
    }else{
      # render site map without fullfield image
      render_site_map(var, traits, date, var)
    }
    
  })
}

render_search_plot <- function(full_cache_data, input, output, exp_name,
                               subexp_name, var, cultivar){
  
  output$search_plot <- renderPlot({
    subexp_data <- full_cache_data[[ exp_name ]][[ subexp_name ]]
    traits <- subexp_data[[ 'trait_data' ]][[ var ]][[ 'traits' ]]
    units <- subexp_data[[ 'trait_data' ]][[ var ]][[ 'units' ]]
    start_date <- as.Date(subexp_data[[ 'start_date' ]])
    end_date <- as.Date(subexp_data[[ 'end_date' ]])
    render_trait_plot(traits, var, cultivar, units, start_date, end_date)
  })
  
}

render_search_mgmt_timeline <- function(full_cache_data, output, exp_name, subexp_name){
  
  output$search_timeline <- renderTimevis({
    
    management_data <- full_cache_data[[ exp_name ]][[ subexp_name ]][[ 'managements' ]]
    
    render_mgmt_timeline(management_data)
    
  })
  
}

render_search_timeline_hover <- function(full_cache_data, input, output, exp_name, subexp_name, selected_input_id){
  
  output$search_timeline_info <- renderUI({
    
    subexp_data <- full_cache_data[[ exp_name ]][[ subexp_name ]]
    
    render_timeline_hover(selected_input_id, subexp_data, input)
    
  })
}
