library(rgeos)
library(dplyr)
library(raster)
library(mapview)
library(sf)
library(leafem)


# render leaflet map from traits for a given date
# overlay fullfield image if thumb avaiable for selected date
render_site_map <- function(traits, render_date, legend_title, overlay_image = 0) {
  
  # get most recent traits for each site
  # convert each site's geometry to a sfc object # will coerce polygons into multipolygon
  latest_traits <- subset(traits, as.Date(date) <= render_date & !is.na(geometry)) %>% 
    mutate(site_poly = st_as_sfc(geometry)) %>% 
    group_by(geometry) %>% 
    top_n(1, date) 

  pal <- colorNumeric(
    palette = 'Greens',
    domain = traits[[ 'mean' ]]
  )
  
  map <- leaflet(options = leafletOptions(minZoom = 18, maxZoom = 21))  %>%
    addProviderTiles(providers$Esri.WorldImagery) 
  
  if(nrow(latest_traits) > 0){
    # color sites by trait mean value
    # coerce site polygons to multipolygon
    map <- addFeatures(map,
                       data = st_cast(latest_traits[[ 'site_poly' ]], "MULTIPOLYGON"),
                       color = pal(latest_traits[[ 'mean' ]]),
                       opacity = 0,
                       fillColor = pal(latest_traits[[ 'mean' ]]),
                       fillOpacity = 0.8,
                       group = 'Heat map')
    
    map <- addLayersControl(map,
                            overlayGroups = "Heat map",
                            position = "topleft")
    
    map <- addLegend(map,
                     "bottomright", 
                     pal = pal, 
                     title = legend_title,
                     values = traits[[ 'mean' ]])
    
    if(overlay_image == 1){ 
      
      image_dir <- '~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs'
      image_paths <- list.files(image_dir, pattern = as.character(render_date))
      full_image_paths <- paste0(image_dir, '/', image_paths)
      
      for(path in full_image_paths){
        scan_number <- which(full_image_paths == path)
        scan_name <- paste0('scan ', scan_number)
        fullfield_image <- brick(path)
        map <- viewRGB(x = fullfield_image, 
                       r = 1, g = 2, b = 3,
                       quantiles = NULL, 
                       map = map, 
                       layer.name = scan_name)
        map <- removeHomeButton(map@map)
      }
    }
  }
  
  map

}
