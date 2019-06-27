library(rgeos)
library(dplyr)
library(mapview)
library(sf)

# render leaflet map from traits for a given date
render_site_map <- function(traits, render_date, legend_title, image_paths = NULL, image_dir = NULL) {
  
  if(render_date %in% unique(as.Date(traits$date))){
    
    # get most recent traits for each site
    # convert each site's geometry to a sfc object
    latest_traits <- subset(traits, as.Date(date) <= render_date) %>% 
      group_by(geometry) %>% 
      top_n(1, date) %>% 
      mutate(site_poly = st_as_sfc(geometry))
    
    pal <- colorNumeric(
      palette = 'Greens',
      domain = traits[[ 'mean' ]]
    )
    
    map <- leaflet(options = leafletOptions(minZoom = 18, maxZoom = 21))  %>%
      addProviderTiles(providers$Esri.WorldImagery) 
    
    map <- fitBounds(map,
                     lng1 = -111.97520,
                     lng2 = -111.97470,
                     lat1 = 33.07650,
                     lat2 = 33.07440)
    
    # add polygon for each site, color by trait mean value
    # coerce data to multipolygon
    map <- addFeatures(map,
                       data = st_cast(latest_traits[[ 'site_poly' ]], "MULTIPOLYGON"),
                       color = pal(latest_traits[[ 'mean' ]]),
                       opacity = 0,
                       fillColor = pal(latest_traits[[ 'mean' ]]),
                       fillOpacity = 0.8,
                       group = 'Heat map')
    
    
    map <- addLegend(map, "bottomright", 
                     pal = pal, 
                     title = legend_title,
                     values = traits[[ 'mean' ]])
    
    
    map <- addLayersControl(map,
                            overlayGroups = "Heat map",
                            position = "topleft")
    
    if(!is.null(image_paths)){ 
      
      # confirm that the correct images will be used for selected date
      confirm_image_paths <- identical(image_paths, grep(render_date, list.files(image_dir), value = TRUE))
      
      if(confirm_image_paths){
        for(path in image_paths){
          scan_number <- which(image_paths == path)
          scan_name <- paste0('scan ', scan_number)
          fullfield_image <- stack(paste0('~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs/',
                                          path))
          map <- viewRGB(x = fullfield_image, map = map, layer.name = scan_name)
          map <- removeHomeButton(map@map)
        }
      }
    }
    
    map
    
  }else{
    return()
  }
  
}
