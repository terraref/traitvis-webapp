library(rgeos)
library(dplyr)
library(raster)
library(mapview)
library(sf)
library(leafem)

# render leaflet map from traits for a given date
render_site_map <- function(traits, render_date, legend_title, image_path, display_heat_map) {
  
  if(!file.exists(image_path)){
    return()
  }else{
    
    # get most recent traits for each site
    # convert each site's geometry to a sfc object
    latest_traits <- subset(traits, date <= render_date) %>% 
      group_by(geometry) %>% 
      top_n(1, date) %>% 
      mutate(site_poly = st_as_sfc(geometry))
    
    pal <- colorNumeric(
      palette = 'Greens',
      domain = traits[[ 'mean' ]]
    )
    
    map <- leaflet(options = leafletOptions(minZoom = 18, maxZoom = 21))  %>%
      addProviderTiles(providers$Esri.WorldImagery) 
    
    # eventually want to overlay with stitched image from current day
    # see /data/terraref/sites/ua-mac/Level_1/fullfield/
    # cannot use addRasterImage (https://rstudio.github.io/leaflet/raster.html) for RasterStack 
    # using multiband RGB thumbs, so use mapview::viewRGB() instead 
    
    # add polygon for each site, color by trait mean value
    # coerce data to multipolygon
    if(display_heat_map == 'Yes'){
      map <- addFeatures(map,
                         data = st_cast(latest_traits[[ 'site_poly' ]], "MULTIPOLYGON"),
                         color = pal(latest_traits[[ 'mean' ]]),
                         opacity = 0,
                         fillColor = pal(latest_traits[[ 'mean' ]]),
                         fillOpacity = 0.8,
                         group = 'Heat map')
      
      map <- addLayersControl(map,
                              overlayGroups = "Heat map",
                              position = "topright")
    }else{
      map <- addLayersControl(map, 
                              position = "topright")
    }
    
    map <- addLegend(map, "bottomright", 
                     pal = pal, 
                     title = legend_title,
                     values = traits[[ 'mean' ]])
    
    fullfield_image <- stack(image_path)
    
    map <- viewRGB(x = fullfield_image,
                   map = map,
                   layer.name = 'full-field image')
    
    
    map <- removeHomeButton(map)
    
    map
    
  }
  
}
