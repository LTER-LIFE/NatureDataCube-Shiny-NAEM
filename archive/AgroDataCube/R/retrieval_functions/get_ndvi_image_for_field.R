# Get raster data geotiff for NDVI (ndvi_image) for specific date
# Created     : 27 November 2025
# Last update : 27 November 2025

# Load packages ----------------------------------------------------------------

library(httr)

# Arguments --------------------------------------------------------------------

# fieldid   : ID of a field in the Netherlands
# date      : yyyymmdd
# api_key   : user specific API key needed to retrieve data from AgroDataCube (request here: https://agrodatacube.wur.nl/api/register.jsp)

# Function to get raster data geotiff for NDVI ---------------------------------

get_ndvi_image_for_field <- function(fieldid,
                                     date,
                                     api_key) {
  
  base_url <- "https://agrodatacube.wur.nl/api/v2/rest/fields/"
  my_url <- paste0(base_url, fieldid, "/ndvi_image?date=", date, "&output_epsg=4326")
  response <- httr::GET(my_url, add_headers(token = api_key))
  geojson <- httr::content(response, as = "text", encoding = "UTF-8")
  ndvi_image <- geojsonio::geojson_sf(geojson)   
  
  return(ndvi_image)
}

