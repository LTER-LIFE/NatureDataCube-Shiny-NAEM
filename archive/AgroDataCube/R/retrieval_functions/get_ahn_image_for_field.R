# Get raster data geotiff for AHN (ahn_image)
# Created     : 27 November 2025
# Last update : 27 November 2025

# Load packages ----------------------------------------------------------------

library(httr)

# Arguments --------------------------------------------------------------------

# fieldid   : ID of a field in the Netherlands
# api_key   : user specific API key needed to retrieve data from AgroDataCube (request here: https://agrodatacube.wur.nl/api/register.jsp)

# Function to get raster data geotiff for AHN (ahn_image) ----------------------

get_ahn_image_for_field <- function(fieldid,
                                    api_key) {
  
  base_url <- "https://agrodatacube.wur.nl/api/v2/rest/fields/"
  my_url <- paste0(base_url, fieldid,  "/ahn_image?output_epsg=4326")
  response <- httr::GET(my_url, add_headers(token = api_key))
  geojson <- httr::content(response, as = "text", encoding = "UTF-8")
  ahn_image <- geojsonio::geojson_sf(geojson)   
  
  return(ahn_image)
}

