# Get list of nearest meteostations (max. 5) to a field, ranked by distance
# Created     : 27 November 2025
# Last update : 27 November 2025

# Load packages ----------------------------------------------------------------

library(httr)

# Arguments --------------------------------------------------------------------

# fieldid   : ID of a field in the Netherlands
# api_key   : user specific API key needed to retrieve data from AgroDataCube (request here: https://agrodatacube.wur.nl/api/register.jsp)

# Function to get nearest meteostations ----------------------------------------

get_nearest_meteostations_for_field <- function(fieldid, 
                                                api_key) {
  
  base_url <- "https://agrodatacube.wur.nl/api/v2/rest/fields/"
  my_url <- paste0(base_url, fieldid, "/meteostations?output_epsg=4326&page_size=50&page_offset=0")
  response <- httr::GET(my_url, add_headers(token = api_key))
  geojson <- httr::content(response, as = "text", encoding = "UTF-8")
  nearest_meteostations <- geojsonio::geojson_sf(geojson)  
  
  return(nearest_meteostations)
}

