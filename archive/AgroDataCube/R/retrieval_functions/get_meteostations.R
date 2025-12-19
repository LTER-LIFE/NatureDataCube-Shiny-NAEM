# Get list of meteostations for which data is available
# Created     : 27 November 2025
# Last update : 27 November 2025

# Load packages ----------------------------------------------------------------

library(httr)

# Arguments --------------------------------------------------------------------

# api_key   : user specific API key needed to retrieve data from AgroDataCube (request here: https://agrodatacube.wur.nl/api/register.jsp)

# Function to get meteostations with data available ----------------------------

get_meteostations <- function(api_key) {
  
  my_url <- "https://agrodatacube.wur.nl/api/v2/rest/meteostations?output_epsg=4326&page_size=10000&page_offset=0"
  response <- httr::GET(my_url, add_headers(token = api_key))
  geojson <- httr::content(response, as = "text", encoding = "UTF-8")
  meteostations <- geojsonio::geojson_sf(geojson)  

  return(meteostations)
}
