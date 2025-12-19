# Get a list of all possible soil codes
# Created     : 27 November 2025
# Last update : 27 November 2025

# Load packages ----------------------------------------------------------------

library(httr)

# Arguments --------------------------------------------------------------------

# api_key   : user specific API key needed to retrieve data from AgroDataCube (request here: https://agrodatacube.wur.nl/api/register.jsp)

# Function to get all soil codes -----------------------------------------------

get_soil_codes <- function(api_key) {
  
  my_url <- "https://agrodatacube.wur.nl/api/v2/rest/codes/soilcodes?page_size=10000&page_offset=0"
  response <- httr::GET(my_url, add_headers(token = api_key))
  geojson <- httr::content(response, as = "text", encoding = "UTF-8")
  soil_codes <- geojsonio::geojson_sf(geojson)   
  
  return(soil_codes)
}





