# Get details for a specific soil code
# Created     : 27 November 2025
# Last update : 27 November 2025

# Load packages ----------------------------------------------------------------

library(httr)

# Arguments --------------------------------------------------------------------

# soilcode  : soilcode to get the details of
# api_key   : user specific API key needed to retrieve data from AgroDataCube (request here: https://agrodatacube.wur.nl/api/register.jsp)

# Function to get details for specific soil code -------------------------------

get_soil_code_details <- function(soilcode,
                                  api_key) {
  
  base_url <- "https://agrodatacube.wur.nl/api/v2/rest/codes/soilcodes/"
  my_url <- paste0(base_url, soilcode, "?page_size=10000&page_offset=0")
  response <- httr::GET(my_url, add_headers(token = api_key))
  geojson <- httr::content(response, as = "text", encoding = "UTF-8")
  soil_code_details <- geojsonio::geojson_sf(geojson)   
  
  return(soil_code_details)
}

