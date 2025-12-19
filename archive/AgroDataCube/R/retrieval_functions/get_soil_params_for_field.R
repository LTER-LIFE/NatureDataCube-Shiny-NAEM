# Get spatial intersections of a field with the soil physical parameters geometries (BOFEK 2012)
# Created     : 27 November 2025
# Last update : 27 November 2025

# Load packages ----------------------------------------------------------------

library(httr)

# Arguments --------------------------------------------------------------------

# fieldid   : ID of a field in the Netherlands
# api_key   : user specific API key needed to retrieve data from AgroDataCube (request here: https://agrodatacube.wur.nl/api/register.jsp)

# Function to get soil physical parameters (BOFEK 2012) ------------------------

get_soil_params_for_field <- function(fieldid,
                                      api_key) {
  
  base_url <- "https://agrodatacube.wur.nl/api/v2/rest/fields/"
  my_url <- paste0(base_url, fieldid, "/soilparams?output_epsg=4326&page_size=10000&page_offset=0")
  response <- httr::GET(my_url, add_headers(token = api_key))
  geojson <- httr::content(response, as = "text", encoding = "UTF-8")
  soil_params <- geojsonio::geojson_sf(geojson)   
  
  return(soil_params)
}


