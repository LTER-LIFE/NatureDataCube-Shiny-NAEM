# Get NDVI for a specific field and time period
# Created     : 27 November 2025
# Last update : 27 November 2025

# Load packages ----------------------------------------------------------------

library(httr)

# Arguments --------------------------------------------------------------------

# fieldid   : ID of a field in the Netherlands
# fromdate  : yyyymmdd
# todate    : yyyymmdd
# api_key   : user specific API key needed to retrieve data from AgroDataCube (request here: https://agrodatacube.wur.nl/api/register.jsp)

# Function to get NDVI for specific field and time period ----------------------

get_ndvi_for_field <- function(fieldid,
                               fromdate,
                               todate,
                               api_key) {
  
  base_url <- "https://agrodatacube.wur.nl/api/v2/rest/fields/"
  my_url <- paste0(base_url, fieldid,  "/ndvi?output_epsg=4326", "&fromdate=", fromdate, "&todate=", todate, "&page_size=10000&page_offset=0")
  response <- httr::GET(my_url, add_headers(token = api_key))
  geojson <- httr::content(response, as = "text", encoding = "UTF-8")
  ndvi <- geojsonio::geojson_sf(geojson)   
  
  return(ndvi)
}


