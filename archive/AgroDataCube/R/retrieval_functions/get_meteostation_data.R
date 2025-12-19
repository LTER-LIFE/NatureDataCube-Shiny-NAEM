# Get data for given meteostation for a specific time period
# Created     : 27 November 2025
# Last update : 27 November 2025

# Load packages ----------------------------------------------------------------

library(httr)

# Arguments --------------------------------------------------------------------

# metostationid   : ID of a meteostation
# fromdate        : yyyymmdd
# todate          : yyyymmdd
# api_key         : user specific API key needed to retrieve data from AgroDataCube (request here: https://agrodatacube.wur.nl/api/register.jsp)

# Function to get data for given meteostation for specific time period ---------

get_meteostation_data <- function(meteostationid,
                                  fromdate,
                                  todate,
                                  api_key) {
  
  base_url <- "https://agrodatacube.wur.nl/api/v2/rest/meteodata?output_epsg=4326&meteostation="
  my_url <- paste0(base_url, meteostationid, "&fromdate=", fromdate, "&todate=", todate, "&page_size=10000&page_offset=0")
  response <- httr::GET(my_url, add_headers(token = api_key))
  geojson <- httr::content(response, as = "text", encoding = "UTF-8")
  meteostation_data <- geojsonio::geojson_sf(geojson)  
  
  return(meteostation_data)
}


