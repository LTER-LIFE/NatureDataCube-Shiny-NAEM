# AgroDataCube Shiny interface

This repository contains the R code for a Shiny interface to interact with the 
[AgroDataCube](https://agrodatacube.wur.nl/), which was made as a test case before 
working on the interface of the NatureDataCube and therefor is in the "archive" of the NatureDataCube repository.

This was made by Minke Mulder (NIOO-KNAW) in October - December 2025.

### Opening the Shiny app

To open and use this code, go to [R/agrodatacube_app](https://github.com/LTER-LIFE/NatureDataCube/tree/main/archive/AgroDataCube/R/agrodatacube_app) 
and open `app.R`. Fill in the API key and click "Run app" in the top right (or Ctrl+Shift+Enter).

### Generate an API token for AgroDataCube
To generate your free personal API token for the AgroDatacube you can go to [Register API key](https://agrodatacube.wur.nl/api/register.jsp).

### Description

The user can open a Shiny interface which lets them select a parcel to retrieve data for. Data like: weather variables, 
soil type, crops of certain years, NDVI etc. Below is a description of all the files that are in this repository.


#### Shiny app

Folder: [R/agrodatacube_app](https://github.com/LTER-LIFE/NatureDataCube/tree/main/archive/AgroDataCube/R/agrodatacube_app) 

This folder contains the R code for the Shiny interface.

Files:

- `app.R`\
  The code for the app itself. Open the R file, fill in the AgroDataCube API key and click "Run app" in the top right (or Ctrl+Shift+Enter).

#### Data retrieval functions

Folder: [R/retrieval_functions](https://github.com/LTER-LIFE/NatureDataCube/tree/main/archive/AgroDataCube/R/retrieval_functions)

This folder contains retrieval functions for data that is in the AgroDataCube at this moment. Which are the [AHN](https://www.ahn.nl/),
weather data from the KNMI, NDVI and soil info.

Files:  

- `get_ahn_for_field.R`
- `get_ahn_image_for_field.R`
- `get_meteostation_data.R`
- `get_ndvi_for_field.R`
- `get_ndvi_image_for_field.R`
- `get_nearest_meteostations_for_field.R`
- `get_soil_code_details.R`
- `get_soil_codes.R`
- `get_soil_params_for_field.R`
- `get_soil_types_for_field.R`  


#### Data

Folder: [data](https://github.com/LTER-LIFE/NatureDataCube/tree/main/archive/AgroDataCube/data)

This folder contains the data that is needed for offering the options of the available datasets and the polygons the user can select.

Files: 

- `dataset_info_agrodatacube.csv`\
  This CSV file contains info for the app for all the data that is available in the AgroDataCube.
  **Note:** this is very basic data to test if the interface would work. The dates for
  available data are mock dates and are not the actual dates for which data is available.
  
  
Folder: [data/polygons](https://github.com/LTER-LIFE/NatureDataCube/tree/main/archive/AgroDataCube/data/polygons)

This folder contains the shapefile with a subset of the parcels on the Veluwe, only southwest of the Veluwe.
These are from the ["Basisregistratie Gewaspercelen"](https://www.pdok.nl/introductie/-/article/basisregistratie-gewaspercelen-brp-).
The user can select a parcel in the app to retrieve data for that parcel.

Files: 

- `brp_subset_veluwe.cpg`
- `brp_subset_veluwe.dbf`
- `brp_subset_veluwe.prj`
- `brp_subset_veluwe.shp`
- `brp_subset_veluwe.shx`

  




