# NatureDataCube Shiny interface (demo version for the NAEM)

This repository contains the R code for a Shiny interface of the NatureDataCube.
The idea of the NatureDataCube is that it offers an accessible way for researchers/ecologists to retrieve relevant data. 
It is a demo version and it will be used for the NAEM.
 The Shiny interface was made as an example of what the interface
of the NatureDataCube could look like and to gather feedback from researchers.



This interface is built upon the work from Minke Mulder (NIOO-KNAW) in October - December 2025.

### Opening the Shiny app

To open and use this code, go to ["R/"](https://github.com/LTER-LIFE/NatureDataCube-Shiny-NAEM/blob/main/R/app.R) open app.R in RStudio (or another R environment), and install/load the required packages. Then, in the R console, enter:
"data_nc <- runApp("app.R")" to launch the interface. If you want to retrieve data, you will also need to provide the API tokens.

### Generate an API token

To generate your free personal API token to retrieve data you can go to [Register API tokens]([https://developer.dataplatform.knmi.nl/apis](https://agrodatacube.wur.nl/api/register.jsp)).


### Description

The user can open a Shiny interface which lets them select an area (management area, research site or they cand draw their own area of interest)
to retrieve data for. Below is a description of all the files that are in this repository.

#### Shiny app

Folder: [R/](https://github.com/LTER-LIFE/NatureDataCube-Shiny-NAEM/tree/main/R)

This folder contains the R code for the Shiny interface. 

Files:

- `app.R`\
  The code for the app itself.

Subfolder: 
  and a sub folder [retrieval_functions(https://github.com/LTER-LIFE/NatureDataCube-Shiny-NAEM/tree/main/R/retrieval_functions)
  taken from [NatureDataCube-R](https://github.com/LTER-LIFE/NatureDataCube-R)

  
#### Data retrieval functions




#### Data

Folder: [data](https://github.com/LTER-LIFE/NatureDataCube-Shiny-NAEM/tree/main/data)

This folder contains the data of the available study areas. 


