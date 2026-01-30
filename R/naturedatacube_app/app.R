# ============================================================
# R Shiny NatureDataCube interface demo version: NAEM
# ============================================================
# Created on   : 26 Jan  2026
# Last update  : 30 Jan 2026

#
# Description:
# This script is based on the previous version created by Minke Mulder
# and this Shiny application allows users to:
# - Select a predefined study area or draw a custom polygon
# - Choose one or more NatureDataCube datasets
# - Configure dataset-specific parameters (for now only year)
# - Retrieve spatial data via the NatureDataCube API
# - Download results as GeoJSON files in a ZIP file or use them in a R variable

# how to run it: 
# first load / install the packages 
# data_nc <- runApp("app.R") # if choosing to continue in r data will
# be stored in the variable data_nc


# Design rules for now:
# - ONE polygon (study area) per download / export 
# - Multiple datasets are allowed for that polygon
# - Geometry is sent to the API as WKT (EPSG:4326)

# ============================================================
# Libraries
# ============================================================
if(!"shiny" %in% installed.packages()){install.packages("shiny")}
library(shiny)

if(!"leaflet" %in% installed.packages()){install.packages("leaflet")}
library(leaflet)

if(!"leaflet.extras" %in% installed.packages()){install.packages("leaflet.extras")}
library(leaflet.extras)

if(!"sf" %in% installed.packages()){install.packages("sf")}
library(sf)

if(!"dplyr" %in% installed.packages()){install.packages("dplyr")}
library(dplyr)

if(!"purrr" %in% installed.packages()){install.packages("purrr")}
library(purrr)

if(!"stringr" %in% installed.packages()){install.packages("stringr")}
library(stringr)

if(!"httr" %in% installed.packages()){install.packages("httr")}
library(httr)

if(!"geojsonsf" %in% installed.packages()){install.packages("geojsonsf")}
library(geojsonsf)

if(!"jsonlite" %in% installed.packages()){install.packages("jsonlite")}
library(jsonlite)

if(!"zip" %in% installed.packages()){install.packages("zip")}
library(zip)

if(!"here" %in% installed.packages()){install.packages("here")}
library(here)
# ============================================================
# NatureDataCube API configuration
# ============================================================
# - ndc_url(): helper function to construct API URLs
# - mytoken  : token for authenticated requests
# - myheaders: HTTP headers used in all API calls
# ============================================================
source(here::here("R", "retrieval_functions", "ndc_url.R"))

mytoken <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3N1ZWR0byI6ImQubGljaHRlbmJlcmdAbmlvby5rbmF3Lm5sIiwicmVzb3VyY2UiOlsiKiJdLCJpYXQiOjE3NjgyMTM3OTZ9.nEmOwkBTzKBjlsZL8obY-kWvghNS4A1M1Vwv1B94SSU
"
myheaders <- c(
  "Accept" = "application/json;charset=utf-8",
  "token"  = mytoken
)

# ============================================================
# Load polygon data
# ============================================================
# Fixed polygons 
gpkg <- here::here("data", "study_sites.gpkg")
layers <- sf::st_layers(gpkg)$name

all_layers <- set_names(
  map(layers, ~ st_read(gpkg, layer = .x, quiet = TRUE)),
  layers
)

# Individual datasets
nutnet    <- all_layers[["nutnet_poly"]]
nestboxes <- all_layers[str_detect(names(all_layers), "^nestkasten_")] %>%
  reduce(rbind)
loobos <- all_layers[["loobos"]]
lights <- all_layers[["20251008_licht_op_natuur_lantaarnpalen"]]

# ============================================================
# add WKT column
# ============================================================
add_wkt_column <- function(sf_obj, col_name = "wkt") {
  sf_obj <- st_transform(sf_obj, 4326)
  sf_obj[[col_name]] <- st_as_text(st_geometry(sf_obj))
  sf_obj
}

nutnet_wkt    <- add_wkt_column(nutnet)
nestboxes_wkt <- add_wkt_column(nestboxes)
loobos_wkt    <- add_wkt_column(loobos)
lights_wkt    <- add_wkt_column(lights)

# ============================================================
# drawn polygon to sf
# ============================================================
# Converts Leaflet draw feature into an sf polygon
# and assigns a unique layer_id.
convert_drawn_to_sf <- function(feat, start_layer_id = 1) {
  if (is.null(feat) || feat$geometry$type != "Polygon") return(NULL)
  
  coords <- feat$geometry$coordinates[[1]]
  coords <- do.call(
    rbind,
    lapply(coords, function(x) as.numeric(unlist(x)))
  )
  
  st_as_sf(st_sfc(st_polygon(list(coords)), crs = 4326)) %>%
    mutate(wkt = st_as_text(st_geometry(.)), layer_id = start_layer_id)
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  # title
  titlePanel("Data Nature Cube"),
  
  # custom style 
  tags$head(
    tags$style(HTML("
      /* Buttons hover effect */
      .btn-custom {
        color: white !important;
        background-color: #28a745 !important;
        border: none !important;
        transition: all 0.2s ease-in-out;
      }
      .btn-custom:hover {
        background-color: #218838 !important;
        color: white !important;
      }
    "))
  ),
  
  # sidebar layout 
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "fixed_layer",
        "Select or draw project area:",
        choices = c(
          "NutNet",
          "Nestboxes",
          "Loobos",
          "Licht op natuur lantaarnpalen"
        )
      ),
      leafletOutput("map", height = "400px"),
      br(),
      #h4("WKT of selected/drawn area"),
      #verbatimTextOutput("selected_wkt"),
      #br(),
      h4("Choose dataset for the selected area"),
      radioButtons(
        "selected_dataset",
        "Available datasets:",
        choices = c(
          "Agricultural fields",
          "AHN",
          "Soil map"
        )
      )
    ),
    
    # main panel layout 
    mainPanel(
      h4("Overview of selected datasets"),
      uiOutput("overview_table"),
      actionButton("clear_overview", "Clear overview", style = "color: white; background-color: grey;"),
      br(),
      uiOutput("download_ui"),      
      br(),
      uiOutput("download_messages"),
      hr(),
      h4("Dataset metadata and selection"),
      uiOutput("dataset_metadata"),
      uiOutput("year_ui"),
      actionButton("add_dataset", "Add to overview")
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  # reactive variables 
  drawn_polygon <- reactiveVal(NULL)
  fixed_polys <- reactiveVal(NULL)
  
  # Overview tibble
  overview <- reactiveVal(
    tibble::tibble(
      dataset = character(),
      year = integer(),
      polygon = character(),
      wkt = character(),
      polygon_sf = list()
    )
  )
  
  download_msgs <- reactiveVal(character(0))
  
  # -----------------------
  # Map
  # -----------------------
  # map 
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 5.3, lat = 52.1, zoom = 7) %>%
      addDrawToolbar(
        targetGroup = "drawn",
        polygonOptions = drawPolygonOptions(showArea = TRUE, repeatMode = FALSE),
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, remove = TRUE)
      )
  })
  
  # Fixed polygon selection
  observeEvent(input$fixed_layer, {
    leafletProxy("map") %>% clearGroup("fixed") %>% clearGroup("highlight_fixed")
    poly <- switch(
      input$fixed_layer,
      "NutNet" = nutnet_wkt,
      "Nestboxes" = nestboxes_wkt,
      "Loobos" = loobos_wkt,
      "Licht op natuur lantaarnpalen" = lights_wkt
    ) %>% mutate(layer_id = row_number())
    fixed_polys(poly)
    leafletProxy("map") %>% addPolygons(data = poly, group = "fixed", color = "black", fillOpacity = 0.3, weight = 2, layerId = ~layer_id)
  })
  
  # Clicking a fixed polygon
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click$id)
    poly <- fixed_polys() %>% filter(layer_id == click$id)
    if (nrow(poly) > 0) {
      drawn_polygon(poly)
      leafletProxy("map") %>%
        clearGroup("highlight_fixed") %>%
        addPolygons(data = poly, group = "highlight_fixed", color = "#007bff", weight = 4, fillOpacity = 0.5)
    }
  })
  
  # Drawing a custom polygon
  observeEvent(input$map_draw_new_feature, {
    start_id <- if (is.null(fixed_polys())) 1 else max(fixed_polys()$layer_id, na.rm = TRUE) + 1
    poly_sf <- convert_drawn_to_sf(input$map_draw_new_feature, start_layer_id = start_id)
    req(poly_sf)
    drawn_polygon(poly_sf)
    leafletProxy("map") %>% clearGroup("highlight_drawn") %>%
      addPolygons(data = poly_sf, group = "highlight_drawn", color = "#007bff", weight = 4, fillOpacity = 0.5, layerId = ~layer_id)
  })
  
  observeEvent(input$map_draw_deleted_features, {
    drawn_polygon(NULL)
    leafletProxy("map") %>% clearGroup("highlight_drawn")
  })
  
  #output$selected_wkt <- renderPrint({ poly <- drawn_polygon(); if (is.null(poly)) "No polygon drawn" else poly$wkt[1] })
  
  # -----------------------
  # Metadata UI
  # -----------------------
  # metadata for each dataset 
  output$dataset_metadata <- renderUI({
    req(input$selected_dataset)
    switch(input$selected_dataset,
           "Agricultural fields" = HTML("Agricultural fields: Data about crops, yield, and fertilization."),
           "AHN" = HTML("AHN: Elevation data of the Netherlands."),
           "Soil map" = HTML("Soil map: Soil types / soil map intersections for the selected area.")
    )
  })
  
  # Dataset-specific options
  output$year_ui <- renderUI({
    req(input$selected_dataset)
    if (input$selected_dataset == "Agricultural fields") {
      numericInput("selected_year", "Select year:", value = 2025, min = 2000, max = 2025)
    } else if (input$selected_dataset == "AHN") {
      numericInput("selected_year", "Select AHN version (year):", value = 2025, min = 2010, max = as.numeric(format(Sys.Date(), "%Y")))
    } else {
      helpText("Soil map selection does not require a year.")
    }
  })
  
  # -----------------------
  # Add to overview 
  # -----------------------
  observeEvent(input$add_dataset, {
    poly <- drawn_polygon()
    if (is.null(poly)) { 
      showNotification("⚠ Please select or draw a polygon first.", type = "error", duration = 5)
      return(NULL) 
    }
    
    # soil doesn't need a year 
    req(input$selected_dataset)
    if (input$selected_dataset != "Soil map") req(input$selected_year)
    
    # Determine polygon name
    poly_name <- if (!is.null(poly$layer_id) && !is.null(fixed_polys())) {
      fixed_poly <- fixed_polys() %>% filter(layer_id == poly$layer_id)
      if (nrow(fixed_poly) > 0) input$fixed_layer else "Own polygon"
    } else {
      "Own polygon"
    }
    
    # -------------------------
    # Single polygon enforcement by WKT
    # -------------------------
    ov <- overview()
    if (nrow(ov) > 0) {
      existing_wkt <- ov$wkt[1]
      new_wkt <- poly$wkt[1]
      
      if (!identical(existing_wkt, new_wkt)) {
        showModal(modalDialog(
          title = "Only one study area allowed",
          paste0(
            "The overview already contains datasets for a different polygon.\n\n",
            "You can add multiple datasets but only for a single polygon at a time.\n\n",
            "To add datasets for this new polygon, please first clear the overview by clicking 'Clear overview'."
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
    }
    
    # soil map doesn't have a year 
    year_val <- if (input$selected_dataset == "Soil map") NA_integer_ else as.integer(input$selected_year)
    
    # Append to overview
    duplicate_check <- ov %>%
      filter(dataset == input$selected_dataset,
             (is.na(year) & is.na(year_val) | year == year_val),
             polygon == poly_name)
    # check for duplicates 
    if (nrow(duplicate_check) == 0) {
      new_row <- tibble::tibble(
        dataset = input$selected_dataset,
        year = year_val,
        polygon = poly_name,
        wkt = poly$wkt[1],
        polygon_sf = list(poly)
      )
      overview(bind_rows(ov, new_row))
    } else {
      showNotification("This dataset + polygon + year is already in the overview.", type = "message", duration = 3)
    }
  })
  
  # -----------------------
  # Overview table UI
  # -----------------------
  output$overview_table <- renderUI({
    ov <- overview()
    if (nrow(ov) == 0) return(p("No datasets added yet.", style = "color: grey; font-style: italic;"))
    # headers 
    tags$table(
      class = "table table-striped table-bordered",
      tags$thead(
        tags$tr(
          tags$th("Dataset"),
          tags$th("Year"),
          tags$th("Polygon"),
          tags$th("Delete")
        )
      ),
      # input (rows)
      tags$tbody(
        lapply(seq_len(nrow(ov)), function(i) {
          row <- ov[i, ]
          tags$tr(
            tags$td(row$dataset),
            tags$td(ifelse(is.na(row$year), "", as.character(row$year))),
            tags$td(row$polygon),
            tags$td(
              actionButton(
                inputId = paste0("delete_row_", i),
                label = "x",
                style = "color: white; background-color: #CD5C5C; padding:2px 5px; font-size:80%;"
              )
            )
          )
        })
      )
    )
  })
  
  # -----------------------
  # Delete buttons
  # -----------------------
  observe({
    ov <- overview()
    if (nrow(ov) == 0) return(NULL)
    lapply(seq_len(nrow(ov)), function(i) {
      idx <- i
      observeEvent(input[[paste0("delete_row_", idx)]], {
        cur <- overview()
        if (nrow(cur) >= idx) {
          overview(cur[-idx, , drop = FALSE])
          download_msgs(character(0))
        }
      }, ignoreInit = TRUE)
    })
  })
  
  # -----------------------
  # Clear overview
  # -----------------------
  observeEvent(input$clear_overview, {
    overview(tibble::tibble(dataset = character(),
                            year = integer(),
                            polygon = character(),
                            wkt = character(),
                            polygon_sf = list()))
    download_msgs(character(0))
  })
  
  # -----------------------
  # Retrieve (call functions) & save
  # -----------------------
  retrieve_and_save <- function(zipfile = NULL, workdir = NULL) {
    ov <- overview()
    req(nrow(ov) > 0)
    tmpdir <- tempdir()
    if (is.null(workdir)) workdir <- file.path(tmpdir, "export")
    if (dir.exists(workdir)) unlink(workdir, recursive = TRUE)
    dir.create(workdir, recursive = TRUE)
    download_msgs(character(0))
    local_msgs <- character(0)
    results <- list()
    
    withProgress(message = "Retrieving datasets...", value = 0, {
      for (i in seq_len(nrow(ov))) {
        ds <- ov$dataset[i]
        year <- ov$year[i]
        outfile <- file.path(workdir, paste0(ds, "_", i, ".geojson"))
        
        tryCatch({
          poly_sf <- ov$polygon_sf[[i]]
          mypolygon <- poly_sf$wkt[1]
          
          # urls per dataset
          myurl <- switch(ds,
                          "Agricultural fields" = ndc_url("Fields", params = c(geometry=mypolygon, epsg="4326", year=year, cropname="mais", output_epsg="4326")),
                          "AHN" = ndc_url("AHN", params = c(geometry=mypolygon, epsg="4326")),
                          "Soil map" = ndc_url("Soiltypes", params = c(geometry=mypolygon, epsg="4326", output_epsg="4326", page_size="25", page_offset="0"))
          )
          
          myres <- content(VERB("GET", url=myurl, add_headers(myheaders)))
          myres_sf <- geojson_sf(toJSON(myres, auto_unbox=TRUE))
          names(myres_sf) <- gsub("[:/\\?<>\\|*\"\\\\]", "_", names(myres_sf))
          st_write(myres_sf, outfile, delete_dsn = TRUE)
          results[[paste0(ds, "_", i)]] <- myres_sf
          local_msgs <- c(local_msgs, paste0("Retrieved: ", ds))
        }, error=function(e){
          local_msgs <- c(local_msgs, paste0("Failed: ", ds, " - ", e$message))
        })
        
        incProgress(1/nrow(ov), detail=ds)
      }
    })
    
    files_created <- list.files(workdir, full.names=TRUE)
    created_zip <- NULL
    if (!is.null(zipfile) && length(files_created)>0) {
      parent <- dirname(zipfile)
      if (!dir.exists(parent)) dir.create(parent, recursive=TRUE)
      oldwd <- setwd(workdir)
      on.exit(setwd(oldwd), add=TRUE)
      zip::zipr(zipfile, files_created)
      created_zip <- normalizePath(zipfile)
      local_msgs <- c(local_msgs, paste0("Zip written to: ", created_zip))
    } else if(length(files_created)==0) local_msgs <- c(local_msgs,"No files were created.")
    
    download_msgs(c(download_msgs(), local_msgs))
    
    list(
      overview = ov,
      messages = download_msgs(),
      zipfile = created_zip,
      datasets = results
    )
  }
  
  # -----------------------
  # Download & Return UI
  # -----------------------
  output$download_ui <- renderUI({
    if(nrow(overview())==0) {
      div(style="color: grey; font-style: italic;", "Download button will appear here once you add a dataset.")
    } else {
      div(style="display:flex; gap:10px; align-items:center;",
          downloadButton("download_data", "Download dataset(s)", class="btn-custom"),
          actionButton("return_to_r", "Return data to R (close app)", class="btn-custom")
      )
    }
  })
  
  output$download_data <- downloadHandler(
    filename=function() paste0("naturedatacube_", Sys.Date(), ".zip"),
    content=function(zipfile) { retrieve_and_save(zipfile=zipfile) }
  )
  
  output$download_messages <- renderUI({
    msgs <- download_msgs()
    if(length(msgs)==0) p("Updates on dataset retrieval will appear here.", style="color: grey; font-style: italic;")
    else HTML(paste0(msgs, collapse="<br>"))
  })
  
  # -----------------------
  # Return to R button
  # -----------------------
  observeEvent(input$return_to_r, {
    req(nrow(overview())>0)
    showNotification("Preparing data and closing app...", type="message")
    res <- retrieve_and_save(zipfile=NULL)
    stopApp(res)
  })
}

# ============================================================
# Run app
# ============================================================
shinyApp(ui, server)

