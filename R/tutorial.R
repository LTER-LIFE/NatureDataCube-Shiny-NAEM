# ============================================================
# Tutorial: Linking bird nest data with weather data
# ============================================================
# This tutorial shows how to:
# 1. Load nest data
# 2. Open a Shiny app to access weather data
# 3. Clean and filter the data
# 4. Merge nest data with temperature data
# 5. Create a plot showing lay dates vs temperature
#
# Run this script step by step using Ctrl + Enter
# ============================================================


# ============================================================
# 1. Load required libraries
# ============================================================
# These packages help us work with data, dates, and plots

library(tidyverse)   # data manipulation + plotting
library(lubridate)   # working with dates
library(dplyr)       # data filtering and joining


# ============================================================
# 2. Load the nest data
# ============================================================
# This file contains information on bird nests such as:
# - lay_date (date eggs were laid)
# - species
# - clutch_size (number of eggs)
# - year

data <- read.csv("data/first_nests_HV.csv")


# ============================================================
# 3. Open the Shiny app
# ============================================================
This will open a Shiny app in a new window.
The app allows you to explore and extract all types of data 
but for now we focus on weather data.
Once the app is closed or finished, the data will be
available in the object called `temp`.

step 1. Run temp 

temp <- runApp("R/naturedatacube_app/app.R")

step 2. Select project Nestboxes and zoom in the map towards Arnhem and select the polygon
north of the A12 (under"De Hoge Veluwe")
step 3. select weather in available datasets 
step 4. select period and choose for this excersize from 2024-01-01 until 2025-01-01
step 5. select add to overview and Return data to R (close app)
data is now stored in the variable temp, ready for the next steps 


# ============================================================
# 4. Ensure lay_date is treated as a Date
# ============================================================
R sometimes reads dates as text.
We convert lay_date to a proper Date format.

data$lay_date <- as.Date(data$lay_date)


# ============================================================
# 5. Filter the nest data
# ============================================================
 We keep only:
 - data from 2024 onwards
 - nests with at least one egg
- nests from March to July (breeding season)

data <- data %>%
  filter(
    year >= 2024,
    clutch_size > 0,
    month(lay_date) %in% 3:7
  )


# ============================================================
# 6. Extract weather data from the Shiny app output
# ============================================================
The Shiny app stores datasets inside `temp`.
Here we extract daily mean temperature data
and rename the columns for clarity.

weather <- temp$datasets$Weather_1 %>%
  select(
    date = datum,
    temp = mean_temperature
  )


# ============================================================
# 7. Ensure weather dates are also in Date format
# ============================================================

weather$date <- as.Date(weather$date)


# ============================================================
# 8. Merge nest data with temperature data
# ============================================================
We join the two datasets so that each nest record
gets the temperature from the corresponding lay date.

plot_data <- data %>%
  left_join(weather, by = c("lay_date" = "date"))


# ============================================================
# 9. Keep only nests from 2024
# ============================================================
# This ensures the plot only shows data from one year.

plot_data <- plot_data %>%
  filter(year(lay_date) == 2024)


# ============================================================
# 10. Create the plot
# ============================================================
 This plot shows:
  - Lay date on the x-axis
 - Mean daily temperature on the y-axis
 - Point size represents clutch size
 - Point colour represents species

ggplot(plot_data, aes(x = lay_date, y = temp)) +
  geom_point(
    aes(size = clutch_size, colour = species),
    alpha = 0.4
  ) +
  scale_size_continuous(
    name = "Clutch size",
    range = c(1, 15)
  ) +
  labs(
    x = "Lay date",
    y = "Mean daily temperature (°C)",
    colour = "Species"
  ) +
  theme_minimal()
