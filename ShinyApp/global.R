
# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(scales)
library(shinyWidgets)
library(leaflet)
library(sf)
#library(rnaturalearth)
#library(rnaturalearthdata)
library(maps)

# Read the healthcare data
healthcare_data <- read.csv("healthcare_data.csv", stringsAsFactors = FALSE)

#world_shape <- ne_countries(scale = "medium", returnclass = "sf")
#world_shape <- world_shape %>% mutate(name= case_when(name == "United States of America" ~ "United States", TRUE ~ name))



world_shape <- st_as_sf(map("world", plot = FALSE, fill = TRUE))
world_shape <- world_shape %>%
  mutate(name = gsub(":.*$", "", ID))
world_shape$name[world_shape$name == "USA"] <- "United States"


# Define color palette para countries
country_colors <- c(
  "United States" = "#1f77b4",
  "Canada" = "#ff7f0e",
  "Germany" = "#2ca02c",
  "United Kingdom" = "#d62728",
  "France" = "#9467bd",
  "Japan" = "#8c564b",
  "Australia" = "#e377c2",
  "Italy" = "#7f7f7f",
  "Spain" = "#bcbd22",
  "Mexico" = "#17becf"
)

# Create formatted variable names for display
var_labels <- c(
  beds = "Hospital Beds (per 1,000)",
  physicians = "Physicians (per 1,000)",
  population = "Population",
  health_spend = "Health Spending (per capita USD)",
  mortality = "Mortality Rate (per 1,000)"
)
