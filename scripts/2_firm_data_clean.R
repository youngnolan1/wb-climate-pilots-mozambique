#-------------------------------------------------------------------------------
# Script: firm_data_clean.R
# Author: Nolan Young Zabala
# Description: - set up Mozambique grids
#              - assign firms to grids
#              - basic calculations and data preparation for firm maps (location,
#                size, sector)
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------
  
library(haven)
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)

# Set working directory
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-mozambique")


#-------------------------------- 2. LOAD DATA ---------------------------------

# Read Moz18 Firm + Coordinate Data
firms <- read_dta("intermediate_data/corresptab_coord_firms_Moz18.dta")

# Read Mozambique shapefile, select polygon data, and add grid_id
country_and_subdivisions <- read_sf("raw_data/shapefiles/gadm41_MOZ_2.shp") %>%    
  select(geometry) %>% 
  mutate(grid_id = row_number())

# Convert firm dataframe to sf object
sf_firms <- st_as_sf(firms, 
                     coords = c("lon_coord", "lat_coord"), 
                     crs = st_crs(country_and_subdivisions)) 

# Assign firms to grids
sf_joined <- st_join(country_and_subdivisions, 
                     sf_firms,
                     left = TRUE)

# Save sf_joined to shapefile
#st_write(sf_joined, "intermediate_data/sf_joined.shp")


#------------------------------ 3. PLOT FUNCTION -------------------------------

# General function which creates a heatmap for the "HeatVar" of the given "df"
heatplot <- function(df, plot_title, legend_title){   
  ggplot(df) +     
    geom_sf(aes(fill = HeatVar), size = 0.2) +     
    scale_fill_gradient(low = "white", high = "red") +     
    theme_minimal() +     
    labs(title = plot_title, fill = legend_title) 
}

# Set var to 1 to count
sf_joined$idstd[!is.na(sf_joined$idstd)] <- 1

# Group by grid and count
sf_firm_count <- sf_joined %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = sum(idstd))


#-------------------------------- 5. FIRM SIZE ---------------------------------

# 5a. Average "size" (1-3 classification)
  
# Group by grid and find average
sf_avgsize <- sf_joined %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = mean(size))


# 5b. Average sales in 2017 (winsorized)
  
# Group by grid and find average
sf_avgsales <- sf_joined %>% 
  group_by(geometry) %>% 
  summarize(HeatVar = mean(d2_win))


#-------------------------------- 6. SECTORS ---------------------------------

# 6a. Firm counts per sector

df_joined <- st_drop_geometry(sf_joined)

df_joined$idstd[!is.na(df_joined$idstd)] <- 1
 
sector_count <- function(sector){
  sector_sum <- df_joined %>% 
    filter(stra_sector == sector) %>% 
    group_by(grid_id) %>% 
    mutate(HeatVar = sum(idstd)) %>% 
    select(grid_id, HeatVar) %>% 
    distinct()
  
  sector_result <- left_join(country_and_subdivisions, sector_sum, by = "grid_id")

  return(sector_result)
}



