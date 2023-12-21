#-------------------------------------------------------------------------------
# Script: climate_data_clean.R
# Author: Nolan Young Zabala
# Description: - format climate data in usable way
#              - compute metrics like old averages/changes/etc
#              - output data for use in dashboard
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

library(dplyr)
library(sf)
library(matrixStats)
library(leaflet)
library(ggplot2)
library(gridExtra)

# Set working directory
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-mozambique/intermediate_data")


#-------------------------------- 2. LOAD DATA ---------------------------------

# Read sf_joined
sf_joined <- read_sf("sf_joined.shp")

# Read climate data
climate <- read.csv("?")

# Get rid of vars I don't need
not_needed <- c("X", "a2x", "a3ax", "size", "a7", "b6", "stra_sector", "b4", "b4a")

climate <- climate %>% 
  select(-all_of(not_needed))

# Replace NAs
climate[climate == -99] <- NA


#------------------------ 3. PRE/CURRENT/ENTIRE PERIOD -------------------------

# Pre-period
pre_period <- 2000:2010

# Current period
current_period <- 2011:2021

# Entire period
entire_period <- 2000:2021


#---------------------------- 4. GENERAL FUNCTIONS -----------------------------


# METHOD: take yearly total e.g. hot days; find average of 2000-10; find average of
# 2011-21; compute change


# Function which subsets for given climate var
climate_var_subset <- function(climate_var){
  
  relevant_cols <- c("idstd", grep(climate_var, names(climate), value = TRUE))
  
  climate_var_df <- climate[, relevant_cols, drop = FALSE]
  
  return(climate_var_df)
}


# Function which computes yearly sum for given year
calculate_yearly_total <- function(climate_var_df, year) {

  # Subset to relevant year
  relevant_cols <- c("idstd", grep(year, names(climate_var_df), value = TRUE))
  varyear_df <- climate_var_df[, relevant_cols, drop = FALSE]
  
  year <- as.character(year)
  
  # Sum all columns except idstd
  result <- varyear_df %>%
    mutate(!!year := rowSums(select(., -idstd))) %>% 
    select(idstd, !!year)
    
  return(result)
}

# Function which identifies yearly max for given year
calculate_yearly_max <- function(climate_var_df, year) {
  
  # Subset to relevant year
  relevant_cols <- c("idstd", grep(year, names(climate_var_df), value = TRUE))
  varyear_df <- climate_var_df[, relevant_cols, drop = FALSE]
  
  year <- as.character(year)
  
  # Find max of all columns except idstd
  matrix_data <- varyear_df[,-1]
  row_max_values <- rowMaxs(as.matrix(matrix_data))
  
  # Add max to df
  varyear_df[[year]] <- row_max_values
  
  # Drop the monthly columns
  result <- varyear_df %>% 
    select(idstd, !!year)
  
  return(result)
}


# Period means function
period_means <- function(df){
  
  result <- df %>%
    mutate(pre_mean = rowMeans(select(., as.character(pre_period)), na.rm = TRUE),
           current_mean = rowMeans(select(., as.character(current_period)), na.rm = TRUE)) %>%
    select(idstd, pre_mean, current_mean)  
}


# Calculate percent change
percent_change <- function(df){
  result <- df %>%
    mutate(percent_change = ((current_mean - pre_mean) / pre_mean) * 100)
  
  return(result)
} 

# Calculate difference
difference <- function(df){
  result <- df %>% 
    mutate(difference = current_mean - pre_mean)
}


# Putting it all together - yearly totals or max, avg by decade, % change
yearlymetric_decadeavg_pcntchange <- function(variable, metric){
  
  # Subset to only climate var
  var_df <- climate_var_subset(variable)
  
  
  # Calculate yearly totals for each year
  wrapper <- function(year) {
    metric(climate_var_df = var_df, year)
  }
  
  var_totals <- lapply(entire_period, wrapper)
  
  var_totals <- Reduce(function(x, y) merge(x, y, by = "idstd", all = TRUE), var_totals)
  
  
  # Calculate period means and percent change
  var_means <- period_means(var_totals)
  
  var_means <- percent_change(var_means)
  
  
  # Merge with sf_joined
  sf_var <- left_join(sf_joined, var_means, by = "idstd")
  
  # Find average percent change per grid
  sf_var <- sf_var %>% 
    group_by(geometry) %>% 
    summarize(HeatVar = mean(percent_change))
}


# Climate-var-specific heatplot function (deals with negative values)
heatplot_climate <- function(df, plot_title, legend_title){
  ggplot(df) +     
    geom_sf(aes(fill = HeatVar), size = 0.2) +     
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +     
    labs(title = plot_title, fill = legend_title) 
}


#------------------------------- 5. TEMPERATURE --------------------------------

#Hotdays
sf_hotdays <- yearlymetric_decadeavg_pcntchange("hotdays", calculate_yearly_total)

#spei
sf_spei <- yearlymetric_decadeavg_pcntchange("spei", calculate_yearly_total)


#-------------------------------- 6. RAINFALL ----------------------------------

#Cwd
sf_cwd <- yearlymetric_decadeavg_pcntchange("cwd", calculate_yearly_max)

#Precip - yearly total
sf_precip <- yearlymetric_decadeavg_pcntchange("precip", calculate_yearly_total)

#sf_precip <- yearly_extreme_decade_pcntchange_compute(WHICH VAR?)


#------------------------------- 7. DROUGHT ------------------------------------

#Drydays
sf_drydays <- yearlymetric_decadeavg_pcntchange("drydays", calculate_yearly_total)

#Cdd
sf_cdd <- yearlymetric_decadeavg_pcntchange("cdd", calculate_yearly_max)

