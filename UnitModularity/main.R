# Author: Nilanjan Roy

# Climate Data Analysis Script

# Description:
# This script analyzes climate data (temperature and precipitation) from weather stations across the USA.
# It focuses on examining changes over time by computing the slopes of climate variables against the year.


#*** Setup ***
# Clear the workspace and close all graphics devices to ensure a clean environment.
# MODULARITY EXAMPLE - Throughout the entire script, before writing the actual code, here I am writing a description of each chunck, describing what
# the next couple of lines of code will do. This will separate each code chunck from each other while maintaing the 
# task and description of each chuck

###MODULARITY EXAMPLE - starting of clean R enviroment to avoid dependency of other script.
# By clearing the environment, we avoid any unintended interactions with variables or settings from previous sessions.
# Loading all necessary libraries at the beginning ensures that functions used later are available.
rm(list = ls())
graphics.off()

# MODULARITY EXAMPLE - Writing extensible code.
# The package installation loop can easily be extended to include more packages if needed.
# Install required packages if they are not already installed
required_packages <- c("maps", "data.table", "tidyverse")
installed_packages <- installed.packages()
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

library(maps)       # For plotting geographical maps
library(data.table) # For efficient data manipulation and performance optimization
library(tidyverse) # For basic plotting

#*** Constants and Parameters ***

# Define constants and parameters that will be used throughout the script.
# MODULARITY EXAMPLE - giving constants variable names.
# Defining constants at the beginning allows for easy modification and improves code readability.
MIN_DATA_POINTS <- 40  # Minimum number of data points required per location for analysis

# MODULARITY EXAMPLE - Lesson 2: Write extensible code.
# If we need to change the minimum data points requirement, we can do so in one place.

#*** Load the Data ***

# Load the temperature and precipitation datasets from RDS files.
# MODULARITY EXAMPLE - Being aware of dependencies in R chunks. As this script performs a certain analysis
# it is pivotal that we define where the working files are and where the outputs will be stored.
# Ensuring data files are correctly loaded before proceeding avoids runtime errors.
# MODULARITY EXAMPLE - Resisting the temptation to just code without planning.
# Planning the data paths and verifying their correctness is crucial.

setwd("/Volumes/Crucial_X6/BIOL701_ML/RoyMLbiol/UnitModularity")

# MODULARITY EXAMPLE -  Writing extensible code.
# Defining data file paths as variables for flexibility.
temperature_data_file <- "USAAnnualTemp1950_2008.rds"
precipitation_data_file <- "USAAnnualPcpn1950_2008.rds"

# MODULARITY EXAMPLE - Checking where the files are present before proceding to data cleaning and analysis.
# MODULARITY EXAMPLE -The file paths are defined as variables to avoid hardcoding and facilitate changes.

# Check if data files exist before loading
if (!file.exists(temperature_data_file)) {
  stop("Temperature data file not found.")
}
if (!file.exists(precipitation_data_file)) {
  stop("Precipitation data file not found.")
}

# MODULARITY EXAMPLE - Loading the data using the varialbe name for the datasets that was assigned earler.

temperature_data <- readRDS(temperature_data_file)
precipitation_data <- readRDS(precipitation_data_file)

#*** Data Cleaning ***

# Define a function to filter locations with sufficient data points.
# MODULARITY EXAMPLE - Here I am defining a function modules and interfaces precisely in advance.
# The function 'filter_locations' has a clear purpose, inputs, and outputs.
# The function takes a dataframe that has 'state', 'name', 'lat', 'lon' columns
# The function take a min_points input to filter the dataset where atleast 40 measurements are there in a particular location

# MODULARITY EXAMPLE - This function prevents duplication of code for filtering different datasets.
# if a dataset has the state', 'name', 'lat', 'lon' columns then this function can be used to filter in this way.

filter_locations <- function(data, min_points) {
  # MODULARITY EXAMPLE - Writing pseudocode.
  # Steps:
  # 1. Aggregate the count of valid data points per location.
  # 2. Identify valid locations with counts >= min_points.
  # 3. Merge the original data with valid locations to filter.
  
  # Aggregate the number of non-missing data points for each location.
  data_counts <- aggregate(
    x = data$data,
    by = data[c('state', 'name', 'lat', 'lon')],
    FUN = function(x) sum(!is.na(x))  # Count of valid data points
  )
  
  # Rename the last column to 'count' for clarity.
  names(data_counts)[ncol(data_counts)] <- 'count'
  
  # Select locations that meet the minimum data points requirement.
  valid_locations <- data_counts[
    data_counts$count >= min_points,
    c('state', 'name', 'lat', 'lon')
  ]
  
  # Merge the original data with the valid locations to filter out insufficient data.
  filtered_data <- merge(
    data,
    valid_locations,
    by = c('state', 'name', 'lat', 'lon')
  )
  
  # Reset row names for cleanliness.
  rownames(filtered_data) <- NULL
  
  # Return the filtered dataset.
  return(filtered_data)
}

# Apply the function to both temperature and precipitation datasets.
# MODULARITY EXAMPLE - Instead coping a chunk of code here I used the filter_locations function
# for filtering both the temperature and precipitation data.
# MODULARITY EXAMPLE - this also shows extensible code feature.
# The same function can be used to filter any dataset with the required structure that has the following 
# 'state', 'name', 'lat', 'lon' columns.
temperature_filtered <- filter_locations(temperature_data, MIN_DATA_POINTS)
precipitation_filtered <- filter_locations(precipitation_data, MIN_DATA_POINTS)


#*** Regression Analysis ***

# Define a function to compute the slope of data over time for each location.
# MODULARITY EXAMPLE - Rewriting/revising your code can be good and important. Here, first, i wrote
# a loop based code to set the slopes. But it was taking too much time. So,
# The original loop-based approach was replaced with a more efficient method using data.table.
# MODULARITY EXAMPLE - this also shows extensible code.
# The function is designed to handle any dataset with the required structure that has 
# column names state, name, lat, lon.

compute_slopes_dt <- function(data) {
  # MODULARITY EXAMPLE - Writing pseudocode.
  # Steps:
  # 1. Convert data to a data.table object.
  # 2. Remove rows with missing 'data' or 'year' values.
  # 3. Group data by location and compute the slope.
  
  # MODULARITY EXAMPLE - Lesson 4: Define your modules and interfaces precisely in advance.
  # Inputs and outputs are clearly specified.
  
  # Convert the data frame to a data.table for efficient processing.
  data_dt <- as.data.table(data)
  
  # Remove rows with missing 'data' or 'year' to prevent errors in regression.
  data_dt <- data_dt[!is.na(data_dt$data) & !is.na(data_dt$year)]
  
  # Compute the slope of 'data' over 'year' for each location.
  slopes_dt <- data_dt[
    , .(
      slope = if (.N >= 2) {
        coef(lm(data ~ year))[2]  # Extract the slope coefficient
      } else {
        NA_real_  # Assign NA if insufficient data points
      }
    ),
    by = .(state, name, lat, lon)  # Group by location
  ]
  
  # Return the data.table containing slopes for each location.
  return(slopes_dt)
}

# Compute slopes for temperature and precipitation datasets.
temperature_slopes <- compute_slopes_dt(temperature_filtered)
precipitation_slopes <- compute_slopes_dt(precipitation_filtered)

#*** Histograms and Basic Statistics ***

# Define a function to plot histograms of the slopes and compute basic statistics.
# MODULARITY EXAMPLE - Instead coping a chunk of code here I used the plot_histogram function
# for plotting both the temperature and precipitation data..
# This function prevents code duplication for plotting histograms of different variables.

# MODULARITY EXAMPLE - Defining the plot_histogram function modules precisely in advance.
# Inputs:
# - slopes_data: Data frame containing slope values.
# - variable_name: String for labeling.
# - filename: String specifying the output PDF file name.

plot_histogram <- function(slopes_data, variable_name, filename) {
  
  # Open a PDF device to save the histogram.
  pdf(file = filename)
  
  # Plot the histogram of slope values.
  hist(
    slopes_data$slope,
    breaks = 50,
    main = paste(variable_name, "Slopes"),
    xlab = paste(variable_name, "Slope"),
    col = "lightblue"
  )
  
  # Calculate the mean slope, excluding NA values.
  mean_slope <- mean(slopes_data$slope, na.rm = TRUE)
  
  # Calculate the standard error of the mean slope.
  se_slope <- sd(slopes_data$slope, na.rm = TRUE) / sqrt(sum(!is.na(slopes_data$slope)))
  
  # Add a vertical line representing the mean slope.
  abline(v = mean_slope, col = "red", lwd = 2)
  
  # Print statistics to the console.
  cat(sprintf("%s Mean Slope: %.5f\n", variable_name, mean_slope))
  cat(sprintf("%s Slope Standard Error: %.5f\n", variable_name, se_slope))
  
  # Close the PDF device.
  dev.off()
}

# Generate histograms and compute statistics for temperature and precipitation slopes.
# MODULARITY EXAMPLE - Lesson 2: Writing extensible code.
# The 'plot_histogram' function can be used to plot rapidly and it is easily expandable for customization. 
# This plot_histogram function also prints the basic statistics such as Mean Slope, Mean of Slope Standard Error.
plot_histogram(temperature_slopes, "Temperature", "Temperature_Slopes_Histogram.pdf")
plot_histogram(precipitation_slopes, "Precipitation", "Precipitation_Slopes_Histogram.pdf")


#*** Create Maps ***

# Define a function to plot the slopes on a geographical map.
# MODULARITY EXAMPLE - The function is carefully designed to handle potential issues, such as negative alpha values in colors.
# it represents the resist of temptation to just code without planning.

# MODULARITY EXAMPLE - Defining the function modules precisely in advance.
# Inputs:
# - slopes_data: Data frame containing slope values and location coordinates.
# - variable_name: String for labeling.
# - filename: String specifying the output PDF file name.

plot_slope_map <- function(slopes_data, variable_name, filename) {
  # Open a PDF device to save the map.
  pdf(file = filename)
  
  # Set up the plotting area without margins
  par(mar = c(0, 0, 0, 0))
  
  # Plot the USA state boundaries
  maps::map(
    database = "state",  # Use the 'state' database for detailed USA boundaries
    xlim = range(slopes_data$lon, na.rm = TRUE) + c(-1, 1),  # Add a small buffer to the range
    ylim = range(slopes_data$lat, na.rm = TRUE) + c(-1, 1),
    fill = TRUE,          # Fill the states with color
    col = "lightgray",    # Set a light gray color for states
    border = "darkgray",  # Set a dark gray color for state borders
    resolution = 0        # Use the highest resolution available
  )
  
  # Determine the maximum absolute slope for color normalization
  max_abs_slope <- max(abs(slopes_data$slope), na.rm = TRUE)
  
  # Handle cases where the maximum absolute slope is zero or non-finite
  if (max_abs_slope == 0 || !is.finite(max_abs_slope)) {
    max_abs_slope <- 1  # Prevent division by zero
  }
  
  # Initialize the 'color' column in the data frame
  slopes_data$color <- NA_character_
  
  # Assign colors based on slope values
  
  # Positive slopes (increasing trends) - shades of red
  positive_indices <- which(slopes_data$slope > 0 & !is.na(slopes_data$slope))
  if (length(positive_indices) > 0) {
    slopes_data$color[positive_indices] <- rgb(
      red = 1,
      green = 0,
      blue = 0,
      alpha = pmin(slopes_data$slope[positive_indices] / max_abs_slope, 1)
    )
  }
  
  # Negative slopes (decreasing trends) - shades of blue
  negative_indices <- which(slopes_data$slope < 0 & !is.na(slopes_data$slope))
  if (length(negative_indices) > 0) {
    slopes_data$color[negative_indices] <- rgb(
      red = 0,
      green = 0,
      blue = 1,
      alpha = pmin(abs(slopes_data$slope[negative_indices]) / max_abs_slope, 1)
    )
  }
  
  # Zero slopes (no trend) - gray color
  zero_indices <- which(slopes_data$slope == 0 & !is.na(slopes_data$slope))
  if (length(zero_indices) > 0) {
    slopes_data$color[zero_indices] <- rgb(
      red = 0.5,
      green = 0.5,
      blue = 0.5,
      alpha = 0.5
    )
  }
  
  # Plot the data points with the assigned colors
  points(
    x = slopes_data$lon,
    y = slopes_data$lat,
    pch = 21,                # Filled circle
    cex = 0.6,               # Adjust point size as needed
    bg = slopes_data$color,  # Background color of the point
    col = "black",           # Border color of the point
    lwd = 0.2                # Line width of the point border
  )
  
  # Add a title to the map
  title(main = paste(variable_name, "Slope Map"), line = -2, cex.main = 1)
  
  # Close the PDF device
  dev.off()
}

# Generate maps for temperature and precipitation slopes
# MODULARITY EXAMPLE - Writing extensible code.
# The 'plot_slope_map' function can be extended to include legends, different color schemes, or interactive features.
# MODULARITY EXAMPLE - Instead of reusing the same chunk of code for plotting the slope maps for the 
# temparature and precipitation variable, here i used the plot_slope_map function to repeat the code for these two variables
plot_slope_map(temperature_slopes, "Temperature", "Temperature_Slope_Map.pdf")
plot_slope_map(precipitation_slopes, "Precipitation", "Precipitation_Slope_Map.pdf")
