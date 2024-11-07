#### Loading packages
library(tidyverse)

#---------------------------------------------------------------
# Data Loading and Preprocessing
#---------------------------------------------------------------

### This is a protein expression data of control mouse and mouse with down syndrome 

# Load the dataset
mouse_data <- read.csv("./Data/Data_Cortex_Nuclear.csv")

# Preview the first few rows
head(mouse_data)

# Check the dimensions of the dataset
dim(mouse_data)

# Remove columns that are not needed for the analysis
mouse_data$Treatment <- NULL
mouse_data$Behavior <- NULL
mouse_data$class <- NULL
mouse_data$MouseID <- NULL

# Replace NA values with zeros (acceptable for protein expression data)
mouse_data[is.na(mouse_data)] <- 0

# Optionally, remove columns that have any NA values (uncomment if needed)
# mouse_data <- mouse_data[, colSums(is.na(mouse_data)) == 0]

# Ensure that 'Genotype' is treated as a categorical variable
mouse_data$Genotype <- as.factor(mouse_data$Genotype)

