# Load necessary libraries
library(tidyverse)     # Collection of R packages for data manipulation and visualization
library(readxl)        # To read Excel files
library(ggplot2)       # For advanced data visualization
library(reshape2)      # For data reshaping
library(rpart)         # Recursive partitioning for decision trees
library(rpart.plot)    # Enhanced tree plots
library(ipred)         # Bagging and resampling methods
library(randomForest)  # For random forest implementation
library(adabag)        # For boosting algorithms
library(xgboost)       # Extreme gradient boosting

#---------------------------------------------------------------
# Data Loading and Preprocessing
#---------------------------------------------------------------

# Load the dataset
mouse_data <- read.csv("/Volumes/Crucial_X6/BIOL701_ML/RoyMLbiol/UnitCart/Data_Cortex_Nuclear.csv")

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

# Save the simplified dataset to a CSV file (adjust the path as needed)
setwd("/Volumes/Crucial_X6/BIOL701_ML/RoyMLbiol/UnitCart")
write.csv(mouse_data, "simplified_mouse_protein_expression_data_for_classifying_down_syndrome.csv", 
          row.names = FALSE, quote = FALSE)

#---------------------------------------------------------------
# Exploratory Data Analysis
#---------------------------------------------------------------

# Create a numeric-only version of the data for correlation analysis and plotting
numeric_data <- mouse_data
numeric_data$Genotype <- NULL

# Compute the correlation matrix of the numeric variables
correlation_matrix <- cor(numeric_data)

# Reshape the numeric dataframe from wide to long format for plotting
df_long <- melt(numeric_data)

# Plot histograms for each numeric variable using ggplot2
ggplot(df_long, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  facet_wrap(~variable, scales = "free_y") +  # Separate histograms for each variable
  theme_classic() +
  labs(x = "Protein Expression", y = "Count")