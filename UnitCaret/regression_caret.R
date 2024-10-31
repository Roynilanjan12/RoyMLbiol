################################################################################
# Regression Exercise using caret and the CASP Dataset
# Biol 701: Topics In: Machine Learning in Biology, University of Kansas
# Fall 2024
#
# This is a dataset of Physicochemical Properties of Protein Tertiary Structure.
# The data set is taken from CASP 5-9. There are 45,730 decoys with sizes varying
# from 0 to 21 angstroms.
#
# Variable Information:
# RMSD - Size of the residue.
# F1   - Total surface area.
# F2   - Non-polar exposed area.
# F3   - Fractional area of exposed non-polar residue.
# F4   - Fractional area of exposed non-polar part of residue.
# F5   - Molecular mass weighted exposed area.
# F6   - Average deviation from standard exposed area of residue.
# F7   - Euclidean distance.
# F8   - Secondary structure penalty.
# F9   - Spatial distribution constraints (N, K Value)
#
# Dataset source:
# https://archive.ics.uci.edu/dataset/265/physicochemical+properties+of+protein+tertiary+structure
################################################################################

# Clear the workspace and close all graphics windows
rm(list = ls())
graphics.off()

# Load necessary libraries
library(caret)
library(doParallel)

################################################################################
# Data Preparation
################################################################################

# Load the CASP dataset
# Assumes the 'CASP.csv' file is stored in the working directory
casp_data <- read.csv(file = "CASP.csv")
head(casp_data)
dim(casp_data)

# Check for missing values and data types in each column
lapply(X = casp_data, FUN = function(x) { sum(is.na(x)) })  # Count of NA values in each column
lapply(X = casp_data, FUN = function(x) { class(x) })       # Data type of each column

################################################################################
# Data Splitting
################################################################################

# Split the data into validation (training) and testing sets using caret's createDataPartition
set.seed(101)  # For reproducibility
indices <- createDataPartition(casp_data$RMSD, p = 0.8, list = FALSE)

# Create validation (training) and testing datasets
validation_data <- casp_data[indices, ]
testing_data <- casp_data[-indices, ]

# Separate predictors and response for the validation set
predictors_validation <- validation_data[, 2:10]  # Exclude the 'RMSD' column (column 1)
response_validation <- validation_data[, 1]       # 'RMSD' column

# Separate predictors and response for the testing set
predictors_testing <- testing_data[, 2:10]
response_testing <- testing_data[, 1]

################################################################################
# Data Preprocessing
################################################################################

# Preprocess the data (centering and scaling)
preprocess_params <- preProcess(predictors_validation, method = c("center", "scale"))

# Apply the preprocessing to the validation and testing predictors
predictors_validation <- predict(preprocess_params, predictors_validation)
predictors_testing <- predict(preprocess_params, predictors_testing)

################################################################################
# Parallel Processing Setup
################################################################################

# Set up parallel processing to speed up computation
cluster <- makePSOCKcluster(5)  # Number of cores/processes to use
registerDoParallel(cluster)

################################################################################
# Model Training with caret
################################################################################

set.seed(101)

# Define the list of model methods to try
model_methods <- c("rpart",      # Decision Tree
                   "treebag",    # Bagged Trees
                   "lm",         # Linear Model
                   "glm",        # Generalized Linear Model
                   "glmboost",   # Boosted Generalized Linear Model
                   "glmnet",     # Regularized Generalized Linear Model
                   "enet",       # Elastic Net
                   "penalized")  # Penalized Linear Models

# Determine 'tuneLength' to use for each model based on the number of tuning parameters
num_tuning_params <- sapply(as.list(model_methods), FUN = function(x) { dim(modelLookup(x))[1] })
tune_lengths <- round(2^(1 / num_tuning_params)) #intentionally kept the tune lengths low
                                                 #as it was taking huge time for this dataset for all the models

# Set up training control for cross-validation
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Initialize list to store model results
model_results <- list()

# Initialize data frame to store performance metrics
performance_metrics <- data.frame(model = model_methods,
                                  RMSE = NA * numeric(length(model_methods)),
                                  Rsquared = NA * numeric(length(model_methods)),
                                  MAE = NA * numeric(length(model_methods)),
                                  RMSESD = NA * numeric(length(model_methods)),
                                  RsquaredSD = NA * numeric(length(model_methods)),
                                  MAESD = NA * numeric(length(model_methods)))

# Loop over each model to train and evaluate
for (i in 1:length(model_methods)) {
  print(paste0(model_methods[i], "; ", i, " of ", length(model_methods)))
  
  # Train the model
  model_results[[i]] <- train(x = predictors_validation, y = response_validation,
                              method = model_methods[i],
                              trControl = train_control, tuneLength = tune_lengths[i])
  
  # Find the index of the best model based on RMSE
  best_index <- which(model_results[[i]]$results$RMSE == max(model_results[[i]]$results$RMSE))
  best_index <- best_index[1]
  
  # Store the best performance metrics
  performance_metrics[i, 2:7] <- model_results[[i]]$results[best_index,
                                                            c("RMSE", "Rsquared", "MAE", "RMSESD", "RsquaredSD", "MAESD")]
}

# Display the performance metrics for each model
performance_metrics

# Seems like 'penalized', 'lm', or 'glm' is working best among the models tested

################################################################################
# Further Tuning of the Best Model
################################################################################

# Try tuning the 'glm' model further with more 'tuneLength' values
best_model <- train(x = predictors_validation, y = response_validation,
                    method = "glm", trControl = train_control, tuneLength = 5)

max(best_model$results$RMSE)  # Did not improve much from initial tune length

################################################################################
# Clean Up
################################################################################

# Save the workspace to a file (uncomment if you wish to save)
# save.image(file = "CaretRegressionDemo_WorkspaceAtEnd.RData")

# Stop the parallel cluster
stopCluster(cluster)
