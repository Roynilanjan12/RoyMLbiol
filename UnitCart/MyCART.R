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

#---------------------------------------------------------------
# Data Splitting for Training and Testing
#---------------------------------------------------------------

# Rearrange columns to have 'Genotype' as the first column
mouse_data <- mouse_data[, c("Genotype", setdiff(names(mouse_data), "Genotype"))]

# Set seed for reproducibility
set.seed(101)

# Randomly shuffle the data
#data_permuted <- mouse_data[sample(nrow(mouse_data)), ]
data_permuted <-mouse_data[sample(dim(mouse_data)[1],dim(mouse_data)[1],replace=FALSE),]

# Split data into training (75%) and testing (25%) sets
train_index <- 1:floor(0.75 * nrow(mouse_data))
data_train <- data_permuted[train_index, ]
data_test <- data_permuted[(max(train_index) + 1):nrow(mouse_data), ]

# Consistency checks
dim(mouse_data)        # Dimensions of the full dataset
dim(data_permuted)     # Dimensions after permutation
dim(data_train)        # Dimensions of the training set
dim(data_test)         # Dimensions of the testing set
nrow(data_train) + nrow(data_test)  # Should equal the total number of samples

#---------------------------------------------------------------
# Decision Tree (CART) Model
#---------------------------------------------------------------

# Fit a default CART decision tree to the training data
cart_model_default <- rpart(Genotype ~ ., data = data_train, method = "class")

# Plot the decision tree
rpart.plot(cart_model_default)

# Predict on the training data
cart_pred_train <- predict(cart_model_default, type = "class")

# Confusion matrix
table(Predicted = cart_pred_train, Actual = data_train$Genotype)

# Calculate training error rate
cart_train_error <- sum(cart_pred_train != data_train$Genotype) / nrow(data_train)
cart_train_error  # Output the error rate

# Fit a full CART tree with no pruning (cp=0, minsplit=1)
cart_model_full <- rpart(Genotype ~ ., data = data_train, method = "class",
                         control = rpart.control(cp = 0, minsplit = 1))

# Plot the full decision tree
rpart.plot(cart_model_full)

# Predict on the training data using the full CART model
cart_full_pred_train <- predict(cart_model_full, type = "class")

# Confusion matrix
table(Predicted = cart_full_pred_train, Actual = data_train$Genotype)

# Calculate training error rate
cart_full_train_error <- sum(cart_full_pred_train != data_train$Genotype) / nrow(data_train)
cart_full_train_error  # Output the error rate

#---------------------------------------------------------------
# Cross-Validation for Decision Trees
#---------------------------------------------------------------

# Number of folds in k-fold cross-validation
num_folds <- 10

# Create fold assignments
fold_assignments <- rep(1:num_folds, length.out = nrow(data_train))

# Initialize vectors to store cross-validation errors
cv_errors_default <- numeric(num_folds)
cv_errors_full <- numeric(num_folds)

# Cross-validation for default CART model
for (fold in 1:num_folds) {
  # Training data excluding the current fold
  train_cv <- data_train[fold_assignments != fold, ]
  # Validation data for the current fold
  validate_cv <- data_train[fold_assignments == fold, ]
  
  # Fit the model
  cart_model_cv <- rpart(Genotype ~ ., data = train_cv, method = "class")
  
  # Predict on the validation fold
  cart_pred_cv <- predict(cart_model_cv, validate_cv, type = "class")
  
  # Calculate error rate for the fold
  cv_errors_default[fold] <- sum(cart_pred_cv != validate_cv$Genotype) / nrow(validate_cv)
}

# Cross-validation for full CART model
for (fold in 1:num_folds) {
  # Training data excluding the current fold
  train_cv <- data_train[fold_assignments != fold, ]
  # Validation data for the current fold
  validate_cv <- data_train[fold_assignments == fold, ]
  
  # Fit the full CART model
  cart_full_model_cv <- rpart(Genotype ~ ., data = train_cv, method = "class",
                              control = rpart.control(cp = 0, minsplit = 1))
  
  # Predict on the validation fold
  cart_full_pred_cv <- predict(cart_full_model_cv, validate_cv, type = "class")
  
  # Calculate error rate for the fold
  cv_errors_full[fold] <- sum(cart_full_pred_cv != validate_cv$Genotype) / nrow(validate_cv)
}

# Mean cross-validation error rates
mean_cv_error_default <- mean(cv_errors_default)
mean_cv_error_default
mean_cv_error_full <- mean(cv_errors_full)
mean_cv_error_full ###full cart doing better even after cross validation

# Plot complexity parameter (cp) for the full CART model
plotcp(cart_model_full)
printcp(cart_model_full)

# Plot cp for the default CART model
plotcp(cart_model_default)
printcp(cart_model_default)

#---------------------------------------------------------------
# Pruned Decision Tree Model
#---------------------------------------------------------------

# Fit a pruned CART model with specified cp value
# based on plotcp(cart_model_full), #36 trees can produce good result
cart_model_pruned <- rpart(Genotype ~ ., data = data_train, method = "class",
                           control = rpart.control(cp = 0.0012920, minsplit = 1))

# Plot the pruned tree (more interpretable)
plot(cart_model_pruned, uniform = TRUE, margin = 0.1)
text(cart_model_pruned, use.n = TRUE, all = TRUE, cex = 0.8)
##plot the pruned tree with rpart.plot function
rpart.plot(cart_model_pruned)

# Plot cp for the pruned tree
plotcp(cart_model_pruned)

# Initialize vector for cross-validation errors of pruned tree
cv_errors_pruned <- numeric(num_folds)

# Cross-validation for pruned CART model
for (fold in 1:num_folds) {
  # Training data excluding the current fold
  train_cv <- data_train[fold_assignments != fold, ]
  # Validation data for the current fold
  validate_cv <- data_train[fold_assignments == fold, ]
  
  # Fit the pruned CART model
  cart_pruned_model_cv <- rpart(Genotype ~ ., data = train_cv, method = "class",
                                control = rpart.control(cp = 0.0012920, minsplit = 1))
  
  # Predict on the validation fold
  cart_pruned_pred_cv <- predict(cart_pruned_model_cv, validate_cv, type = "class")
  
  # Calculate error rate for the fold
  cv_errors_pruned[fold] <- sum(cart_pruned_pred_cv != validate_cv$Genotype) / nrow(validate_cv)
}

# Mean cross-validation error rate for pruned tree
mean_cv_error_pruned <- mean(cv_errors_pruned)
mean_cv_error_default
mean_cv_error_full
mean_cv_error_pruned ### full cart with 41 trees and pruned cart with 36 trees bascially giving same cross validation error rate

#---------------------------------------------------------------
# Bagging Model
#---------------------------------------------------------------

# Fit a bagging model using ipred's bagging function
bagging_model <- ipred::bagging(Genotype ~ ., data = data_train, nbagg = 500, coob = TRUE, method = "class",
                                control = rpart.control(cp = 0, minsplit = 1, xval = 0))

# View out-of-bag error estimate
bagging_model$err

# Predict on the training data
bagging_pred_train <- predict(bagging_model)

# Calculate training error rate
bagging_train_error <- sum(bagging_pred_train != data_train$Genotype) / nrow(data_train)
bagging_train_error  # Output the error rate

# Initialize vector for cross-validation errors of bagging model
cv_errors_bagging <- numeric(num_folds)

# Cross-validation for bagging model
for (fold in 1:num_folds) {
  # Training data excluding the current fold
  train_cv <- data_train[fold_assignments != fold, ]
  # Validation data for the current fold
  validate_cv <- data_train[fold_assignments == fold, ]
  
  # Fit the bagging model
  bagging_model_cv <- ipred::bagging(Genotype ~ ., data = train_cv, nbagg = 500, coob = FALSE, method = "class",
                                     control = rpart.control(cp = 0, minsplit = 1, xval = 0))
  
  # Predict on the validation fold
  bagging_pred_cv <- predict(bagging_model_cv, validate_cv, type = "class")
  
  # Calculate error rate for the fold
  cv_errors_bagging[fold] <- sum(bagging_pred_cv != validate_cv$Genotype) / nrow(validate_cv)
}

# Mean cross-validation error rate for bagging model
mean_cv_error_bagging <- mean(cv_errors_bagging)
mean_cv_error_default
mean_cv_error_full
mean_cv_error_pruned
mean_cv_error_bagging ## cross validation with bagging shows, bagging reduces the error rate significantly 

#---------------------------------------------------------------
# Random Forest Model
#---------------------------------------------------------------

# Fit a Random Forest model
random_forest_model <- randomForest(Genotype ~ ., data = data_train, ntree = 1000)

# View OOB error rate and other model details
print(random_forest_model)

# Predict on the training data (already stored in the model)
rf_pred_train <- random_forest_model$predicted

# Calculate training error rate
rf_train_error <- sum(rf_pred_train != data_train$Genotype) / nrow(data_train)
rf_train_error  # Output the error rate

# Plot error rate vs number of trees
plot(random_forest_model)

# Initialize vector for cross-validation errors of Random Forest model
cv_errors_rf <- numeric(num_folds)

# Cross-validation for Random Forest model
for (fold in 1:num_folds) {
  # Training data excluding the current fold
  train_cv <- data_train[fold_assignments != fold, ]
  # Validation data for the current fold
  validate_cv <- data_train[fold_assignments == fold, ]
  
  # Fit the Random Forest model
  rf_model_cv <- randomForest(Genotype ~ ., data = train_cv, ntree = 1000)
  
  # Predict on the validation fold
  rf_pred_cv <- predict(rf_model_cv, validate_cv, type = "class")
  
  # Calculate error rate for the fold
  cv_errors_rf[fold] <- sum(rf_pred_cv != validate_cv$Genotype) / nrow(validate_cv)
}

# Mean cross-validation error rate for Random Forest model
mean_cv_error_rf <- mean(cv_errors_rf)
mean_cv_error_default
mean_cv_error_full
mean_cv_error_pruned
mean_cv_error_bagging
mean_cv_error_rf ### so far random forest is performing well

#---------------------------------------------------------------
# AdaBoost Model
#---------------------------------------------------------------

# Fit an AdaBoost model using the adabag package
adaboost_model <- boosting(Genotype ~ ., data = data_train)

# Predict on the training data
adaboost_pred_train <- predict(adaboost_model, data_train[, -1])$class  # Exclude 'Genotype' column

# Calculate training error rate
adaboost_train_error <- sum(adaboost_pred_train != data_train$Genotype) / nrow(data_train)
adaboost_train_error  # Output the error rate (we are getting here 0 error rate)

# View variable importance
adaboost_model$importance

# Initialize vector for cross-validation errors of AdaBoost model
cv_errors_adaboost <- numeric(num_folds)

# Cross-validation for AdaBoost model
for (fold in 1:num_folds) {
  print(paste("Processing fold", fold, "of", num_folds))
  
  # Training data excluding the current fold
  train_cv <- data_train[fold_assignments != fold, ]
  # Validation data for the current fold
  validate_cv <- data_train[fold_assignments == fold, ]
  
  # Fit the AdaBoost model
  adaboost_model_cv <- boosting(Genotype ~ ., data = train_cv)
  
  # Predict on the validation fold
  adaboost_pred_cv <- predict(adaboost_model_cv, validate_cv[, -1])$class  # Exclude 'Genotype' column
  
  # Calculate error rate for the fold
  cv_errors_adaboost[fold] <- sum(adaboost_pred_cv != validate_cv$Genotype) / nrow(validate_cv)
}

# Mean cross-validation error rate for AdaBoost model
mean_cv_error_adaboost <- mean(cv_errors_adaboost)
mean_cv_error_default
mean_cv_error_full
mean_cv_error_pruned
mean_cv_error_bagging
mean_cv_error_rf
mean_cv_error_adaboost ###so far adaboost giving the best model with error rate 0.007407407

#---------------------------------------------------------------
# XGBoost Model
#---------------------------------------------------------------

# Prepare data for XGBoost
X_train <- as.matrix(data_train[, -1])  # Exclude 'Genotype' column
y_train <- as.integer(data_train$Genotype) - 1  # Convert factors to integers starting from 0

# Fit an XGBoost model
xgboost_model <- xgboost(data = X_train, label = y_train, max_depth = 6, eta = 0.3,
                         nthread = 2, nrounds = 20, objective = "binary:logistic", verbose = 2)

# Predict on the training data
xgboost_pred_train <- predict(xgboost_model, X_train)

# Convert probabilities to class labels
xgboost_class_train <- ifelse(xgboost_pred_train > 0.5, "Down", "Control")

# Calculate training error rate
xgboost_train_error <- sum(xgboost_class_train != as.character(data_train$Genotype)) / nrow(data_train)
xgboost_train_error  # Output the error rate (giving awful error rate (0.47))

# Initialize vector for cross-validation errors of XGBoost model
cv_errors_xgboost <- numeric(num_folds)

# Cross-validation for XGBoost model
for (fold in 1:num_folds) {
  print(paste("Processing fold", fold, "of", num_folds))
  
  # Training data excluding the current fold
  train_cv <- data_train[fold_assignments != fold, ]
  X_train_cv <- as.matrix(train_cv[, -1])
  y_train_cv <- as.integer(train_cv$Genotype) - 1
  
  # Validation data for the current fold
  validate_cv <- data_train[fold_assignments == fold, ]
  X_validate_cv <- as.matrix(validate_cv[, -1])
  
  # Fit the XGBoost model
  xgboost_model_cv <- xgboost(data = X_train_cv, label = y_train_cv, max_depth = 7, eta = 0.2,
                              subsample = 0.5, nrounds = 55, nthread = 2, objective = "binary:logistic",
                              verbose = 0)
  
  # Predict on the validation fold
  xgboost_pred_cv <- predict(xgboost_model_cv, X_validate_cv)
  xgboost_class_cv <- ifelse(xgboost_pred_cv > 0.5, "Down", "Control")
  
  # Calculate error rate for the fold
  cv_errors_xgboost[fold] <- sum(xgboost_class_cv != as.character(validate_cv$Genotype)) / nrow(validate_cv)
}

# Mean cross-validation error rate for XGBoost model
mean_cv_error_xgboost <- mean(cv_errors_xgboost)
mean_cv_error_default
mean_cv_error_full
mean_cv_error_pruned
mean_cv_error_bagging
mean_cv_error_rf
mean_cv_error_adaboost ### seems to be best model
mean_cv_error_xgboost ###cross validation also confirms horrible error rate with xgboost
