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