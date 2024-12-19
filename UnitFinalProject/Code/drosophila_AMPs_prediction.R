# Load necessary libraries
library(tidyverse)     # Collection of R packages for data manipulation and visualization
library(readxl)        # To read Excel files
library(ggplot2)       # For advanced data visualization
library(reshape2)      # For data reshaping
library(rpart)         # Recursive partitioning for decision trees
library(rpart.plot)    # Enhanced tree plots
library(ipred)         # Bagging and resAMP_1ling methods
library(randomForest)  # For random forest implementation
library(adabag)        # For boosting algorithms
library(xgboost)       # Extreme gradient boosting

#---------------------------------------------------------------
# Data Loading and Preprocessing
# The data is mainly consists of dipeptide composition of insect proteins
# Data contains around 400 known insect AMPs and 400 non AMPs of insects
# In the data there is also a variable that represents the pecent identify
# of these proteins to drosophila proteins.
# we included this variable as our goal is to predict drosophila AMPs
#---------------------------------------------------------------
# Load the dataset
## assuming that the working directory is set as the project directory
amp_data <- read_xlsx("./Data/insect_AMP_nonAMP.xlsx")

# Preview the first few rows
head(amp_data)
amp_data <- as.data.frame(amp_data)
rownames(amp_data) <- amp_data$name
amp_data$name<-NULL

names(amp_data) <- paste0(names(amp_data), "_1")
# Check the dimensions of the dataset
dim(amp_data)
library(readr)

relatedness <- read_table("./Data/amp_relatedness_Dmel.txt", col_names = FALSE)
colnames(relatedness) <- c("genes","pident")
merged_df <- merge(amp_data, relatedness, by.x = "row.names", by.y = "genes", all = TRUE)
rownames(merged_df) <- merged_df$Row.names
merged_df$Row.names <- NULL
library(IDPmisc)
merged_df <- NaRV.omit(merged_df)
amp_data <- merged_df
rm(merged_df,relatedness)
amp_data <- as.data.frame(amp_data)

#---------------------------------------------------------------
# Data Splitting for Training and Testing
#---------------------------------------------------------------

# Set seed for reproducibility
set.seed(101)

# Randomly shuffle the data
#data_permuted <- amp_data[sAMP_1le(nrow(amp_data)), ]
data_permuted <-amp_data[sample(dim(amp_data)[1],dim(amp_data)[1],replace=FALSE),]

# Split data into training (75%) and testing (25%) sets
train_index <- 1:floor(0.75 * nrow(amp_data))
data_train <- data_permuted[train_index, ]
data_test <- data_permuted[(max(train_index) + 1):nrow(amp_data), ]

# Consistency checks
dim(amp_data)        # Dimensions of the full dataset
dim(data_permuted)     # Dimensions after permutation
dim(data_train)        # Dimensions of the training set
dim(data_test)         # Dimensions of the testing set
nrow(data_train) + nrow(data_test)  

#---------------------------------------------------------------
# SVM (Radial Kernel) Model
#---------------------------------------------------------------

# Load the e1071 package for SVM
library(e1071)
set.seed(404)
# Convert the target variable to a factor if it's not already
# (Assuming binary classification: ensure AMP_1 is a factor with correct levels)
data_train$AMP_1 <- as.factor(data_train$AMP_1)

# Fit an SVM model with a radial kernel on the full training data
svm_model <- svm(AMP_1 ~ ., data = data_train, kernel = "radial", probability = FALSE)

# Predict on the training data
svm_pred_train <- predict(svm_model, data_train)

# Calculate training error rate
svm_train_error <- sum(svm_pred_train != data_train$AMP_1) / nrow(data_train)
svm_train_error  # Output the training error rate

# Number of folds in k-fold cross-validation
# Number of folds in k-fold cross-validation
num_folds <- 10

# Create fold assignments
fold_assignments <- rep(1:num_folds, length.out = nrow(data_train))

#---------------------------------------------------------------
# Cross-validation for SVM model
#---------------------------------------------------------------

# Initialize vector for cross-validation errors of SVM model
cv_errors_svm <- numeric(num_folds)
# Perform cross-validation
for (fold in 1:num_folds) {
  cat("Processing fold", fold, "of", num_folds, "\n")
  
  # Training data excluding the current fold
  train_cv <- data_train[fold_assignments != fold, ]
  
  # Validation data for the current fold
  validate_cv <- data_train[fold_assignments == fold, ]
  
  # Fit SVM model on training fold
  svm_model_cv <- svm(AMP_1 ~ ., data = train_cv, kernel = "radial", probability = FALSE)
  
  # Predict on the validation fold
  svm_pred_cv <- predict(svm_model_cv, validate_cv)
  
  # Calculate error rate for the fold
  cv_errors_svm[fold] <- sum(svm_pred_cv != validate_cv$AMP_1) / nrow(validate_cv)
}

# Mean cross-validation error rate for SVM model
mean_cv_error_svm <- mean(cv_errors_svm)
svm_train_error*100
mean_cv_error_svm*100

# Predict on the test set using the best model (e.g., svmRadial)
svmRadial_pred_test <- predict(svm_model, data_test[, -1])

# Calculate the test error rate
svm_test_error <- sum(svmRadial_pred_test != data_test$AMP_1) / nrow(data_test)
svm_test_error*100  # Output the test error rate

##### the train error for svmRadial is 0.3012048%, cross validation error is 1.95839%
##### and the test data error is 4.054054% which is a bit higher that cross validation (a little bit overfitted).

#---------------------------------------------------------------
# Random Forest Model
#---------------------------------------------------------------

# Fit a Random Forest model
random_forest_model <- randomForest(AMP_1 ~ ., data = data_train, ntree = 1000)

# View OOB error rate and other model details
print(random_forest_model)

# Predict on the training data (already stored in the model)
rf_pred_train <- random_forest_model$predicted

# Calculate training error rate
rf_train_error <- sum(rf_pred_train != data_train$AMP_1) / nrow(data_train)
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
  rf_model_cv <- randomForest(AMP_1 ~ ., data = train_cv, ntree = 1000)
  
  # Predict on the validation fold
  rf_pred_cv <- predict(rf_model_cv, validate_cv, type = "class")
  
  # Calculate error rate for the fold
  cv_errors_rf[fold] <- sum(rf_pred_cv != validate_cv$AMP_1) / nrow(validate_cv)
}

mean_cv_error_rf <- mean(cv_errors_rf)
rf_train_error*100 ### the value is 3.162651%
mean_cv_error_rf*100 #### the value is 3.163727%

# Predict on the test set using the best model (e.g., rf)
rf_pred_test <- predict(random_forest_model, data_test[, -1])


# Calculate the test error rate
rf_test_error <- sum(rf_pred_test != data_test$AMP_1) / nrow(data_test)
rf_test_error*100  # Output the test error rate (we find it around 3.153153%)
##the train, cross validation and test error are all around 3%, which shows no overfitting of the model.

#################
#Drosophila protein AMP prediction
#This is the same format data with same variables as the data we used for the svmRadial and Random forest model,
#but this is for all the drosophila proteins to identify which ones are the AMPs.
#the data is mainly consists of dipeptide composition of proteins
################
Dmel_DPC <- read_csv("./Data/Dmel_DPC.csv")
Dmel_DPC <- as.data.frame(Dmel_DPC)
rownames(Dmel_DPC) <- Dmel_DPC$name
Dmel_DPC$name <- NULL

names(Dmel_DPC) <- paste0(names(Dmel_DPC), "_1")
set.seed(123) # Setting seed for reproducibility
Dmel_DPC <- as.data.frame(Dmel_DPC)
Dmel_DPC$pident <- runif(nrow(Dmel_DPC), min = 98, max = 100)


# Predict on the test set using the best model (e.g., rf)
rf_pred_Dmel_DPC <- predict(random_forest_model, Dmel_DPC)
rf_predict_amp_drosophila <- as.data.frame(rf_pred_Dmel_DPC)
rf_predict_amp_drosophila$gene <- rownames(rf_predict_amp_drosophila)
table(rf_predict_amp_drosophila$rf_pred_Dmel_DPC) ### note that, same gene (mRNA) can produce multiple proteins
###so, there are multiple proteins here that are generated by same genes. Here we predicted 1376 proteins as
###AMPs but if you count genes they are 930 genes. You can figure that out by looking at the '-PA,-PB,-PC strings' 
###at the end of the protein names.
colnames(rf_predict_amp_drosophila) <- c("predict","gene")

# Assuming random_forest_model is your trained random forest model
# Extract variable importance
var_importance <- as.data.frame(randomForest::importance(random_forest_model))
var_importance$Variable <- rownames(var_importance)

# Remove '_1' from the variable names
var_importance$Variable <- gsub("_1$", "", var_importance$Variable)

# Sort the variables by importance
var_importance <- var_importance[order(var_importance$MeanDecreaseGini, decreasing = TRUE), ]

# Select the top 10 variables
top_vars <- var_importance[1:10, ]

# Load ggplot2
library(ggplot2)

# Create the plot
ggplot(top_vars, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue4") +
  coord_flip() +
  labs(
    title = "Top 10 Amino Acid Dipeptide Composition features for predicting AMPs",
    x = "Amino Acid Pairs",
    y = "Mean Decrease in Gini"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and centered title
    axis.title = element_text(size = 14, face = "bold"),               # Bold axis titles
    axis.text = element_text(size = 12, face = "bold")                # Bold axis text
  )

####Quality matrices of the Random forest model
# Load necessary libraries
library(ROCR)
library(pROC)
library(ggplot2)
library(caret)

# Assuming `random_forest_model` is your trained random forest model and `data_test` is your test data
# Predict probabilities for test data
rf_prob <- predict(random_forest_model, data_test[, -1], type = "prob")
rf_pred <- predict(random_forest_model, data_test[, -1])

# Ensure that the actual test labels are factors
data_test$AMP_1 <- as.factor(data_test$AMP_1)

# Ensure that the predicted labels are factors with the same levels as the test labels
rf_pred <- factor(rf_pred, levels = levels(data_test$AMP_1))

# Calculate Confusion Matrix
confusion <- confusionMatrix(rf_pred, data_test$AMP_1, positive = "1")
accuracy <- confusion$overall["Accuracy"]
precision <- confusion$byClass["Precision"]
recall <- confusion$byClass["Recall"]
f1_score <- confusion$byClass["F1"]

# Calculate ROC and AUC
roc_curve <- roc(data_test$AMP_1, rf_prob[, 2])
auc_value <- auc(roc_curve)

# Calculate Log Loss
log_loss <- -mean(ifelse(data_test$AMP_1 == "1", 
                         log(rf_prob[, 2]), 
                         log(1 - rf_prob[, 2])))

# Prepare data for plotting
metrics <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall (Sensitivity)", "F1 Score", "AUC (ROC)", "Log Loss"),
  Value = c(accuracy, precision, recall, f1_score, auc_value, log_loss)
)

# Plot Metrics
plot <- ggplot(metrics, aes(x = reorder(Metric, Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Random Forest Model Evaluation Metrics",
    x = "Metric",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold")
  )

print(plot)

# Plot ROC Curve
roc_plot <- ggplot() +
  geom_line(aes(x = 1 - roc_curve$specificities, y = roc_curve$sensitivities), color = "blue", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "ROC Curve",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold")
  )

print(roc_plot)

# Prepare error rates for training, cross-validation, and test sets
error_rates <- data.frame(
  Metric = c("Training Error", "Cross-Validation Error", "Test Error"),
  Value = c(3.162651, 3.163727, 3.153153)
)

# Plot error rates
error_plot <- ggplot(error_rates, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(
    x = "Metric",
    y = "Error Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold")
  )

print(error_plot)


