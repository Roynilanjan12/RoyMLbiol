################################################################################
# Toxicity Classification using the caret Package
# Biol 701: Topics In Machine Learning in Biology, University of Kansas
#
# The dataset includes 171 molecules designed for functional domains of a core
# clock protein, CRY1, responsible for generating circadian rhythm.
# 56 of the molecules are toxic and the rest are non-toxic.
# Dataset source: https://archive.ics.uci.edu/dataset/728/toxicity-2
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

# Load the toxicity dataset
# Assumes the 'Toxicity-13F.csv' file is stored in the working directory
toxicity_data <- read.csv(file = "Toxicity-13F.csv")
head(toxicity_data)
dim(toxicity_data)

# Rearrange columns to have 'Class' as the first column
toxicity_data <- toxicity_data[, c("Class", setdiff(names(toxicity_data), "Class"))]

################################################################################
# Data Splitting
################################################################################

# Split the data into validation and testing sets using caret's createDataPartition
set.seed(101)  # For reproducibility
indices <- createDataPartition(toxicity_data$Class, p = 0.8, list = FALSE)

# Create validation (training) and testing datasets
validation_data <- toxicity_data[indices, ]
testing_data <- toxicity_data[-indices, ]

# Separate predictors and response for the validation set
predictors_validation <- validation_data[, 2:14]  # Exclude the 'Class' column
response_validation <- validation_data[, 1]       # The 'Class' column

################################################################################
# Parallel Processing Setup
################################################################################

# Set up parallel processing to speed up computation
cluster <- makePSOCKcluster(5)  # Adjust the number based on your machine's capabilities
registerDoParallel(cluster)

################################################################################
# Model Training with Decision Tree (rpart)
################################################################################

# Try out the main function 'train' in caret with an rpart model (decision tree)
# Since we already know about decision trees, we can focus on how to use 'train'

# Look up the model information for 'rpart'
modelLookup("rpart")  # Provides information on the model, tuning parameters, etc.

# Set up training control for cross-validation
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# This sets up k-fold cross-validation with 10 folds, repeated 3 times

# Train the rpart model
rpart_model <- train(x = predictors_validation, y = response_validation,
                     method = "rpart", trControl = train_control)

# Examine the model
class(rpart_model)
names(rpart_model)
rpart_model
rpart_model$results

# Make predictions on the validation data
predictions <- predict(rpart_model, validation_data)
table(predictions, response_validation)

# Make predictions using the final model directly
predictions_final_model <- predict(rpart_model$finalModel, validation_data)

# Convert probabilities to class labels
predictions_final_model_class <- apply(X = predictions_final_model, MARGIN = 1, FUN = function(x) {
  ifelse(x[1] < 0.5, "Toxic", "NonToxic")
})

# Check if the predictions from 'predict' and 'finalModel' are the same
identical_predictions <- sum(predictions == predictions_final_model_class)
identical_predictions  # Shows that predictions from 'predict' are from the best model

################################################################################
# Tuning the 'cp' Parameter in Decision Tree
################################################################################

# Tune the 'cp' (complexity parameter) by specifying 'tuneLength'
rpart_model_tuned <- train(x = predictors_validation, y = response_validation,
                           method = "rpart", trControl = train_control, tuneLength = 15)
rpart_model_tuned$results

# Plot the relationship between 'cp' and misclassification rate
plot(rpart_model_tuned$results$cp, 1 - rpart_model_tuned$results$Accuracy, type = "l",
     xlab = "Complexity Parameter (cp)", ylab = "Misclassification Rate",
     main = "Decision Tree Complexity vs. Misclassification Rate")
# Apparently, cp range 0.03 to 0.08 are better

# Control the specific values of 'cp' to use using 'tuneGrid'
tune_grid <- data.frame(cp = seq(from = 0.01, to = 0.8, by = 0.01))
rpart_model_tuned_grid <- train(x = predictors_validation, y = response_validation,
                                method = "rpart", trControl = train_control, tuneGrid = tune_grid)
rpart_model_tuned_grid$results

# Plot the relationship between 'cp' and misclassification rate
plot(rpart_model_tuned_grid$results$cp, 1 - rpart_model_tuned_grid$results$Accuracy, type = "l",
     xlab = "Complexity Parameter (cp)", ylab = "Misclassification Rate",
     main = "Decision Tree Complexity vs. Misclassification Rate")
# Apparently, cp range 0.2 to 0.8 are better and gives same results

# Get the best accuracy achieved
best_accuracy_rpart <- max(rpart_model_tuned_grid$results$Accuracy)  # This is the best accuracy we got
best_accuracy_rpart

################################################################################
# Preprocessing Data (Centering and Scaling)
################################################################################

# Now do it with preprocessing (centering and scaling) - experimental

set.seed(101)
rpart_model_no_preprocess <- train(x = predictors_validation, y = response_validation,
                                   method = "rpart", trControl = train_control, tuneLength = 15)
set.seed(101)
rpart_model_preprocess <- train(x = predictors_validation, y = response_validation,
                                method = "rpart", trControl = train_control, tuneLength = 15,
                                preProcess = c("center", "scale"))

# Compare the results
accuracy_comparison <- cbind(rpart_model_no_preprocess$results$Accuracy,
                             rpart_model_preprocess$results$Accuracy)
accuracy_difference <- rpart_model_no_preprocess$results$Accuracy - rpart_model_preprocess$results$Accuracy
accuracy_difference  # Same results, as expected, since centering and scaling makes no difference for CART

# Henceforth, we will use preprocessing, since some algorithms work better with centered, scaled data

################################################################################
# Model Training with k-Nearest Neighbors (k-NN)
################################################################################

# Try another classification algorithm: k-Nearest Neighbors (k-NN)

modelLookup("knn")  # Look up model information for 'knn'

# Train the k-NN model
knn_model <- train(x = predictors_validation, y = response_validation,
                   method = "knn", trControl = train_control, tuneLength = 15,
                   preProcess = c("center", "scale"))
knn_model$results  # 'k' is the single hyperparameter the routine varies
max(knn_model$results$Accuracy)

################################################################################
# Model Training with 'C5.0' Decision Tree
################################################################################

# Try an algorithm we have not yet learned: 'C5.0' decision tree

modelLookup("C5.0")  # Look up model information for 'C5.0'

# Train the C5.0 model
C5p0_model <- train(x = predictors_validation, y = response_validation,
                    method = "C5.0", trControl = train_control, tuneLength = 3,
                    preProcess = c("center", "scale"))
C5p0_model$results  # Some warnings to investigate if this turns out to be a useful method

max(C5p0_model$results$Accuracy)

################################################################################
# Training Multiple Models in a Loop
################################################################################

# Now let's do it with a loop - this is what we may ordinarily do from the beginning

# Define the training control again
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


set.seed(101)
# Look up model information for various algorithms
modelLookup("treebag")
modelLookup("rf")
modelLookup("AdaBoost.M1")
modelLookup("gbm")
modelLookup("LogitBoost")
modelLookup("svmLinear")
modelLookup("svmRadial")
modelLookup("nnet")

# List of model names to train
model_names <- c("rpart", "knn", "C5.0",        # Models we did already
                 "treebag",                     # Bagged CART, no tuning parameters
                 "rf",                          # Random Forest
                 "AdaBoost.M1",                 # Adaptive Boosting
                 "gbm",                         # Gradient Boosting Machine
                 "LogitBoost",                  # Boosted Logistic Regression
                 "svmLinear",                   # SVM with Linear Kernel
                 "svmRadial",                   # SVM with Radial Kernel
                 "nnet")                        # Neural Network

# Corresponding 'tuneLength' for each model
tune_lengths <- c(15, 15, 3,  # Values used above for the first three models
                  1,          # No tuning parameters for 'treebag', so 'tuneLength' doesn't matter
                  15,         # 'rf' - Random Forest
                  2,          # 'AdaBoost.M1'
                  5,          # 'gbm' - Gradient Boosting Machine
                  15,         # 'LogitBoost'
                  15,         # 'svmLinear'
                  5,          # 'svmRadial'
                  5)          # 'nnet' - Neural Network

# Initialize list to store model results
model_results <- list()

# Initialize data frame to store accuracies
accuracy_results <- data.frame(model = model_names,
                               Accuracy = NA * numeric(length(model_names)),
                               Kappa = NA * numeric(length(model_names)),
                               AccuracySD = NA * numeric(length(model_names)),
                               KappaSD = NA * numeric(length(model_names)))

# Loop over each model to train and evaluate
for (counter in 1:length(model_names)) {
  print(paste0(model_names[counter], "; ", counter, " of ", length(model_names)))
  model_results[[counter]] <- train(x = predictors_validation, y = response_validation,
                                    method = model_names[counter], trControl = train_control,
                                    tuneLength = tune_lengths[counter],
                                    preProcess = c("center", "scale"))
  # Find the index of the best model based on accuracy
  index <- which(model_results[[counter]]$results$Accuracy == max(model_results[[counter]]$results$Accuracy))
  index <- index[1]
  # Store the best accuracy and related metrics
  accuracy_results[counter, 2:5] <- model_results[[counter]]$results[index,
                                                                     c("Accuracy", "Kappa", "AccuracySD", "KappaSD")]
}
accuracy_results

# Seems like 'svmRadial or knn' is working best

################################################################################
# Further Tuning of the Best Model
################################################################################

# Try tuning the 'svmRadial' model further with more 'tuneLength' values
svm_radial_model <- train(x = predictors_validation, y = response_validation,
                          method = "svmRadial", trControl = train_control, tuneLength = 30)
svm_radial_model$results
max(svm_radial_model$results$Accuracy) ## did not improve that much from initial tune length

################################################################################
# Clean Up
################################################################################

# Save the workspace to a file (uncomment if you wish to save)
# save.image(file = "CaretClassificationDemo_WorkspaceAtEnd.RData")

# Stop the parallel cluster
stopCluster(cluster)
