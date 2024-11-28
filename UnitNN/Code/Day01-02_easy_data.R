# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(keras3)     # For building neural networks
library(reticulate) # For interfacing with Python (if needed)

#### set directory
setwd("/Volumes/Crucial_X6/BIOL701_ML/RoyMLbiol/UnitNN")

# Load and Prepare the Data ---------------------------------------------------

# This is the Fashion MNIST dataset.
# It consists of 70,000 grayscale images of 28x28 pixels each, divided into 10 classes.
# The following key represents what each category represents:
# 0 - T-shirt/top
# 1 - Trouser
# 2 - Pullover
# 3 - Dress
# 4 - Coat
# 5 - Sandal
# 6 - Shirt
# 7 - Sneaker
# 8 - Bag
# 9 - Ankle boot

# Load the dataset
dataset <- dataset_fashion_mnist()

# Explore the structure of the dataset
str(dataset)

# Separate the dataset into training and testing sets
train_images <- dataset$train$x  # Training images
train_labels <- dataset$train$y  # Training labels
test_images <- dataset$test$x    # Test images
test_labels <- dataset$test$y    # Test labels

# Data Exploration ------------------------------------------------------------

# The training images are 28x28 grayscale images
class(train_images)
dim(train_images)

# Display the first training image
train_images[1,,]
image(1:28, 1:28, train_images[1,,])  # Note: The image is rotated 90 degrees counterclockwise
train_labels[1]  # Label: 9 (Ankle boot)

# Display the second training image
train_images[2,,]
image(1:28, 1:28, train_images[2,,])
train_labels[2]  # Label: 0 (T-shirt/top)

# The labels are integers from 0 to 9
class(train_labels)
dim(train_labels)
head(train_labels)
sort(unique(train_labels))

# Data Preprocessing ----------------------------------------------------------

# Reshape the images so that each image is a vector of length 28^2 instead of a 28x28 matrix
dim(train_images) <- c(dim(train_images)[1], 28^2)
dim(train_images)
dim(test_images) <- c(dim(test_images)[1], 28^2)
dim(test_images)

# Rescale all pixel values to be between 0 and 1 (neural networks perform better with normalized data)
sort(unique(as.vector(train_images)))
sort(unique(as.vector(test_images)))  # Pixel values range from 0 to 255

train_images <- train_images / 255
test_images <- test_images / 255

# Build and Compile the Models ------------------------------------------------

# Model 1: Basic Neural Network with Dropout Layers
model_basic <- keras_model_sequential() %>%
  
  # First dense layer with 256 units and ReLU activation function
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  
  # Dropout layer to prevent overfitting (drops 40% of the neurons randomly)
  layer_dropout(rate = 0.4) %>%
  
  # Second dense layer with 128 units and ReLU activation function
  layer_dense(units = 128, activation = 'relu') %>%
  
  # Another dropout layer with a lower dropout rate (30%)
  layer_dropout(rate = 0.3) %>%
  
  # Output layer with 10 units (one for each class) and softmax activation function
  layer_dense(units = 10, activation = 'softmax')

# Display the model's architecture
summary(model_basic)

# Compile the model
model_basic %>% compile(
  loss = 'sparse_categorical_crossentropy',  # Suitable for multi-class classification
  optimizer = optimizer_rmsprop(),           # RMSProp optimizer
  metrics = c('accuracy')                    # Evaluate performance using accuracy
)

# Train the Model -------------------------------------------------------------

history_basic <- model_basic %>% fit(
  train_images, train_labels,      # Training data and labels
  epochs = 30,                     # Number of epochs
  batch_size = 128,                # Batch size for gradient updates
  validation_split = 0.2           # Use 20% of training data for validation
)

# Plot the training and validation metrics over epochs
plot(history_basic)

# Alternative Models ----------------------------------------------------------

# Model 2: Neural Network without Dropout Layers (to observe overfitting)
model_no_dropout <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

# Display the model's architecture
summary(model_no_dropout)

# Model 3: Enhanced Neural Network with Additional Layers and Dropout
model_enhanced <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 648, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10, activation = 'softmax')

# Display the model's architecture
summary(model_enhanced)

# Compile both models
model_no_dropout %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

model_enhanced %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the Alternative Models ------------------------------------------------

history_no_dropout <- model_no_dropout %>% fit(
  train_images, train_labels,
  epochs = 30,
  batch_size = 128,
  validation_split = 0.2
)
# Note: The overfitting is expected to be worse than in model_basic

history_enhanced <- model_enhanced %>% fit(
  train_images, train_labels,
  epochs = 30,
  batch_size = 128,
  validation_split = 0.2
)

# Compare Training Histories --------------------------------------------------

# Display training histories for comparison
history_basic
history_no_dropout
history_enhanced

# Plot the training and validation metrics for each model
plot(history_basic)
plot(history_no_dropout)
plot(history_enhanced)

# Make Predictions on Test Data -----------------------------------------------

# Use model_basic to make predictions on the test set (after evaluating accuracy on test and validation set)
predictions <- model_basic %>% predict(test_images)

# The predictions are probabilities for each class
class(predictions)
dim(predictions)
predictions[1, ]  # Probabilities for the first test image

# Convert probabilities to class labels
predicted_classes <- apply(
  X = predictions, 
  MARGIN = 1, 
  FUN = function(x) { which.max(x) - 1 }  # Subtract 1 because R is 1-indexed
)

class(predicted_classes)

# Calculate the accuracy on the test set
test_accuracy <- sum(predicted_classes == test_labels) / length(test_labels)
test_accuracy

# Visualization Functions -----------------------------------------------------

# Function to plot an image from the test set
plot_image <- function(image_vector) {
  image_matrix <- matrix(image_vector, 28, 28)
  image_matrix <- t(image_matrix)
  image_matrix <- image_matrix[, ncol(image_matrix):1]
  image(1:28, 1:28, image_matrix)
}

# Plot the first test image
plot_image(test_images[1, ])

# Analyze Misclassifications --------------------------------------------------

# Identify indices of misclassified images
misclassified_indices <- which(predicted_classes != test_labels)

# Function to review misclassified images
review_misclassification <- function(index) {
  print(paste("Model predicts:", predicted_classes[misclassified_indices[index]], 
              "Actual label:", test_labels[misclassified_indices[index]]))
  probabilities <- cbind(0:9, round(predictions[misclassified_indices[index], ], 3))
  colnames(probabilities) <- c("Class", "Probability")
  print(probabilities[probabilities[, 2] > 0.01, ])
  plot_image(test_images[misclassified_indices[index], ])
}

# Review the first misclassified image
review_misclassification(1) ##Model says Sandal but actually is a sneaker, hard to tell even by human.

# Create the Results directory if it doesn't exist
if (!dir.exists("Results")) {
  dir.create("Results")
}

png(filename="/Results/fashin_mnist_history_basic.png")
plot(history_basic)
dev.off()

png(filename="/Results/fashin_mnist_history_no_dropout.png")
plot(history_no_dropout)
dev.off()

png(filename="/Results/fashin_mnist_history_enhanced.png")
plot(history_enhanced)
dev.off()
