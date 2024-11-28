# Load Necessary Libraries ------------------------------------------------------

# Load the tidyverse for data manipulation and visualization
library(tidyverse)

# Install and load the keras3 package for building neural networks
#install.packages("keras3")
library(keras3)

# Install and load the reticulate package to interface with Python
#install.packages("reticulate")
library(reticulate)

# Install and load the jpeg package for reading JPEG images
#install.packages('jpeg')
library(jpeg)

# Install and load the imager package for image processing
#install.packages('imager')
library(imager)

# Data Preparation --------------------------------------------------------------

# Unzip the dataset containing MRI images of brain tumors
#unzip("/content/tumor_MRI_Data.zip", exdir = "/content/tumor_MRI_data")

#### set directory
setwd("/Volumes/Crucial_X6/BIOL701_ML/RoyMLbiol/UnitNN")

# Define the path to the dataset
dataset_path <- "/Data/tumor_MRI_Data"

# Extract all the file names from each class directory
glioma_files <- list.files(path = paste0(dataset_path, "/glioma_tumor/"))
meningioma_files <- list.files(path = paste0(dataset_path, "/meningioma_tumor/"))
normal_files <- list.files(path = paste0(dataset_path, "/normal/"))
pituitary_files <- list.files(path = paste0(dataset_path, "/pituitary_tumor/"))

# Load Images as Tensors --------------------------------------------------------

num_images <- 200   # Number of images to load per class, loading 200 to decrease the processing time
img_height <- 256
img_width <- 256
num_channels <- 3   # RGB images

# Initialize arrays to store images for each class
glioma_array <- array(NA, c(num_images, img_height, img_width, num_channels))
meningioma_array <- array(NA, c(num_images, img_height, img_width, num_channels))
normal_array <- array(NA, c(num_images, img_height, img_width, num_channels))
pituitary_array <- array(NA, c(num_images, img_height, img_width, num_channels))

# Load images for each class
for (i in 1:num_images) {
  glioma_array[i, , , ] <- readJPEG(source = paste0(dataset_path, "/glioma_tumor/", glioma_files[i]))
  meningioma_array[i, , , ] <- readJPEG(source = paste0(dataset_path, "/meningioma_tumor/", meningioma_files[i]))
  normal_array[i, , , ] <- readJPEG(source = paste0(dataset_path, "/normal/", normal_files[i]))
  pituitary_array[i, , , ] <- readJPEG(source = paste0(dataset_path, "/pituitary_tumor/", pituitary_files[i]))
}

# Function to Facilitate Viewing Images -----------------------------------------

view_image <- function(image_array, index) {
  img <- image_array[index, , , ]
  img <- aperm(img, c(2, 1, 3))  # Adjust dimensions for plotting
  img <- as.cimg(img)
  plot(img)
}

# Display the first image from each class
view_image(glioma_array, 1)       # Glioma tumor image
view_image(meningioma_array, 1)   # Meningioma tumor image
view_image(normal_array, 1)       # Normal brain image
view_image(pituitary_array, 1)    # Pituitary tumor image

# Combine Data and Generate Labels ----------------------------------------------

# Combine all classes into a single array
total_images <- 4 * num_images
image_data <- array(NA, c(total_images, img_height, img_width, num_channels))

image_data[1:num_images, , , ] <- glioma_array
rm(glioma_array)  # Free up memory

image_data[(num_images + 1):(2 * num_images), , , ] <- meningioma_array
rm(meningioma_array)

image_data[(2 * num_images + 1):(3 * num_images), , , ] <- normal_array
rm(normal_array)

image_data[(3 * num_images + 1):(4 * num_images), , , ] <- pituitary_array
rm(pituitary_array)

# Generate labels: 0 = glioma, 1 = meningioma, 2 = normal, 3 = pituitary
labels <- c(
  rep(0, num_images),
  rep(1, num_images),
  rep(2, num_images),
  rep(3, num_images)
)

# Class codes for reference
class_codes <- c("glioma", "meningioma", "normal", "pituitary")

# Shuffle the Dataset -----------------------------------------------------------

set.seed(101)  # For reproducibility
indices <- sample(1:total_images, size = total_images, replace = FALSE)

# Shuffle the image data and labels
image_data <- image_data[indices, , , ]
labels <- labels[indices]

# Preprocess Inputs for the Model -----------------------------------------------

# Check the range before scaling
range(image_data)  # Should be between 0 and 1

# Scale pixel values to be between -1 and 1 (required for Xception)
image_data <- 2 * image_data - 1

# Split into Training and Testing Datasets --------------------------------------

split_ratio <- 0.2  # Percentage for testing
num_test <- round(split_ratio * length(labels))

# Split the data
x_test <- image_data[1:num_test, , , ]
x_train <- image_data[(num_test + 1):length(labels), , , ]
y_test <- labels[1:num_test]
y_train <- labels[(num_test + 1):length(labels)]

# Check for balanced classes
table(y_train)
table(y_test)

# Remove unnecessary variables to free up memory
rm(image_data, labels)

# Build and Compile the Models --------------------------------------------------

# Model 1: Convolutional Neural Network with Dropout Layers
model1 <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                input_shape = c(img_height, img_width, num_channels), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(4, 4)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 4, activation = "softmax")

# Display the model architecture
summary(model1)

# Compile the model
model1 %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the Model ---------------------------------------------------------------

history1 <- model1 %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 32,
  validation_split = 0.2
)

# View training history
history1
plot(history1)

# Build Alternative Models ------------------------------------------------------

# Model 2: Adjusted Dropout Rates
model2 <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                input_shape = c(img_height, img_width, num_channels), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(4, 4)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 4, activation = "softmax")

# Display the model architecture
summary(model2)

# Compile the model
model2 %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train Model 2
history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 32,
  validation_split = 0.2
)

# Model 3: Simplified Model with Fewer Layers
model3 <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                input_shape = c(img_height, img_width, num_channels), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(4, 4)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 4, activation = "softmax")

# Display the model architecture
summary(model3)

# Compile the model
model3 %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train Model 3
history3 <- model3 %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 32,
  validation_split = 0.2
)

# Evaluate and Compare Models ---------------------------------------------------

# View training histories
history2
plot(history2)

history3
plot(history3)

# Make Predictions on Test Data -------------------------------------------------

# Get the predictions in one-hot format from Model 2 after evaluating accuracy and accuray on validation set
predictions <- model2 %>% predict(x_test)

# The predictions are probabilities for each class
class(predictions)
dim(predictions)
predictions[1, ]  # Probabilities for the first test image

# Convert probabilities to class labels
predicted_labels <- apply(
  X = predictions,
  MARGIN = 1,
  FUN = function(x) { which.max(x) }  # R is 1-indexed
) - 1  # Adjust index to match label coding

# Calculate accuracy on test data
test_accuracy <- sum(y_test == predicted_labels) / length(y_test)
test_accuracy ## around 0.80

# Analyze Misclassifications ----------------------------------------------------

# Identify misclassified indices
misclassified_indices <- which(predicted_labels != y_test)
correct_indices <- setdiff(1:length(y_test), misclassified_indices)
length(misclassified_indices)  # Number of errors on the test data
length(correct_indices)

# Function to summarize and review wrong predictions
review_misclassification <- function(index) {
  print(paste0(
    "Model predicts: ", class_codes[predicted_labels[misclassified_indices[index]] + 1], "; ",
    "Actual: ", class_codes[y_test[misclassified_indices[index]] + 1]
  ))
  probabilities <- cbind(0:3, class_codes, round(predictions[misclassified_indices[index], ], 3))
  colnames(probabilities) <- c("Code", "Class", "Probability")
  print(probabilities)
  view_image(x_test, misclassified_indices[index])
}

# Review the second misclassified image
review_misclassification(2)

# Experiment with Different Optimizers ------------------------------------------

# Model 2 with LAMB optimizer
model2_lamb <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                input_shape = c(img_height, img_width, num_channels), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(4, 4)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 4, activation = "softmax")

# Display the model architecture
summary(model2_lamb)

# Compile the model with LAMB optimizer
model2_lamb %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_lamb(),
  metrics = c('accuracy')
)

# Train the model
history2_lamb <- model2_lamb %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 32,
  validation_split = 0.2
)

# Model 2 with Lion optimizer
model2_lion <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                input_shape = c(img_height, img_width, num_channels), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(4, 4)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 4, activation = "softmax")

# Display the model architecture
summary(model2_lion)

# Compile the model with Lion optimizer
model2_lion %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_lion(),
  metrics = c('accuracy')
)

# Train the model
history2_lion <- model2_lion %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 32,
  validation_split = 0.2
)

# Model 2 with Adam optimizer
model2_adam <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                input_shape = c(img_height, img_width, num_channels), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(4, 4)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.8) %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same",
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 4, activation = "softmax")

# Display the model architecture
summary(model2_adam)

# Compile the model with Adam optimizer
model2_adam %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Train the model
history2_adam <- model2_adam %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 32,
  validation_split = 0.2
)

# Compare Training Histories ----------------------------------------------------

# View training histories
history2_lamb
history2_lion
history2_adam

# Plot the training histories
plot(history2_lamb)
plot(history2_lion)
plot(history2_adam)

# Create "Results" folder if it doesn't exist
if (!dir.exists("Results")) {
  dir.create("Results")
}

png(filename="/Results/MRI_history_1.png")
plot(history1)
dev.off()

png(filename="/Results/MRI_history_2.png")
plot(history2)
dev.off()

png(filename="/Results/MRI_history_3.png")
plot(history3)
dev.off()

png(filename="/Results/MRI_history_lamb.png")
plot(history2_lamb)
dev.off()

png(filename="/Results/MRI_history_lion.png")
plot(history2_lion)
dev.off()

png(filename="/Results/MRI_history_adam.png")
plot(history2_adam)
dev.off()