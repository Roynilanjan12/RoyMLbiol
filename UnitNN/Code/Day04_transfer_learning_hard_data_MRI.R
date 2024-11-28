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

#### set directory
setwd("/Volumes/Crucial_X6/BIOL701_ML/RoyMLbiol/UnitNN")

# Define the path to the dataset
dataset_path <- "/Data/tumor_MRI_Data"

# Extract all the file names from each class directory
glioma_files <- list.files(path = paste0(data_path, "/glioma_tumor/"))
meningioma_files <- list.files(path = paste0(data_path, "/meningioma_tumor/"))
normal_files <- list.files(path = paste0(data_path, "/normal/"))
pituitary_files <- list.files(path = paste0(data_path, "/pituitary_tumor/"))

# Load Images as Tensors --------------------------------------------------------

num_images_per_class <- 200  # Number of images to load per class
img_height <- 256            # Image height
img_width <- 256             # Image width
num_channels <- 3            # Number of color channels (RGB)

# Initialize arrays to store images for each class
glioma_images <- array(NA, c(num_images_per_class, img_height, img_width, num_channels))
meningioma_images <- array(NA, c(num_images_per_class, img_height, img_width, num_channels))
normal_images <- array(NA, c(num_images_per_class, img_height, img_width, num_channels))
pituitary_images <- array(NA, c(num_images_per_class, img_height, img_width, num_channels))

# Load images for each class
for (i in 1:num_images_per_class) {
  # Load Glioma images
  glioma_images[i, , , ] <- readJPEG(source = paste0(data_path, "/glioma_tumor/", glioma_files[i]))
  
  # Load Meningioma images
  meningioma_images[i, , , ] <- readJPEG(source = paste0(data_path, "/meningioma_tumor/", meningioma_files[i]))
  
  # Load Normal images
  normal_images[i, , , ] <- readJPEG(source = paste0(data_path, "/normal/", normal_files[i]))
  
  # Load Pituitary images
  pituitary_images[i, , , ] <- readJPEG(source = paste0(data_path, "/pituitary_tumor/", pituitary_files[i]))
}

# Function to Facilitate Viewing Images -----------------------------------------

plot_image <- function(image_array, index) {
  img <- image_array[index, , , ]
  img <- aperm(img, c(2, 1, 3))  # Adjust dimensions for plotting
  img <- as.cimg(img)
  plot(img)
}

# Display the first image from each class
plot_image(glioma_images, 1)       # Display Glioma image
plot_image(meningioma_images, 1)   # Display Meningioma image
plot_image(normal_images, 1)       # Display Normal image
plot_image(pituitary_images, 1)    # Display Pituitary image

# Combine Data and Generate Labels ----------------------------------------------

# Combine all classes into a single array
total_images <- 4 * num_images_per_class  # Total number of images
all_images <- array(NA, c(total_images, img_height, img_width, num_channels))

# Assign images to the combined array
all_images[1:num_images_per_class, , , ] <- glioma_images
rm(glioma_images)  # Free up memory

all_images[(num_images_per_class + 1):(2 * num_images_per_class), , , ] <- meningioma_images
rm(meningioma_images)

all_images[(2 * num_images_per_class + 1):(3 * num_images_per_class), , , ] <- normal_images
rm(normal_images)

all_images[(3 * num_images_per_class + 1):(4 * num_images_per_class), , , ] <- pituitary_images
rm(pituitary_images)

# Generate labels for each class
# 0 = glioma, 1 = meningioma, 2 = normal, 3 = pituitary
labels <- c(
  rep(0, num_images_per_class),
  rep(1, num_images_per_class),
  rep(2, num_images_per_class),
  rep(3, num_images_per_class)
)

# Class codes for reference
codes <- c("glioma", "meningioma", "normal", "pituitary")

# Shuffle the Dataset -----------------------------------------------------------

set.seed(101)  # For reproducibility
indices <- sample(1:total_images, size = total_images, replace = FALSE)

# Shuffle the images and labels
all_images <- all_images[indices, , , ]
labels <- labels[indices]

# Preprocess Inputs -------------------------------------------------------------

# Check the range before scaling
range(all_images)  # Should be between 0 and 1

# Scale pixel values to be between -1 and 1 (required for Xception model)
all_images <- 2 * all_images - 1

# Split into Training and Test Sets ---------------------------------------------

test_split <- 0.2  # Percentage for testing
num_test <- round(test_split * length(labels))

# Split the data into training and testing sets
x_test <- all_images[1:num_test, , , ]
x_train <- all_images[(num_test + 1):length(labels), , , ]
y_test <- labels[1:num_test]
y_train <- labels[(num_test + 1):length(labels)]

# Check for balanced classes
table(y_train)
table(y_test)

# Remove unnecessary variables to free up memory
rm(all_images, labels)

# Build and Compile the CNN Model -----------------------------------------------

# Define the Convolutional Neural Network model
cnn_model <- keras_model_sequential() %>%
  # First convolutional layer
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = "same", 
                input_shape = c(img_height, img_width, num_channels), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(4, 4)) %>%
  layer_dropout(rate = 0.4) %>%
  # Second convolutional layer
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), padding = "same", activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.3) %>%
  # Third convolutional layer
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), padding = "same", activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(rate = 0.2) %>%
  # Flatten the output and add dense layers
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  # Output layer
  layer_dense(units = 4, activation = "softmax")

# Display the model architecture
summary(cnn_model)  

# Compile the CNN model
cnn_model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Train the CNN Model -----------------------------------------------------------

cnn_history <- cnn_model %>% fit(
  x_train, y_train,
  batch_size = 32,
  epochs = 10, 
  validation_split = 0.2
)

# View training history
cnn_history
plot(cnn_history)

# Build the Transfer Learning Model (Xception) ----------------------------------

# Load the pre-trained Xception model without the top layers
base_model <- application_xception(
  weights = "imagenet",    # Load weights pre-trained on ImageNet
  include_top = FALSE,     # Do not include the final output layer
  input_shape = c(img_height, img_width, num_channels)  # Adjust input shape
)

# Freeze the base model layers to prevent them from being updated during training
base_model$trainable <- FALSE

# Display the base model architecture
summary(base_model)

# Build the new model on top of the base model
transfer_model <- keras_model_sequential() %>%
  base_model %>%
  # Add global average pooling layer
  layer_global_average_pooling_2d() %>%
  # Add dropout for regularization
  layer_dropout(rate = 0.2) %>%
  # Add output layer
  layer_dense(units = 4, activation = "softmax")

# Display the new model architecture
summary(transfer_model)

# Compile the Transfer Learning Model -------------------------------------------

transfer_model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Train the Transfer Learning Model ---------------------------------------------

transfer_history <- transfer_model %>% fit(
  x_train, y_train,
  batch_size = 32,
  epochs = 6, 
  validation_split = 0.2
)

# View training history
transfer_history
plot(transfer_history)

# Evaluate the Model ------------------------------------------------------------

# Get predictions on the test set
predictions <- transfer_model %>% predict(x_test)

# Check the class of predictions
class(predictions)
dim(predictions)
predictions[1, ]  # Probabilities for the first test image

# Convert probabilities to class labels
predicted_classes <- apply(
  X = predictions, 
  MARGIN = 1, 
  FUN = function(x) { which.max(x) }
) - 1  # Adjust index to match label coding (R is 1-indexed)

# Calculate accuracy on the test set
accuracy <- sum(y_test == predicted_classes) / length(y_test)  # e.g., 98.33%
accuracy

# Analyze Misclassifications ----------------------------------------------------

# Identify misclassified indices
misclassified_indices <- which(predicted_classes != y_test)
correctly_classified_indices <- setdiff(1:length(y_test), misclassified_indices)
length(misclassified_indices)  # Number of errors on the test data
length(correctly_classified_indices)

# Function to summarize and review wrong predictions
review_misclassification <- function(index) {
  cat("Model predicts:", codes[predicted_classes[misclassified_indices[index]] + 1], "; ",
      "Actual:", codes[y_test[misclassified_indices[index]] + 1], "\n")
  probabilities_table <- cbind(0:3, codes, round(predictions[misclassified_indices[index], ], 3))
  colnames(probabilities_table) <- c("Code", "Class", "Probability")
  print(probabilities_table)
  plot_image(x_test, misclassified_indices[index])
}

# Review the third misclassified image
review_misclassification(3)


# Create the Results directory if it doesn't exist
if (!dir.exists("Results")) {
  dir.create("Results")
}

png(filename="/Results/MRI_history_CNN_base_model_Day04.png")
plot(cnn_history)
dev.off()

png(filename="/Results/MRI_history_transfer_model_Day04.png")
plot(transfer_history)
dev.off()
