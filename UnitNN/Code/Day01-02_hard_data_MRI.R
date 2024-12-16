# Load Necessary Libraries ------------------------------------------------------

# Load the tidyverse for data manipulation and visualization
library(tidyverse)

# Install and load the keras3 package for building neural networks
#install.packages("keras3") #DAN: Thank you for commenting the install commands
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

#DAN: Nice code comments and sections and so on.

# Data Preparation --------------------------------------------------------------

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

num_images <- 200   # Number of images to load per class
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
image_data[(3 * num_images + 1):(4 * num_images), , , ] <- pituitary_array
rm(normal_array)
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

# Scale pixel values to be between -1 and 1 (as required by Xception model)
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

# Reshape Data for Feedforward Neural Network -----------------------------------

# Store original x_train and x_test for reference
x_train_original <- x_train
x_test_original <- x_test

# Flatten images to vectors
dim(x_train) <- c(dim(x_train)[1], img_height * img_width * num_channels)
dim(x_test) <- c(dim(x_test)[1], img_height * img_width * num_channels)

# Build and Compile the Models --------------------------------------------------

# Model 1: Simple Feedforward Neural Network
model1 <- keras_model_sequential() %>% 
  
  # First dense layer with 64 units and ReLU activation
  layer_dense(units = 64, activation = 'relu', input_shape = c(196608)) %>% 
  
  # Dropout layer to prevent overfitting (20% dropout rate)
  layer_dropout(rate = 0.2) %>% 
  
  # Output layer with 4 units (one for each class) and softmax activation
  layer_dense(units = 4, activation = 'softmax')

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
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)

#DAN: According to your write up, the accuracy of this model *declines* as you do more epochs,
#both on the training and validation datasets simultaneously. Overfitting could lead to a 
#decline in accuracy on the validation dataset, but I can't see how anything other than a 
#bug could cause a decline on the training data. The weird thing is, the loss function is
#simultaneously getting better. How could that be? All of that amount to a red flag, makes 
#me suspect some sort of bug. 

# View training history
history1
plot(history1)

# Build Alternative Models ------------------------------------------------------

# Model 2: Adding Another Dense Layer and Dropout
model2 <- keras_model_sequential() %>% 
  
  # First dense layer with 64 units and ReLU activation
  layer_dense(units = 64, activation = 'relu', input_shape = c(196608)) %>% 
  
  # Dropout layer with 60% dropout rate
  layer_dropout(rate = 0.6) %>% 
  
  # Second dense layer with 32 units and ReLU activation
  layer_dense(units = 32, activation = 'relu') %>%
  
  # Dropout layer with 40% dropout rate
  layer_dropout(rate = 0.4) %>%
  
  # Output layer with 4 units and softmax activation
  layer_dense(units = 4, activation = 'softmax')

# Display the model architecture
summary(model2)

# Model 3: Adjusting Sizes and Dropout Rates
model3 <- keras_model_sequential() %>% 
  
  # First dense layer with 128 units and ReLU activation
  layer_dense(units = 128, activation = 'relu', input_shape = c(196608)) %>% 
  
  # Dropout layer with 50% dropout rate
  layer_dropout(rate = 0.5) %>% 
  
  # Second dense layer with 64 units and ReLU activation
  layer_dense(units = 64, activation = 'relu') %>%
  
  # Dropout layer with 30% dropout rate
  layer_dropout(rate = 0.3) %>%
  
  # Output layer with 4 units and softmax activation
  layer_dense(units = 4, activation = 'softmax')

# Display the model architecture
summary(model3)

# Compile both models
model2 %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

model3 %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train Alternative Models ------------------------------------------------------

history2 <- model2 %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

history3 <- model3 %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

# Evaluate and Compare Models ---------------------------------------------------

# View training histories
history1
history2
history3

# Plot the training histories
plot(history1)
plot(history2)
plot(history3)
## all of these models are horrible. So did not bother to check with the misclassification function.
### Tried the misclassification function after using CNN in day 3's code.

# Create the Results directory if it doesn't exist
if (!dir.exists("Results")) {
  dir.create("Results")
}

png(filename="/Results/MRI_history1_Day01_02.png")
plot(history1)
dev.off()

png(filename="/Results/MRI_history2_Day01_02.png")
plot(history2)
dev.off()

png(filename="/Results/MRI_history3_Day01_02.png")
plot(history3)
dev.off()
