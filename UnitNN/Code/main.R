# main.R

####set directory
setwd("/Volumes/Crucial_X6/BIOL701_ML/RoyMLbiol/UnitNN")

####Easy data, trying out different NN models
source("Code/Day01-02_easy_data.R")
####Hard data (MRI-tumor classification), trying out different NN models
source("Code/Day01-02_hard_data_MRI.R")
####CNN and different optimizer
source("Code/Day03_CNN_optimizer_hard_data.R")
####Data analysis
source("Code/Day04_transfer_learning_hard_data_MRI.R")