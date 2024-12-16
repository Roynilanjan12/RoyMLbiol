#DAN:
#You need a readme
#You need file headers
#Your code is very well organized, aside from the lack of headers/readme
#Nice work trying a non-standard dataset
#Your three CNNs appear somewhat overfitted given the discrepancy between performance on the training and
#validation sets. You have have explored additional ways to mitigate that. But using only 200 images per
#set may be at the root of it - overfitting is easier when you have a smaller dataset. 
#The accuracy *declines* with epochs in the first set of models (dense and dropout layers), which is weird and
#makes me wonder if there is a bug. Or it may relate to using so few images. Not sure but needs investigating. 
#The CNNs did a lot better though, so that's good.
#What your laptop unable to accommodate more than 200 images per set? If you wanted to do this problem for 
#real, you should get on the cluster. Ask Luis for a 30-minute intro on how to do that.
#Overall solid work. Grade: S+

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