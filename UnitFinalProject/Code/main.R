# main.R
####set directory
## assuming that the working directory is set as the project directory
####Machine learning model
source("./Code/drosophila_AMPs_prediction.R")
####Concordance with experimental data of the predicted AMPs from the random forest model
source("./Code/experimental_concordance_with_predicted_AMPs.R")
