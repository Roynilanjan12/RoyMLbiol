library(tidyverse)

###loading and inspecting the dataframe
data<-read.csv(file="/Volumes/Crucial_X6/BIOL701_ML/RoyMLbiol/UnitCart/breast_cancer_data.csv")
head(data)
dim(data)

##see if there is any NA values
colSums(is.na(data)) #### in this breast cancer data all the 'X' column data is NAs


###remove the unnecessary columns (removing the standard error (se) and '_worst' columns)
data$id <- NULL
data<-data[,1:11]

### checking the classes of the dataframe
str(data)
### making diagnosis column as factor
data$diagnosis <- as.factor(data$diagnosis)

### saving the simplified breast cancer data
setwd("/Volumes/Crucial_X6/BIOL701_ML/RoyMLbiol/UnitCart")
write.csv(data, "simplified_breast_cancer_data.csv", row.names = FALSE, quote = F)

