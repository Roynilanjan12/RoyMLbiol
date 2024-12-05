#DAN: Overall very good, all the elements are here and its all so simple, too, which helps.
#Code pretty much worked aside from a couple small things. 
#You could use a readme, tho
#Grade: S+

# main.R

#DAN: Nice that you made this just source commands

####set directory
setwd("/Volumes/Crucial_X6/BIOL701_ML/RoyMLbiol/UnitWorkflow")
#DAN: This ine does not ork on my machine. Use the "here" package or relative addressing 
#after telling the user in a readme (you need one) where to put the working directory of R

####Data Cleaning
source("Code/data_cleaning.R")
####Data analysis
source("Code/analysis.R")
#DAN: The code ran except for some warnings
