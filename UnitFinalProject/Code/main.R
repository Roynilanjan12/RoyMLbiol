#DAN:
#1) This is a really nice project. You asked me to think like a reviewer, so I will kinda do that below.
#2) HOWEVER, I will not focus on aspects of the writing of the report, becuase you and I can both see that the
#report is not yet polished for actual submission. So I'll forcus on the content instead of the writing.
#3) The content seems really good as far as I can tell. I checked your code in drosophila_AMPs_prediction.R
#pretty carefully and I re-ran much of it on my machine. I did not re-run the other models besides the 
#random forest model. You can see changes that I made to the code using gitk, but nothing of real substance.
#I agree you seem to have a very successful classification method. 
#4) Caveat: I don't know anything about AMPs and I am not even a geneticist. So I have checked if your
#ML model is correctly implemented and I have evalualted whether your dataset seems appropriately constructed
#and it seems to me it is OK. 
#5) So I encourage you to consider publishing this. Here are the steps I would undertake if I were you:
#a) Get someone who is expert in AMPs and is a geneticist to read it and comment and potentially become a
#co-author. This could possible be your supervisor. That person can also help you write the paper in a formal
#format and communciate best with the intended audience. As far as I can tell, the underlying analysis is pretty 
#solid, and what you need is to focus on the writing.
#b) You predict that 12 genes are AMPs. How hard would it be to test that experimentally, now that those
#genes are identified? You may consider doing that. Even testing a few of them would be cool of you have
#a not-too-difficult method of experimentally verifying for sure one way or the other whether these are
#AMPs.
#c) Possible point for the discussion: What other species would it be useful to know AMPs? As far as I can 
#tell, you could straightforwardly do the same thing for another species, especially another insect species.
#Basically you used insect AMPs, so I woul expect the same method to work at least for any insect. Do
#you have any other species you want to apply this to? 
#d) I suggest you take another look at the two drosophila AMPs that were incorrectly predicted to not be AMPs.
#What went wrong there? You can look at the vote across trees for those AMPs. If only 51% of the trees in
#the random forest classified those as non-AMPs, then the model was not very certain in its classification,
#which means you can kind of "forgive" the model for being wrong in those cases. How certain was it for the
#other 23 that it got right?
#e) You can use the same voting-based measure of certainty for the predicted 930 drosophila AMPs. Probably 
#you want to focus on the subset of those for which the model was relatively certain they were AMPs. So, 
#in addition to screening from those 930 to only those which are differentially expressed in studies of
#bacterial infection, you may also want to screen for those the model is relatively certain of.
#f) I also wonder about the screening for those differentially expressed in studies of bacterial 
#expression, what if a gene gets differentially expressed under infection by bacterium A, but not under
#infection by bacterium B, and the experiments which have been done use only bacterium B? If that can 
#happen, there are so many types of bacteria, then I wonder whether screening for those differentially
#expressed in the bacterial infection studies that happen to have been done actually makes sense? Maybe
#you are actually missing a lot of AMPs that the model correctly identified but that are differentially
#expressed under other, non-tested types of bacterial infection? Could that happen? If so, then it
#may be worth listing genes about which the random forest was relatively certain they were amps, even if
#they are not listed as differentially expressed in bacterial infection. 
#
#Great job overall. I'd love to see the paper once it gets published! I'll save it and make future iterations
#of this class read it.
#Grade: 30/30


# main.R
####set directory
## assuming that the working directory is set as the project directory
####Machine learning model
source("./Code/drosophila_AMPs_prediction.R")
####Concordance with experimental data of the predicted AMPs from the random forest model
source("./Code/experimental_concordance_with_predicted_AMPs.R")
