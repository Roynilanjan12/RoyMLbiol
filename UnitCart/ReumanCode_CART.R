#Basic CART exercise for Wisconsin breast cancer dataset, in support of the CART
#unit of Biol 701: Topics In: Machine Learning in Biology, University of Kansas,
#Fall 2024.
#
#I use a version of the Wisconsin breast cancer dataset downloaded from here 
#https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data?resource=download
#and then with some of the features stripped out for simplicity. 

#**basic setup

rm(list=ls())
graphics.off()

library(rpart)

#**load the data and simplify

d<-read.csv(file="BreastCancerData.csv") #assumes the file stored in the working directory
head(d)
dim(d)
d<-d[,2:12] #keep only the "mean" columns
head(d)
unique(d$diagnosis)
d$diagnosis<-as.factor(d$diagnosis)
names(d)<-c("diagnosis","radius","texture","perimeter","area","smoothness",
            "compactness","concavity","concave_points","symmetry","fractal_dimension")
head(d) #renamed for simplicity

#**split the data into validation and testing sets

#split
set.seed(101)
d_perm<-d[sample(dim(d)[1],dim(d)[1],replace=FALSE),]
d_val<-d_perm[1:floor(.75*(dim(d)[1])),]
d_test<-d_perm[(floor(.75*(dim(d)[1]))+1):(dim(d)[1]),]

#some consistency checks
dim(d)
dim(d_perm)
dim(d_val)
dim(d_test)
dim(d_val)[1]+dim(d_test)[1]

#**do a CART using some "pre-pruning" (default)

#fit a CART
m_d<-rpart(diagnosis~.,
           data=d_val,method="class")

#examine it
m_d #pretty simple
plot(m_d,uniform=TRUE,margin=0.1)
text(m_d,use.n=TRUE,all=TRUE,cex=0.8) 

#save as pdf
pdf(file="RTree_m_d.pdf")
plot(m_d,uniform=TRUE,margin=0.1)
text(m_d,use.n=TRUE,all=TRUE,cex=2) 
dev.off()

#see the errors it makes on the training data
m_d_pred<-predict(m_d,type="class")
table(m_d_pred,d_val$diagnosis)
sum(m_d_pred!=d_val$diagnosis)/dim(d_val)[1] #Fraction of mis-classifications
#on the validation data is already low. Though these are errors on the validation 
#set, so expected to be lower than would be the case on an out-of-sample set.

#**Now do a full CART, all the way down (i.e., no stopping criteria), to work 
#toward being able to make points about bias-variance cross validation, overfitting, 
#etc.

#fit it - setting cp=0 and minsplit=1 means the algorithm does not stop until impurity 
#is 0, even if it's necessary to sometimes go all the way to leaf nodes which have only
#one element to do that
m_f<-rpart(diagnosis~.,
           data=d_val,method="class",
           control=rpart.control(cp=0,minsplit=1)) 

#examine it
m_f
plot(m_f,uniform=TRUE,margin=0.1) #it is huge!
text(m_f,use.n=TRUE,all=TRUE,cex=0.8) 

#see the errors it makes on the training data
m_f_pred<-predict(m_f,type="class")
table(m_f_pred,d_val$diagnosis) #so it is perfect, as it should be given how we 
#constructed it, on the validation data set at least

#**make some plot versus first couple splits for both models (they are the same on the
#first couple splits)

plot(d_val$concave_points[d_val$diagnosis=="M"],d_val$texture[d_val$diagnosis=="M"],
     type="p",pch=20,xlab="Concave points",ylab="Texture",
     xlim=range(d_val$concave_points),ylim=range(d_val$texture),cex=.5,col="red")
points(d_val$concave_points[d_val$diagnosis=="B"],d_val$texture[d_val$diagnosis=="B"],
       pch=4,col="green",cex=.5)
lines(rep(0.05592,2),c(0,100),type="l")

#**now do a manual cross validation exercise for each of the two above trees

numgp<-10 #number of folds in k-fold cross validation
gp<-rep(1:numgp,length.out=dim(d_val)[1])
xerrs_d<-NA*numeric(numgp)
xerrs_f<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  #fit the models on all of the data excluding one group
  m_f_s<-rpart(diagnosis~.,
               data=d_val[gp!=counter,],method="class",control=rpart.control(cp=0,minsplit=1))
  m_d_s<-rpart(diagnosis~.,
               data=d_val[gp!=counter,],method="class")
  
  #get predictions for the left out group and get error rates
  pred_f<-predict(m_f_s,d_val[gp==counter,],type="class")
  pred_d<-predict(m_d_s,d_val[gp==counter,],type="class")
  xerrs_f[counter]<-sum(pred_f!=d_val$diagnosis[gp==counter])/sum(gp==counter)
  xerrs_d[counter]<-sum(pred_d!=d_val$diagnosis[gp==counter])/sum(gp==counter)
}
mean(xerrs_f)
mean(xerrs_d) #Note the x-val error rate is lower for the pre-pruned model, even though 
#the error rate on the validation dataset was lower for the full model. The full model
#was overfitted! You don't get any actual benefit from the extra complexity of the more 
#complex model. Note also these values are both higher than the error rates on the validation 
#set, which is what you'd expect. 

#**Automated procedure of post-pruning based on cross validation, illustrates tools provided 
#by rpart

plotcp(m_f) 
printcp(m_f) #xerror plotted and printed here is a risk-based thing, not the same as
#the classification error rate computed above in the manual x-val exercise. But should 
#be better for judging what sub-tree to use.

#save a plot for the slide show
pdf(file="ComplexityParameterPlot.pdf")
plotcp(m_f)
dev.off()

plotcp(m_d) 
printcp(m_d) 

#These two analyses suggest different optimal tree complexities. However, the xerror for a 
#tree with 5 splits (size 6) obtained through the m_f route was 
m_f$cptable[5,4]
#with that value plus SE being 
m_f$cptable[5,4]+m_f$cptable[5,5]
#which was less than the xerror for a tree with one split (size 2) from the m_d route
m_d$cptable[2,4]
#so I go with m_f option with 5 splits.

#**let's get manual x-val for that tree

m_f_5<-rpart(diagnosis~.,
           data=d_val,method="class",control=rpart.control(cp=0.014,minsplit=1))
m_f_5
m_f
plot(m_f_5,uniform=TRUE,margin=0.1) 
text(m_f_5,use.n=TRUE,all=TRUE,cex=0.8) #this is a pretty interpretable tree!

plotcp(m_f_5)
plotcp(m_f) #pretty similar (remember these are stochastic, they depend on the random splitting of the data)

xerrs_5<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  #fit the models on all of the data excluding one group
  m_5_s<-rpart(diagnosis~.,
               data=d_val[gp!=counter,],method="class",control=rpart.control(cp=0.014,minsplit=1))

  #get predictions for the left-out group and get error rates
  pred_5<-predict(m_5_s,d_val[gp==counter,],type="class")
  xerrs_5[counter]<-sum(pred_5!=d_val$diagnosis[gp==counter])/sum(gp==counter)
}
mean(xerrs_f)
mean(xerrs_d) 
mean(xerrs_5) #So this new model beats both m_d and m_f

#**now we do bagging and see if we can beat m_f_5

library(ipred)

bagres<-bagging(diagnosis~.,
                data=d_val,nbagg=500,coob=TRUE,method="class",
                control=rpart.control(cp=0,minsplit=1,xval=0),aggregation="majority")
#Here, minsplit is 1 and cp=0, because we want to use full trees for each bag - no 
#pruning because we are using bagging instead to reduce variance

bagres #the oob misclassification error 
bagres$err #to be more precise
b_pred<-predict(bagres,type="class",aggregation="majority") 
sum(b_pred!=d_val$diagnosis)/dim(d_val)[1] #Should be similar to the oob 
#error above. A bit confusingly, when new data are not provided to 
#``predict'', predictions are supposed to be based on a plurality vote 
#of out-of-bag predictions. See help file predict.classbagg, which is 
#what is run when you call predict on bagres since bagres has class 
#classbagg. When you call predict with new data, oob does not make
#sense and all trees are used. 

#**let's do x-val on the bagging result, in a comparable way to the manual x-val done above

xerrs_b<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  #fit the models on all of the data excluding one group
  bagres_s<-bagging(diagnosis~., #again, the ipred:: prefix is just to make sure we are using the bagging routine from the ipred package
                    data=d_val[gp!=counter,],nbagg=500,coob=FALSE,method="class",
                    control=rpart.control(cp=0,minsplit=1,xval=0),aggregation="majority")
  
  #get predictions for the left out group and get error rates
  pred_b_s<-predict(bagres_s,d_val[gp==counter,],type="class",aggregation="majority")
  xerrs_b[counter]<-sum(pred_b_s!=d_val$diagnosis[gp==counter])/sum(gp==counter)
}
mean(xerrs_f)
mean(xerrs_d)
mean(xerrs_5)
mean(xerrs_b) 
#So the bagging is comparable to our pruned result, though it only
#uses fully built trees.
#Note also the oob misclassification error above is similar, but not 
#the same, as the x-val error we just computed. That's known - it is
#supposed to be asymptotically equal to some kinds of x-val error,
#but may differ, in a biased way, in real situations.

#Note that bagged decision trees are no longer as interpretable as pruned ones,
#though! Basically, bagging can reduce the variance at the expense of 
#interpretability and computational speed. Can be worth doing. The interpretability
#thing can be remedied (see 10.5 from B&G)

#**Now do random forests

library(randomForest)

m_rf<-randomForest(diagnosis~.,
                   data=d_val,ntree=1000)

m_rf #includes oob error rate
names(m_rf) 
tail(m_rf$err.rate) #last entry of column 1 is the same as the oob error rate above

plot(m_rf) #You can also see as the number of trees increases the accuracy increases and then
#levels off 
dim(m_rf$err.rate)[1] #corresponds to the number of trees

rf_pred<-predict(m_rf)
sum(m_rf$predicted==rf_pred)
length(rf_pred) #ok so predict is just pulling a slot that already is in m_rf
sum(rf_pred!=d_val$diagnosis)/dim(d_val)[1] #agrees with the oob error rate above.

#**let's do x-val on the rf result, in a comparable way to the manual x-val done above

xerrs_rf<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  #fit the models on all of the data excluding one group
  rfres_s<-randomForest(diagnosis~.,
                        data=d_val[gp!=counter,],ntree=1000)
  
  #get predictions for the left out group and get error rates
  pred_rf_s<-predict(rfres_s,d_val[gp==counter,],type="class")
  xerrs_rf[counter]<-sum(pred_rf_s!=d_val$diagnosis[gp==counter])/sum(gp==counter)
}
mean(xerrs_f)
mean(xerrs_d)
mean(xerrs_b) 
mean(xerrs_rf) 
#Comparable to bagging. Technically the best model (by a tiny bit) is the random
#forest model. 

#**Now apply adaptive boosting

library(adabag)

m_ada<-boosting(diagnosis~.,data=d_val) #This also takes an rpart.control 
#type argument if you want to to control the nature of the base learners.
#This uses the default of 100 boosting steps.

ada_pred<-predict(m_ada,d_val[,2:dim(d_val)[2]])$class #note the prediction output gives 
#more detail, including information on certainty
sum(ada_pred!=d_val$diagnosis)/dim(d_val)[1] #perfect, in sample

m_ada$importance #gives the relative importance of the different variables for
#making the final model predictions

#**let's see how adaptive boosting does on Xval

xerrs_ada<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  print(paste(counter,"of",numgp))
  
  #fit the models on all of the data excluding one group
  adares_s<-boosting(diagnosis~.,data=d_val[gp!=counter,])
  
  #get predictions for the left out group and get error rates
  pred_ada_s<-predict(adares_s,d_val[gp==counter,],type="class")$class
  xerrs_ada[counter]<-sum(pred_ada_s!=d_val$diagnosis[gp==counter])/sum(gp==counter)
}
mean(xerrs_f)
mean(xerrs_d)
mean(xerrs_5)
mean(xerrs_b) 
mean(xerrs_rf) 
mean(xerrs_ada) #best so far

#**Now apply extreme gradient boosting

library(xgboost)

#put the data in the specific form expected by the xgb function
X_val<-as.matrix(d_val[,2:(dim(d_val)[2])])
y_val<-as.integer(d_val[,1])-1

m_xgb<-xgboost(data=X_val,
           label=y_val,
           max_depth=6,eta=.3,
           nthread=2,nrounds=20,
           objective="binary:logistic",verbose=2)

#get error rate on the validation data
pred_xgb<-predict(m_xgb,X_val) #gives probabilities 
#of being in the first class, which is additionally useful, tho 
#we don't use it
predictions<-rep("B",length(pred_xgb))
predictions[pred_xgb>.5]<-"M"
sum(predictions!=as.character(d_val$diagnosis))/dim(d_val)[1] 

#**Now do X val error rate for extreme gradient boosting

xerrs_xgb<-NA*numeric(numgp)
for (counter in 1:numgp)
{
  #status
  print(paste(counter,"of",numgp))
  
  #fit the models on all of the data excluding one group
  m_xgb_s<-xgboost(data=X_val[gp!=counter,],
                 label=y_val[gp!=counter],
                 max_depth=7,eta=.2,subsample=0.5,nrounds=55,
                 nthread=2,objective="binary:logistic",verbose=0)
  
  #get predictions for the left out group and get error rates
  pred_xgb_s<-predict(m_xgb_s,X_val[gp==counter,])
  predictions_s<-rep("B",length(pred_xgb_s))
  predictions_s[pred_xgb_s>.5]<-"M"
  xerrs_xgb[counter]<-sum(predictions_s!=
                            d_val$diagnosis[gp==counter])/sum(gp==counter)
}
mean(xerrs_f)
mean(xerrs_d)
mean(xerrs_5)
mean(xerrs_b) 
mean(xerrs_rf) 
mean(xerrs_ada)
mean(xerrs_xgb)

importance_matrix<-xgb.importance(model=m_xgb)
importance_matrix #You can get information on the importance of
#the different factors from bagging and boosting

#**Now apply THAT ONE MODEL ONLY on the test data and see prediction error

testpred_ada<-predict(m_ada,d_test[,2:11],type="class")$class
sum(testpred_ada!=d_test$diagnosis)/dim(d_test)[1]
#So actually the predictions on the test data were slightly better than on 
#x-val error rate, which is lucky (often it's a bit worse). But at the end
#of the day we have a very good classifier! 




