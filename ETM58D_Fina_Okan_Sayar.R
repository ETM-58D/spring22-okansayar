library(tree)
library(rpart)
library(data.table)
library(caret)
library(lubridate)
library(rattle)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)

#First Data
bank=read.csv("/Users/okansayar/Desktop/R/UniversalBank.csv")
bank=data.table(bank)
str(bank)
summary(bank)

# obtain classification data
bank$Personal.Loan=as.factor(bank$Personal.Loan)
banka_agaci=tree(Personal.Loan~.,data=bank)
plot(banka_agaci)
text(banka_agaci,cex=0.8)
banka_agaci

#dive into the two data set (0.8 trainin 0.2 test samples)
split=sample.split(bank$Personal.Loan, SplitRatio = 0.8)
datatr=subset(bank,split==TRUE)
datate=subset(bank,split==FALSE)

#make tree
banka_agaci2=tree(Personal.Loan~.,data=datatr)
plot(banka_agaci2)
text(banka_agaci2,cex=0.8)

#Decision Tree
ctrlbank=trainControl(method="cv",number=10)
fitbank=train(Personal.Loan~.,data=bank, method = "rpart",
           trControl = ctrlbank)
fitbank
names(fitbank)
#prediction
fit1pred=predict(fitbank)
confusionMatrix(fit1pred,bank$Personal.Loan,positive = "1")

# Random Forest
dim(bank)

fitbank2=randomForest(Personal.Loan~ ., data=datatr)

fitbank2
plot(fitbank2)

fit2pred=predict(fitbank2)
confusionMatrix(fit1pred,bank$Personal.Loan,positive = "1")


#Second Data
trip=read.csv("/Users/okansayar/Desktop/R/RStudio Directory/Final ETM58D/non-verbal tourist data.csv",stringsAsFactors=T)
data=data.table(trip)

str(trip)
summary(trip)

trip$Hostile...friendly=as.factor(trip$Hostile...friendly)

#dive into the two data set (0.8 trainin 0.2 test samples)
split2=sample.split(trip$Hostile...friendly, SplitRatio = 0.8)

datatr=subset(trip,split2==TRUE)
datate=subset(trip,split2==FALSE)


set.seed(580)
#train data set with method cv
ctrl1=trainControl(method="cv",number=10)
fittree=rpart(Hostile...friendly~ ., data = datate,control=rpart.control(maxdepth=3))

fittree
names(fittree)

library(caret)
n_repeats=10
n_folds=10


fitControl=trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5,
                        )    
#The data set variables have at least 2 level, so that´s why variables that have one level removed form dataset
trip=trip[,c(-9)]

tree_fit=train(Hostile...friendly~ ., data=data.frame(trip),
               method = "rf",
               trControl = fitControl)
                
tree_fit   
trellis.par.set(caretTheme())
plot(tree_fit)  


#random forest method
library(randomForest)
target=as.factor(trip$Hostile...friendly)
fitrf=randomForest(trip,target,ntree=1000)
fitrf
plot(fitrf)
varImpPlot(fitrf)


rf_fit=train(Hostile...friendly ~ ., data = data.frame(trip), 
             metric= "Accuracy", trControl = fitControl, tuneGrid = NULL)
rf_fit
plot(rf_fit)

rf_fit$finalModel$variable.importance

results = resamples(list(dtree=tree_fit,rf=rf_fit),metrics='Accuracy')
summary(results)
bwplot(results)
densityplot(results)


#Third Data

d3=read.table("/Users/okansayar/Desktop/R/RStudio Directory/Final ETM58D/student/student-mat.csv",sep=";",header=TRUE)
summary(d3)
str(d3)
data2=data.table(d3)

#dive into trainin and test samples of data
split3=sample.split(d3$G2, SplitRatio = 0.8)

datatr2=subset(d3,split3==TRUE)
datate2=subset(d3,split3==FALSE)


ctrl2=trainControl(method="cv",number=10)
fittreestd=rpart(G2~ ., data = datate2,control=rpart.control(maxdepth=3))

fittreestd
names(fittreestd)

library(caret)
n_repeats=10
n_folds=10


fitControl2=trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5,
)    

## random forest with ranger
tree_fit2=train(G2~ ., data=data.frame(d3),
               method = "rf",
               trControl = fitControl
               )

tree_fit2   
trellis.par.set(caretTheme())
plot(tree_fit2)  

tree_fit2$finalModel$variable.importance

library(randomForest)
target=as.factor(d3$G2)
fitrf2=randomForest(d3,target,ntree=1000)
fitrf2
plot(fitrf2)
varImpPlot(fitrf2)


rf_fitstd=train(G2~., data = data.frame(d3), 
             metric= "RMSE", trControl = fitControl, tuneGrid = NULL)
rf_fitstd
plot(rf_fitstd)


results2 = resamples(list(dtree=tree_fit2,rf=rf_fitstd))
summary(results2)
bwplot(results2)
densityplot(results2)

#Findings & Results
#when mtry increases all prediction accuracy increase
#Classification performance is best described by the confusion matrix which ı used for the data set 1. 
#It makes more accurate predictions for the training data sets.