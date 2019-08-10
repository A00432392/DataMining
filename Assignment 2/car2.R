# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("pROC")
# install.packages('caTools')
# install.packages("randomForest")
# install.packages("caret")
# install.packages("e1071")

setwd("D:/r-workspace/Assignments/Assignment2")

carData=read.csv("car.csv")
summary(carData)
# View(carData)

x=carData[,1:6]
y=carData[,7]
#View(x)
#View(y)

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(carData$shouldBuy, SplitRatio = 0.75)
training_set = subset(carData, split == TRUE)
test_set = subset(carData, split == FALSE)

#-------------------------------

# Decision Tree

#Deciding on the Best Min Split using Training Set

library(rpart)

min_range = 250
max_range = 350
skip = 10

#Defining Min Split Range
min_split_list <- c(seq(min_range, max_range, 10)) 
accuracy_list <- c()

for (i in min_split_list){
  #Model
  treeCar = rpart(shouldBuy~.,data=training_set,method="class",control=rpart.control(minsplit=i))
  #Pridiction
  predCar=predict(treeCar,training_set,type="class")
  #Matrix
  treeCM=table(training_set[,7],predCar)
  #Accuracy
  accuracy = sum(diag(treeCM))/sum(treeCM)
  accuracy_list <- c(accuracy_list,accuracy)
}

#Plotting the Accuracy of The Training Dataset for Different Min Splits  
plot(y=accuracy_list,x=min_split_list,
     main = paste(c("Decision Tree - Accuracy of Training Dataset for Different Min Splits (",min_range,"-",max_range,")"), collapse = ""),
     ylab = "Accuracy",
     xlab = "Min Split Value")


#Displaying the Accuracy of The Training Dataset for Different Min Splits 
min_split_list
accuracy_list


#-------------------------------

#Apply the Best Min Split and Predicting Accuracy using Test Set

library(rpart)
library(rpart.plot)

msplit = 140

#Model
treeCar = rpart(shouldBuy~.,data=training_set,method="class",control=rpart.control(minsplit=msplit))
# Plot Decision Tree
rpart.plot(treeCar, box.palette="RdBu", shadow.col="gray", 
           main=paste(c("Decision Tree - Min Split:", msplit), collapse = ""), nn=TRUE)
#Pridiction
predCar=predict(treeCar,test_set,type="class")
#Matrix
treeCM=table(test_set[,7],predCar)
treeCM
#Accuracy of the prediction using Decision Tree
accuracy=sum(diag(treeCM))/sum(treeCM)
accuracy

#-------------------------------

#Kfold with rpart - Decision Tree

library(caret)
library(e1071)

msplit = 140

x=carData[,1:6]
y=carData[,7]

control <- trainControl(method="cv", number=10, savePredictions=TRUE)
seed <- 123
metric <- "Accuracy"
set.seed(seed)
rf_default <- train(x,y, method="rpart", metric=metric, control = rpart.control(minsplit = msplit), 
                    trControl=control)
print(rf_default)
varImp(rf_default)
ggplot(varImp(rf_default))

#--------------------------------------------------------------------------------------------------------


setwd("D:/r-workspace/Assignments/Assignment2")

carData=read.csv("car.csv")
summary(carData)
# View(carData)

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(carData$shouldBuy, SplitRatio = 0.75)
training_set = subset(carData, split == TRUE)
test_set = subset(carData, split == FALSE)

x=training_set[,1:6]
y=training_set[,7]


#--------------------------------------------------------------------------------------------------------

# Random Forest

# Deciding on the Best nTree using Training Set

library(randomForest)

min_range = 100
max_range = 1000
skip = 100

#Defining nTree Range
ntree_list <- c(seq(min_range, max_range, skip)) 
accuracy_list <- c()

for (i in ntree_list){
  rf=randomForest(x,y, ntree=i)
  rfp=predict(rf,training_set)
  rfCM=table(rfp,training_set[,7])
  # rfCM
  #Accuracy of the prediction using randomForest
  accuracy = sum(diag(rfCM))/sum(rfCM)
  accuracy_list <- c(accuracy_list,accuracy)
}


#Plotting the Accuracy of The Training Dataset for Different Min Splits  
plot(y=accuracy_list,x=ntree_list,
     main = paste(c("Randon Forest - Accuracy of Training Dataset for Different nTree (",min_range,"-",max_range,")"), collapse = ""),
     ylab = "Accuracy",
     xlab = "nTree Value")

#Displaying the Accuracy of The Training Dataset for Different nTree 
ntree_list
accuracy_list


#--------------------------------------------------------------------------------------------------------

#Apply the Best nTree and Predicting Accuracy using Test Set

library(randomForest)

best_ntree = 500

#Model
rf=randomForest(x,y, ntree=best_ntree)
#Pridiction
rfp=predict(rf,test_set)
#Matrix
rfCM=table(rfp,test_set[,7])
rfCM
#Accuracy of the prediction using Random Forest
accuracy = sum(diag(rfCM))/sum(rfCM)
accuracy

#--------------------------------------------------------------------------------------------------------

x=carData[,1:6]
y=carData[,7]

# Use caret package for 10 fold cross validation
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 123
metric <- "Accuracy"
set.seed(seed)
rf_default <- train(x,y, method="rf", ntree = 500, metric=metric, trControl=control)
print(rf_default)
varImp(rf_default)
ggplot(varImp(rf_default))


#--------------------------------------------------------------------------------------------------------


# library(pROC)
# predCarProb=predict(treeCar,newdata=carData,type="prob")
# roc(carData[,7],predCarProb[,2]) # what is this 2 ? *****
# plot(roc(carData[,7],predCarProb[,2]))
# 


rf=randomForest(x,y, ntree=500)

rfProb=predict(rf,x,type="prob")
roc.multi = multiclass.roc(carData[,7],rfProb)
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]], rfProb)
sapply(2:length(rs),function(i) lines.roc(rs[[1]],col=1))

library(caret)
library(e1071)
control <- trainControl(method="cv", number=10, savePredictions=TRUE)
seed <- 123
metric <- "Accuracy"
set.seed(seed)
rf_default <- train(x,y, method="rpart", metric=metric, control = rpart.control(minsplit = 140), 
                    trControl=control)
print(rf_default)
varImp(rf_default)
ggplot(varImp(rf_default))


rs[1]
lines.roc(rs[1],rfProb,col=1)

# If you have more than two classes, you can run this separately for each class
multiclass.roc(carData[,7],rfProb)
plot(multiclass.roc(carData[,7],rfProb))


roc(carData[,7],rfProb[,1])
plot(roc(carData[,7],rfProb[,1]))
title(main = "RandomForest Probability using 500 node")
roc(carData[,7],rfProb[,2])
plot(roc(carData[,7],rfProb[,2]))
title(main = "RandomForest Probability using 500 node (Class 2)")
roc(carData[,7],rfProb[,3])
plot(roc(carData[,7],rfProb[,3]))
title(main = "RandomForest Probability using 500 node (Class 3)")
roc(carData[,7],rfProb[,4])
plot(roc(carData[,7],rfProb[,4]))
title(main = "RandomForest Probability using 500 node (Class 4)")


#--------------------------------------------------------------------------------------------------------


x=carData[,1:6]
y=carData[,7]

# Use caret package for 10 fold cross validation
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 123
metric <- "Accuracy"
set.seed(seed)
rf_default <- train(x,y, method="rf", metric=metric, trControl=control)
print(rf_default)
varImp(rf_default)
ggplot(varImp(rf_default))


# 
# rf=randomForest(x,y,nodesize=50) # what is nodesize ? *****
# rfp=predict(rf,x)
# rfCM=table(rfp,y)
# rfCM
# #Accuracy of the prediction using randomForest and node 50
# sum(diag(rfCM))/sum(rfCM)
# rfProb=predict(rf,x,type="prob")
# 
# # sensitivity is the ability to correctly identify a given class
# # specificity is the ability to correctly identify those who are in different class(es)
# # Here, we are using the second column that means class 'M'
# 
# # If we have more than two classes you need to use multiclass.roc
# # roc(carData[,7],rfProb[,2])
# 
# # plot(roc(carData[,7],rfProb[,2]))
# # title(main = "RandomForest Probability using 50 node (Class 2)")
# 
# # If you have more than two classes, you can run this separately for each class
# roc(carData[,7],rfProb[,1])
# plot(roc(carData[,7],rfProb[,1]))
# title(main = "RandomForest Probability using 50 node (Class 1)")
# roc(carData[,7],rfProb[,2])
# plot(roc(carData[,7],rfProb[,2]))
# title(main = "RandomForest Probability using 50 node (Class 2)")
# roc(carData[,7],rfProb[,3])
# plot(roc(carData[,7],rfProb[,3]))
# title(main = "RandomForest Probability using 50 node (Class 3)")
# roc(carData[,7],rfProb[,4])
# plot(roc(carData[,7],rfProb[,4]))
# title(main = "RandomForest Probability using 50 node (Class 4)")
# 
# rf=randomForest(x,y,nodesize=10)
# rfp=predict(rf,x)
# rfCM=table(rfp,y)
# rfCM
# #Accuracy of the prediction using randomForest and node 10
# sum(diag(rfCM))/sum(rfCM)
# rfProb=predict(rf,x,type="prob")
# #roc(carData[,7],rfProb[,2])
# #plot(roc(carData[,7],rfProb[,2]))
# 
# # If you have more than two classes, you can run this separately for each class
# roc(carData[,7],rfProb[,1])
# plot(roc(carData[,7],rfProb[,1]))
# title(main = "RandomForest Probability using 10 node (Class 1)")
# roc(carData[,7],rfProb[,2])
# plot(roc(carData[,7],rfProb[,2]))
# title(main = "RandomForest Probability using 10 node (Class 2)")
# roc(carData[,7],rfProb[,3])
# plot(roc(carData[,7],rfProb[,3]))
# title(main = "RandomForest Probability using 10 node (Class 3)")
# roc(carData[,7],rfProb[,4])
# plot(roc(carData[,7],rfProb[,4]))
# title(main = "RandomForest Probability using 10 node (Class 4)")
# 
# # Use caret package for 10 fold cross validation
# library(caret)
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# seed <- 123
# metric <- "Accuracy"
# set.seed(seed)
# rf_default <- train(x,y, method="rf", metric=metric, trControl=control)
# library(caret)
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# rf_default <- train(x,y, method="rf", metric=metric, trControl=control)
# print(rf_default)
# varImp(rf_default)
# ggplot(varImp(rf_default))

# library(caret)
# library(e1071)
# control <- trainControl(method="cv", number=10, savePredictions=TRUE)
# seed <- 7
# metric <- "Accuracy"
# set.seed(seed)
# rf_default <- train(x,y, method="rpart", metric=metric, control = rpart.control(minsplit = 500), 
#                     trControl=control)
# print(rf_default)
# varImp(rf_default)
# ggplot(varImp(rf_default))

#-------------------------------------------

#Extra

