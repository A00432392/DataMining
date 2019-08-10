# If you are on linux you can uncomment the following lines to run caret on multiple cores
#install.packages("doMC")
#install.packages("caret")
#library(doMC)
#registerDoMC(4)
library(caret)
setwd("F:/SMU2/Data mining/MLAssignment4/")
getwd()
# xy needs to be tried for 3 data sets.
# 1. 15minutesmatrix.txt  
# 2. daymatrix.txt
# 3. hourlymatrix.txt
xy=read.table("15minutesmatrix.txt",sep=' ',header=F)
y=xy[,8]
head(y)
x=xy[,1:7]

# Using pre-sliced data
# method = the resampling method is repeatedcv.
# K-fold , dividing the training set into 10 parts.
# number = is number of folds or number of resampling iterations.
# repeats= for repeated k-fold cross-validation only: the number of complete sets of folds to compute.

myCvControl <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 5)
# Linear regression 
# glm (Generalized linear model)
# Cross-Validated (10 fold)
# RMSE = Root mean square deviation or Root mean square error 
# Math.sqrt(Sigma[0 - n](values predicted - values observed)^2)
# MAE = computes the average absolute difference between two numeric vectors

glmFitTime <- train(V8 ~ .,
                    data = xy,
                    method = "glm",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
print(glmFitTime)
summary(glmFitTime)
y_hat = predict(glmFitTime, newdata = x)
summary(y_hat)
mean(100*abs(y_hat-y)/y)
# Your error with linear regression

# Support Vector Machine Radial
svmFitTime <- train(V8 ~ .,
                    data = xy,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    #tuneLength = 10,
                    trControl = myCvControl)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = x)
mean(100*abs(y_hat-y)/y) 
# Your error with support vector regression

# Neural Network
nnFitTime <- train(V8 ~ .,
                   data = xy,
                   method = "avNNet",
                   preProc = c("center", "scale"),
                   trControl = myCvControl,
                   #tuneLength = 10,
                   linout = T,
                   trace = F,
                   MaxNWts = 10 * (ncol(xy) + 1) + 10 + 1,
                   maxit = 500)
nnFitTime
summary(nnFitTime)
y_hat = predict(nnFitTime, newdata = x)
mean(100*abs(y_hat-y)/y)
# Your error with neural networks

# random forest
randomforest <- train(V8 ~ ., data = xy,method = "rf",ntree = 500,trControl = myCvControl
)
randomforest
summary(randomforest)
y_hat = predict(randomforest, newdata = x)
mean(100*abs(y_hat-y)/y)

# You can experiment with other methods, here is where you can find the methods caret supports:
# https://topepo.github.io/caret/available-models.html

# Compare models
resamps <- resamples(list(lm = glmFitTime,
                          svn = svmFitTime,
                          nn = nnFitTime,
                          rf = randomforest))
summary(resamps)


# Now working with the time-series modeling
# t needs to be tried for 3 data sets.
# 1. 15minutes.csv 
# 2. day.csv
# 3. hourly.csv
t= read.csv("15minutes.csv",header=FALSE)
head(t)
tSeries = ts(t,freq=4)
head(tSeries)
plot.ts(tSeries)
time(tSeries)
quantile(tSeries)
plot(decompose(tSeries))
plot(diff(tSeries))
ggseasonplot(tSeries)
adf.test(tSeries)
acf(tSeries, lag.max = 20)
#install.packages("forecast")
library(forecast)

ar <- Arima(tSeries,order=c(1,0,2))
mean(100*abs(fitted(ar) - tSeries)/tSeries)
#13.50665
# Your Arima error

res = auto.arima(tSeries, stepwise = F, approximation = F)
res
plot(forecast(res, h= 3))