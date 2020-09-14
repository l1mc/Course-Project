# To build a L1-regularized logistic regression model to predict the click-through rate of an advertising demand-side platform (DSP). 
# My aim is to predict whether an advertisement will be clicked by a mobile phone customer.

# load glmnet
install.packages('shape')
install.packages('glmnet')
install.packages("caret")
library(caret)
library('glmnet')
library('shape')
# load the data
# stringsAsFacotors = True
RTB <- read.csv("D:/PS4/RTB.csv",stringsAsFactors = TRUE)
# check the structure of RTB
str(RTB)
# dc, atype, instl, isp, nt are categorical variables. Thus convert them  to factors 
RTB$dc = as.factor(RTB$dc)
RTB$atype = as.factor(RTB$atype)
RTB$instl = as.factor(RTB$instl)
RTB$isp = as.factor(RTB$isp)
RTB$nt = as.factor(RTB$nt)

# The variable X is the index. Thus, I will remove this variable from my model.
RTB['X']

# split the data into the training set and the testing set
set.seed(888)
training.rows <- sample(1:nrow(RTB), nrow(RTB)*0.7)
RTB_training = RTB[training.rows,]
RTB_testing = RTB[-training.rows,]

# cross-validation
# set 5-fold cross-validation
trControl <- trainControl(method  = "cv", number  = 5)
# use accuracy as the metric for classification model 
lasso_cv<- train(dc~.-X, method = "glmnet",family='binomial',trControl=trControl,tuneGrid = expand.grid(alpha=1,lambda = seq(0,1,0.01)),metric='Accuracy',data = RTB_training)
lasso_cv

