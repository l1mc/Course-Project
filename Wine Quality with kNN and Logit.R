# 1. Preparations 

# import data
wine <- read.csv("D:/wine.csv")

# create the Y variable
str(wine)
wine$Y <- wine$quality>5
head(wine,15)

# train-validation-test split
set.seed(8)
training.rows <- sample(1:nrow(wine), nrow(wine)*0.6)
vt.rows = setdiff(1:nrow(wine),training.rows)
validation.rows = sample(vt.rows,nrow(wine)*0.2)
testing.rows = setdiff(vt.rows,validation.rows)

wine_training = wine[training.rows,]
wine_validation = wine[validation.rows,]
wine_testing = wine[testing.rows,]

str(wine_training)
str(wine_validation)
summary(wine_validation)
str(wine_testing)


# 2. Compare the prediction performance of logistic model and KNN model
# logistic model
model1 = glm(Y ~ .-quality, data=wine_training,family=binomial)
summary(model1)

#Out-of-Sample Prediction
predicted_Y=predict(model1,type="response",newdata=wine_validation)

table(wine_validation$Y, predicted_Y > 0.5)
accuracy_logistic=(172+563)/(172+161+83+563)
accuracy_logistic

# kNN model
# standardize data in training set
fixed_A = (wine_training$fixed.acidity-mean(wine_training$fixed.acidity))/sd(wine_training$fixed.acidity)
Volatile_A = (wine_training$volatile.acidity-mean(wine_training$volatile.acidity))/sd(wine_training$volatile.acidity)
citric_A = (wine_training$citric.acid-mean(wine_training$citric.acid))/sd(wine_training$citric.acid)
residual_S = (wine_training$residual.sugar-mean(wine_training$residual.sugar))/sd(wine_training$residual.sugar)
chlorides_ = (wine_training$chlorides-mean(wine_training$chlorides))/sd(wine_training$chlorides)
free_SD = (wine_training$free.sulfur.dioxide-mean(wine_training$free.sulfur.dioxide))/sd(wine_training$free.sulfur.dioxide)
total_SD = (wine_training$total.sulfur.dioxide-mean(wine_training$total.sulfur.dioxide))/sd(wine_training$total.sulfur.dioxide)
density_ = (wine_training$density-mean(wine_training$density))/sd(wine_training$density)
pH_ = (wine_training$pH-mean(wine_training$pH))/sd(wine_training$pH)
sulphates_ = (wine_training$sulphates-mean(wine_training$sulphates))/sd(wine_training$sulphates)
alcohol_ = (wine_training$alcohol-mean(wine_training$alcohol))/sd(wine_training$alcohol)

knn_training=data.frame(fixed_A, Volatile_A, citric_A, residual_S, chlorides_, free_SD, total_SD, density_, pH_, sulphates_, alcohol_)

# standardize data in validation set
fixed_A = (wine_validation$fixed.acidity-mean(wine_validation$fixed.acidity))/sd(wine_validation$fixed.acidity)
Volatile_A = (wine_validation$volatile.acidity-mean(wine_validation$volatile.acidity))/sd(wine_validation$volatile.acidity)
citric_A = (wine_validation$citric.acid-mean(wine_validation$citric.acid))/sd(wine_validation$citric.acid)
residual_S = (wine_validation$residual.sugar-mean(wine_validation$residual.sugar))/sd(wine_validation$residual.sugar)
chlorides_ = (wine_validation$chlorides-mean(wine_validation$chlorides))/sd(wine_validation$chlorides)
free_SD = (wine_validation$free.sulfur.dioxide-mean(wine_validation$free.sulfur.dioxide))/sd(wine_validation$free.sulfur.dioxide)
total_SD = (wine_validation$total.sulfur.dioxide-mean(wine_validation$total.sulfur.dioxide))/sd(wine_validation$total.sulfur.dioxide)
density_ = (wine_validation$density-mean(wine_validation$density))/sd(wine_validation$density)
pH_ = (wine_validation$pH-mean(wine_validation$pH))/sd(wine_validation$pH)
sulphates_ = (wine_validation$sulphates-mean(wine_validation$sulphates))/sd(wine_validation$sulphates)
alcohol_ = (wine_validation$alcohol-mean(wine_validation$alcohol))/sd(wine_validation$alcohol)

knn_validation=data.frame(fixed_A, Volatile_A, citric_A, residual_S, chlorides_, free_SD, total_SD, density_, pH_, sulphates_, alcohol_)

# standardize data in test set
fixed_A = (wine_testing$fixed.acidity-mean(wine_testing$fixed.acidity))/sd(wine_testing$fixed.acidity)
Volatile_A = (wine_testing$volatile.acidity-mean(wine_testing$volatile.acidity))/sd(wine_testing$volatile.acidity)
citric_A = (wine_testing$citric.acid-mean(wine_testing$citric.acid))/sd(wine_testing$citric.acid)
residual_S = (wine_testing$residual.sugar-mean(wine_testing$residual.sugar))/sd(wine_testing$residual.sugar)
chlorides_ = (wine_testing$chlorides-mean(wine_testing$chlorides))/sd(wine_testing$chlorides)
free_SD = (wine_testing$free.sulfur.dioxide-mean(wine_testing$free.sulfur.dioxide))/sd(wine_testing$free.sulfur.dioxide)
total_SD = (wine_testing$total.sulfur.dioxide-mean(wine_testing$total.sulfur.dioxide))/sd(wine_testing$total.sulfur.dioxide)
density_ = (wine_testing$density-mean(wine_testing$density))/sd(wine_testing$density)
pH_ = (wine_testing$pH-mean(wine_testing$pH))/sd(wine_testing$pH)
sulphates_ = (wine_testing$sulphates-mean(wine_testing$sulphates))/sd(wine_testing$sulphates)
alcohol_ = (wine_testing$alcohol-mean(wine_testing$alcohol))/sd(wine_testing$alcohol)

knn_testing=data.frame(fixed_A, Volatile_A, citric_A, residual_S, chlorides_, free_SD, total_SD, density_, pH_, sulphates_, alcohol_)


# create the labels
Y_training=wine_training$Y
Y_validation=wine_validation$Y


# define a function to measure overall accuracy
accuracy_knn = function(actual, predicted) {
  mean(actual == predicted)
}

# predict Y
library(class)
predicted_Y=knn(train = knn_training, test = knn_validation, cl = Y_training, k = 5)

# valuate
accuracy = accuracy_knn(actual = Y_validation,predicted = predicted_Y)  # overall accuracy
print(paste("The overall accuracy is",accuracy))  # larger than the one of logistic model → choose knn model
table(Y_validation, predicted_Y)  # confusion

# test
predicted_Y_test=knn(train = knn_training, test = knn_testing, cl = Y_training, k = 5)
testing_error = 1-accuracy_knn(actual = Y_testing,predicted = predicted_Y_test)  # overall accuracy
testing_error



# 3. Use 6-fold cross-validation to find the optimal k (1 ≤ k ≤ 15) for the k−NN model.
# necessary packages
install.packages("caret")
library(caret)
library(class)
install.packages("e1071")
library(e1071)

set.seed(888)
#  split the data into training and testing sets
training.rows <- sample(1:nrow(wine), nrow(wine)*0.7)
wine_training = wine[training.rows,]
wine_testing = wine[-training.rows,]
Y_training=wine_training$Y

length(Y_training)
length(wine_training$fixed.acidity)
mode(Y_training)
Y_training=as.factor(Y_training)
mode(Y_training)

# standardize data in training set
fixed_A = (wine_training$fixed.acidity-mean(wine_training$fixed.acidity))/sd(wine_training$fixed.acidity)
Volatile_A = (wine_training$volatile.acidity-mean(wine_training$volatile.acidity))/sd(wine_training$volatile.acidity)
citric_A = (wine_training$citric.acid-mean(wine_training$citric.acid))/sd(wine_training$citric.acid)
residual_S = (wine_training$residual.sugar-mean(wine_training$residual.sugar))/sd(wine_training$residual.sugar)
chlorides_ = (wine_training$chlorides-mean(wine_training$chlorides))/sd(wine_training$chlorides)
free_SD = (wine_training$free.sulfur.dioxide-mean(wine_training$free.sulfur.dioxide))/sd(wine_training$free.sulfur.dioxide)
total_SD = (wine_training$total.sulfur.dioxide-mean(wine_training$total.sulfur.dioxide))/sd(wine_training$total.sulfur.dioxide)
density_ = (wine_training$density-mean(wine_training$density))/sd(wine_training$density)
pH_ = (wine_training$pH-mean(wine_training$pH))/sd(wine_training$pH)
sulphates_ = (wine_training$sulphates-mean(wine_training$sulphates))/sd(wine_training$sulphates)
alcohol_ = (wine_training$alcohol-mean(wine_training$alcohol))/sd(wine_training$alcohol)
y = as.factor(wine_training$Y)

knn_train=data.frame(fixed_A, Volatile_A, citric_A, residual_S, chlorides_, free_SD, total_SD, density_, pH_, sulphates_, alcohol_)
knn_training=data.frame(fixed_A, Volatile_A, citric_A, residual_S, chlorides_, free_SD, total_SD, density_, pH_, sulphates_, alcohol_, y)

# 6-fold cross-validation
trControl <- trainControl(method  = "cv", number  = 6)
fit_cv<- train(y ~., data=knn_training, method = "knn",trControl=trControl,tuneGrid = expand.grid(k = 1:15),metric='Accuracy')
fit_cv


# test
# standardize data in testing set
fixed_A = (wine_testing$fixed.acidity-mean(wine_testing$fixed.acidity))/sd(wine_testing$fixed.acidity)
Volatile_A = (wine_testing$volatile.acidity-mean(wine_testing$volatile.acidity))/sd(wine_testing$volatile.acidity)
citric_A = (wine_testing$citric.acid-mean(wine_testing$citric.acid))/sd(wine_testing$citric.acid)
residual_S = (wine_testing$residual.sugar-mean(wine_testing$residual.sugar))/sd(wine_testing$residual.sugar)
chlorides_ = (wine_testing$chlorides-mean(wine_testing$chlorides))/sd(wine_testing$chlorides)
free_SD = (wine_testing$free.sulfur.dioxide-mean(wine_testing$free.sulfur.dioxide))/sd(wine_testing$free.sulfur.dioxide)
total_SD = (wine_testing$total.sulfur.dioxide-mean(wine_testing$total.sulfur.dioxide))/sd(wine_testing$total.sulfur.dioxide)
density_ = (wine_testing$density-mean(wine_testing$density))/sd(wine_testing$density)
pH_ = (wine_testing$pH-mean(wine_testing$pH))/sd(wine_testing$pH)
sulphates_ = (wine_testing$sulphates-mean(wine_testing$sulphates))/sd(wine_testing$sulphates)
alcohol_ = (wine_testing$alcohol-mean(wine_testing$alcohol))/sd(wine_testing$alcohol)
y = as.factor(wine_testing$Y)

knn_test=data.frame(fixed_A, Volatile_A, citric_A, residual_S, chlorides_, free_SD, total_SD, density_, pH_, sulphates_, alcohol_)
knn_testing=data.frame(fixed_A, Volatile_A, citric_A, residual_S, chlorides_, free_SD, total_SD, density_, pH_, sulphates_, alcohol_, y)

predicted_Y_test=knn(train = knn_train, test = knn_test, cl = Y_training, k = 1)
testing_error = 1-accuracy_knn(actual = Y_testing,predicted = predicted_Y_test)  # overall accuracy
testing_error
