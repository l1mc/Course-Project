BVTradeoff_train <- read.csv("D:/BVTradeoff_train.csv")
# check the structure of BVTradeoff_train
str(BVTradeoff_train)


# 1. Fit five models whose polynomial of degree equals to 1, 2, 3, 4, 5 separately.
# Generate the power of X in data.frame
# the 'i' after X.train means i power of X.train
for (i in 2:5){
  BVTradeoff_train[paste('X.train', i, sep='')]=BVTradeoff_train$X.train**i
}
str(BVTradeoff_train)

# regression
model1 = lm(Y.train ~ X.train, data=BVTradeoff_train)
summary(model1)
model2 = lm(Y.train ~ X.train + X.train2, data=BVTradeoff_train)
summary(model2)
model3 = lm(Y.train ~ X.train + X.train2 + X.train3, data=BVTradeoff_train)
summary(model3)
model4 = lm(Y.train ~ X.train + X.train2 + X.train3 + X.train4, data=BVTradeoff_train)
summary(model4)
model5 = lm(Y.train ~ X.train + X.train2 + X.train3 + X.train4 + X.train5, data=BVTradeoff_train)
summary(model5)


# 2. estimate the expected errors E(Y − fˆi(X))2 (i = 1, 2, 3, 4, 5) of each model using the testing data set
# import data
BVTradeoff_test <- read.csv("D:/BVTradeoff_test.csv")
str(BVTradeoff_test)

# rename the column because to predict Y, the predict() function requires that the make of variables in the test set is the same as that of the train set.
names(BVTradeoff_test) <- c("X.train", "Y.train")

# Generate the power of X in data.frame
# the 'i' after X.train means i power of X.train
for (i in 2:5){
  BVTradeoff_test[paste('X.train', i, sep='')]=BVTradeoff_test$X.train**i
}

# predict Y
BVTradeoff_test['predicted_Y1']=predict(model1,newdata=BVTradeoff_test)
BVTradeoff_test['predicted_Y2']=predict(model2,newdata=BVTradeoff_test)
BVTradeoff_test['predicted_Y3']=predict(model3,newdata=BVTradeoff_test)
BVTradeoff_test['predicted_Y4']=predict(model4,newdata=BVTradeoff_test)
BVTradeoff_test['predicted_Y5']=predict(model5,newdata=BVTradeoff_test)

# calculate the expected error
# considering the columns' names, the number after 'expected error' indicates the ID of the models
# model1
BVTradeoff_test['expected error1']=(BVTradeoff_test['Y.train']-BVTradeoff_test['predicted_Y1'])**2
print(paste('The expected error of model 1 is ',colSums(BVTradeoff_test['expected error1'])/nrow(BVTradeoff_test)))
var(BVTradeoff_test['predicted_Y1'])
# model2
BVTradeoff_test['expected error2']=(BVTradeoff_test['Y.train']-BVTradeoff_test['predicted_Y2'])**2
print(paste('The expected error of model 2 is ',colSums(BVTradeoff_test['expected error2'])/nrow(BVTradeoff_test)))
var(BVTradeoff_test['predicted_Y2'])
# model3
BVTradeoff_test['expected error3']=(BVTradeoff_test['Y.train']-BVTradeoff_test['predicted_Y3'])**2
print(paste('The expected error of model 3 is ',colSums(BVTradeoff_test['expected error3'])/nrow(BVTradeoff_test)))
var(BVTradeoff_test['predicted_Y3'])
# model4
BVTradeoff_test['expected error4']=(BVTradeoff_test['Y.train']-BVTradeoff_test['predicted_Y4'])**2
print(paste('The expected error of model 4 is ',colSums(BVTradeoff_test['expected error4'])/nrow(BVTradeoff_test)))
var(BVTradeoff_test['predicted_Y4'])
# model5
BVTradeoff_test['expected error5']=(BVTradeoff_test['Y.train']-BVTradeoff_test['predicted_Y5'])**2
print(paste('The expected error of model 5 is ',colSums(BVTradeoff_test['expected error5'])/nrow(BVTradeoff_test)))
var(BVTradeoff_test['predicted_Y5'])


# 3. Use the testing data set to estimate the bias of each model
# Create the column of f(X)
BVTradeoff_test['Fx']=BVTradeoff_test['X.train3']+BVTradeoff_test['X.train2']*2+BVTradeoff_test['X.train']*3+1

# calculate the bias of the models
# considering the columns' names, the number after 'bias' indicates the ID of the models
# model1
BVTradeoff_test['bias1']=(BVTradeoff_test['Fx']-BVTradeoff_test['predicted_Y1'])**2
print(paste('The bias of model 1 is ',colSums(BVTradeoff_test['bias1'])/nrow(BVTradeoff_test)))
# model2
BVTradeoff_test['bias2']=(BVTradeoff_test['Fx']-BVTradeoff_test['predicted_Y2'])**2
print(paste('The bias of model 2 is ',colSums(BVTradeoff_test['bias2'])/nrow(BVTradeoff_test)))
# model3
BVTradeoff_test['bias3']=(BVTradeoff_test['Fx']-BVTradeoff_test['predicted_Y3'])**2
print(paste('The bias of model 3 is ',colSums(BVTradeoff_test['bias3'])/nrow(BVTradeoff_test)))
# model4
BVTradeoff_test['bias4']=(BVTradeoff_test['Fx']-BVTradeoff_test['predicted_Y4'])**2
print(paste('The bias of model 4 is ',colSums(BVTradeoff_test['bias4'])/nrow(BVTradeoff_test)))
# model5
BVTradeoff_test['bias5']=(BVTradeoff_test['Fx']-BVTradeoff_test['predicted_Y5'])**2
print(paste('The bias of model 5 is ',colSums(BVTradeoff_test['bias5'])/nrow(BVTradeoff_test)))


# 4. Use the testing data set to estimate the variance of each model
# calculate E_predicted_Y
BVTradeoff_test['E_predicted_Y1']=colMeans(BVTradeoff_test['predicted_Y1'])
BVTradeoff_test['E_predicted_Y2']=colMeans(BVTradeoff_test['predicted_Y2'])
BVTradeoff_test['E_predicted_Y3']=colMeans(BVTradeoff_test['predicted_Y3'])
BVTradeoff_test['E_predicted_Y4']=colMeans(BVTradeoff_test['predicted_Y4'])
BVTradeoff_test['E_predicted_Y5']=colMeans(BVTradeoff_test['predicted_Y5'])

# calculate the variance of the models
# model1
BVTradeoff_test['variance1']=(BVTradeoff_test['predicted_Y1']-BVTradeoff_test['E_predicted_Y1'])**2
print(paste('The variance of model 1 is ',colSums(BVTradeoff_test['variance1'])/nrow(BVTradeoff_test)))
# model2
BVTradeoff_test['variance2']=(BVTradeoff_test['predicted_Y2']-BVTradeoff_test['E_predicted_Y2'])**2
print(paste('The variance of model 2 is ',colSums(BVTradeoff_test['variance2'])/nrow(BVTradeoff_test)))
# model3
BVTradeoff_test['variance3']=(BVTradeoff_test['predicted_Y3']-BVTradeoff_test['E_predicted_Y3'])**2
print(paste('The variance of model 3 is ',colSums(BVTradeoff_test['variance3'])/nrow(BVTradeoff_test)))
# model4
BVTradeoff_test['variance4']=(BVTradeoff_test['predicted_Y4']-BVTradeoff_test['E_predicted_Y4'])**2
print(paste('The variance of model 4 is ',colSums(BVTradeoff_test['variance4'])/nrow(BVTradeoff_test)))
# model5
BVTradeoff_test['variance5']=(BVTradeoff_test['predicted_Y5']-BVTradeoff_test['E_predicted_Y5'])**2
print(paste('The variance of model 5 is ',colSums(BVTradeoff_test['variance5'])/nrow(BVTradeoff_test)))