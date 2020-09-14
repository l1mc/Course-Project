# In this R script, I built a model that uses statistics of images of four letters – A, B, P, and R – to predict which letter a particular image corresponds to.

letters <- read.csv("D:/letters_ABPR.csv")
str(letters)
# Change the value of 'BPR' to 'others'
letters$letter_changed[letters$letter!="A"]<- 'others'
letters$letter_changed[letters$letter=="A"]<- 'A'
str(letters)

# split the data into the training set and the testing set
set.seed(888)
training.rows <- sample(1:nrow(letters), nrow(letters)*0.7)
letters_training = letters[training.rows,]
letters_testing = letters[-training.rows,]



# 1. Build a CART tree model
# import libraries
library(rpart)
library(rpart.plot)

# build tree
letters_tree = rpart(letter_changed ~.-letter, data=letters_training, method="class",parms = list(split = "gini"),cp=0) # method是基于Y的类型决定。
summary(letters_tree)

# Plot the tree
prp(letters_tree,extra=1)

# Predict on the testing set
letters_testing$predict = predict(letters_tree, newdata = letters_testing, type = "prob")[,1]
str(letters_testing)
letters_testing$predict[letters_testing$predict>0.5] = 'A'
letters_testing$predict[letters_testing$predict!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy=letters_testing$predict==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy'])))



# 2. Vary the value of cp within the set {0, 0.005, 0.01, 0.02, 0.03, 0.05, 0.1} to control the model complexity
# ① cp=0.005
# build tree
letters_tree = rpart(letter_changed ~.-letter, data=letters_training, method="class",parms = list(split = "gini"),cp=0.005) # method是基于Y的类型决定。
summary(letters_tree)

# Predict on the testing set
letters_testing$predict = predict(letters_tree, newdata = letters_testing, type = "prob")[,1]
str(letters_testing)
letters_testing$predict[letters_testing$predict>0.5] = 'A'
letters_testing$predict[letters_testing$predict!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy=letters_testing$predict==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy'])))

# ② cp=0.01
letters_tree = rpart(letter_changed ~.-letter, data=letters_training, method="class",parms = list(split = "gini"),cp=0.01) # method是基于Y的类型决定。

# Predict on the testing set
letters_testing$predict = predict(letters_tree, newdata = letters_testing, type = "prob")[,1]
str(letters_testing)
letters_testing$predict[letters_testing$predict>0.5] = 'A'
letters_testing$predict[letters_testing$predict!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy=letters_testing$predict==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy'])))

# ③ cp=0.02
letters_tree = rpart(letter_changed ~.-letter, data=letters_training, method="class",parms = list(split = "gini"),cp=0.02) # method是基于Y的类型决定。

# Predict on the testing set
letters_testing$predict = predict(letters_tree, newdata = letters_testing, type = "prob")[,1]
str(letters_testing)
letters_testing$predict[letters_testing$predict>0.5] = 'A'
letters_testing$predict[letters_testing$predict!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy=letters_testing$predict==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy'])))

# ④ cp=0.03
letters_tree = rpart(letter_changed ~.-letter, data=letters_training, method="class",parms = list(split = "gini"),cp=0.03) # method是基于Y的类型决定。

# Predict on the testing set
letters_testing$predict = predict(letters_tree, newdata = letters_testing, type = "prob")[,1]
str(letters_testing)
letters_testing$predict[letters_testing$predict>0.5] = 'A'
letters_testing$predict[letters_testing$predict!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy=letters_testing$predict==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy'])))

# ⑤ cp=0.05
letters_tree = rpart(letter_changed ~.-letter, data=letters_training, method="class",parms = list(split = "gini"),cp=0.05) # method是基于Y的类型决定。

# Predict on the testing set
letters_testing$predict = predict(letters_tree, newdata = letters_testing, type = "prob")[,1]
str(letters_testing)
letters_testing$predict[letters_testing$predict>0.5] = 'A'
letters_testing$predict[letters_testing$predict!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy=letters_testing$predict==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy'])))

# ⑥ cp=0.1
letters_tree = rpart(letter_changed ~.-letter, data=letters_training, method="class",parms = list(split = "gini"),cp=0.1) # method是基于Y的类型决定。

# Predict on the testing set
letters_testing$predict = predict(letters_tree, newdata = letters_testing, type = "prob")[,1]
str(letters_testing)
letters_testing$predict[letters_testing$predict>0.5] = 'A'
letters_testing$predict[letters_testing$predict!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy=letters_testing$predict==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy'])))




# 3. Build a random forest model. Also vary the value of cp in the same set as part 2 to find the best cp value
library(randomForest)
letters_training$letter_changed = as.factor(letters_training$letter_changed)
letters_testing$letter_changed = as.factor(letters_train$letter_changed)

# ①cp=0
# build random forest
letters_forest = randomForest(letter_changed ~.-letter, data = letters_training, ntree=100, cp=0)

# Make predictions of positive outcome probabilities
letters_testing$predict_forest = predict(letters_forest, newdata = letters_testing,type = "prob")[,1]  # prob表示预测的是概率
letters_testing$predict_forest[letters_testing$predict_forest>0.5] = 'A'
letters_testing$predict_forest[letters_testing$predict_forest!='A'] ='others'
str(letters_testing)

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy_forest=letters_testing$predict_forest==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy_forest'])))

# ②cp=0.005
# build random forest
letters_forest = randomForest(letter_changed ~.-letter, data = letters_training, ntree=100, cp=0.005)

# Make predictions of positive outcome probabilities
letters_testing$predict_forest = predict(letters_forest, newdata = letters_testing,type = "prob")[,1]  # prob表示预测的是概率
letters_testing$predict_forest[letters_testing$predict_forest>0.5] = 'A'
letters_testing$predict_forest[letters_testing$predict_forest!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy_forest=letters_testing$predict_forest==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy_forest'])))

# ③cp=0.01
# build random forest
letters_forest = randomForest(letter_changed ~.-letter, data = letters_training, ntree=100, cp=0.01)

# Make predictions of positive outcome probabilities
letters_testing$predict_forest = predict(letters_forest, newdata = letters_testing,type = "prob")[,1]  # prob表示预测的是概率
letters_testing$predict_forest[letters_testing$predict_forest>0.5] = 'A'
letters_testing$predict_forest[letters_testing$predict_forest!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy_forest=letters_testing$predict_forest==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy_forest'])))

# ④cp=0.02
# build random forest
letters_forest = randomForest(letter_changed ~.-letter, data = letters_training, ntree=100, cp=0.02)

# Make predictions of positive outcome probabilities
letters_testing$predict_forest = predict(letters_forest, newdata = letters_testing,type = "prob")[,1]  # prob表示预测的是概率
letters_testing$predict_forest[letters_testing$predict_forest>0.5] = 'A'
letters_testing$predict_forest[letters_testing$predict_forest!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy_forest=letters_testing$predict_forest==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy_forest'])))

# ⑤cp=0.03
# build random forest
letters_forest = randomForest(letter_changed ~.-letter, data = letters_training, ntree=100, cp=0.02)

# Make predictions of positive outcome probabilities
letters_testing$predict_forest = predict(letters_forest, newdata = letters_testing,type = "prob")[,1]  # prob表示预测的是概率
letters_testing$predict_forest[letters_testing$predict_forest>0.5] = 'A'
letters_testing$predict_forest[letters_testing$predict_forest!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy_forest=letters_testing$predict_forest==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy_forest'])))

# ⑥cp=0.05
# build random forest
letters_forest = randomForest(letter_changed ~.-letter, data = letters_training, ntree=100, cp=0.02)

# Make predictions of positive outcome probabilities
letters_testing$predict_forest = predict(letters_forest, newdata = letters_testing,type = "prob")[,1]  # prob表示预测的是概率
letters_testing$predict_forest[letters_testing$predict_forest>0.5] = 'A'
letters_testing$predict_forest[letters_testing$predict_forest!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy_forest=letters_testing$predict_forest==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy_forest'])))

# ⑦cp=0.1
# build random forest
letters_forest = randomForest(letter_changed ~.-letter, data = letters_training, ntree=100, cp=0.02)

# Make predictions of positive outcome probabilities
letters_testing$predict_forest = predict(letters_forest, newdata = letters_testing,type = "prob")[,1]  # prob表示预测的是概率
letters_testing$predict_forest[letters_testing$predict_forest>0.5] = 'A'
letters_testing$predict_forest[letters_testing$predict_forest!='A'] ='others'

# let letter be 'A' if the probability that the letter is 'A' is larger than 0.5.
letters_testing$accuracy_forest=letters_testing$predict_forest==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy_forest'])))




# 4. Buid an XGBT and fine tune the hyper-parameters to improve its performance.
library('xgboost')
# Create the features and labels
X_training = data.matrix(letters_training[,2:17])
Y_training = letters_training[,18]
X_tesing = data.matrix(letters_testing[,2:17])
Y_testing = letters_testing[,18]
# letters_training$Y_training[letters_training$letter=='A'] = 1
# letters_training$Y_training[letters_training$letter!='A'] = 0
# letters_testing$Y_testing[letters_testing$letter=='A'] = 1
# letters_testing$Y_testing[letters_testing$letter!='A'] = 0
# str(letters_training)

# train the model
set.seed(888)
letters_xgbt = xgboost(data = X_training, label = Y_training, nrounds = 50, max_depth = 5, eta = 0.1, gamma = 0.001, lambda=1, colsample_bynode=0.8,objective = "reg:squarederror")
# predict
letters_testing$predict_xgbt = predict(letters_xgbt, X_tesing)
letters_testing$predict_xgbt[letters_testing$predict_xgbt<1.5] = 'A'
letters_testing$predict_xgbt[letters_testing$predict_xgbt!='A'] ='others'
letters_testing$accuracy_xgbt=letters_testing$predict_xgbt==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy_xgbt'])))

# fine tune the model
set.seed(888)
letters_xgbt = xgboost(data = X_training, label = Y_training, nrounds = 125, max_depth = 10, eta = 0.1, gamma = 0.01,lambda=3,colsample_bynode=0.7,verbose=0,objective = "reg:squarederror")
# predict
letters_testing$predict_xgbt = predict(letters_xgbt, X_tesing)
letters_testing$predict_xgbt[letters_testing$predict_xgbt<1.5] = 'A'
letters_testing$predict_xgbt[letters_testing$predict_xgbt!='A'] ='others'
letters_testing$accuracy_xgbt=letters_testing$predict_xgbt==letters_testing$letter_changed
print(paste('The the out-of-sample accuracy is',colMeans(letters_testing['accuracy_xgbt'])))