# In this R script, I used monthly stock returns from the NASDAQ stock exchange to do clustering. Then, I used the clustering results to predict future stock returns. 

# 1. import and split the data
stock <- read.csv("D:/PS5/Stock.csv")
str(stock)
# split the data into the training set and the testing set
set.seed(888)
training.rows <- sample(1:nrow(stock), nrow(stock)*0.7)
stock_training = stock[training.rows,]
stock_testing = stock[-training.rows,]
# Prepare the features and labels
X_training = data.matrix(stock_training[,1:11])
Y_training = stock_training[,12]
X_testing = data.matrix(stock_testing[,1:11])
Y_testing = stock_testing[,12]


# 2. Train and validate a XGBoost model, predict the future return and then report the out-of-sample AUC.
# Fit an XGBoost model on the training data set
set.seed(888)
stock_xgbt = xgboost(data = X_training, label = Y_training, nrounds = 125, max_depth = 20, eta = 0.1, gamma = 0.01,lambda=100,colsample_bynode=0.7,verbose=0,objective = "binary:logistic")

# predict
stock_testing$predict_xgbt = predict(stock_xgbt, X_testing)
stock_testing$predict_xgbt =as.numeric(stock_testing$predict_xgbt > 0.5)
stock_testing$accuracy_xgbt=stock_testing$predict_xgbt==stock_testing$PositiveDec
print(paste('The the out-of-sample accuracy is',colMeans(stock_testing['accuracy_xgbt'])))

# AUC
library(AUC)
roc_xgbt <- roc(stock_testing$predict_xgbt, as.factor(stock_testing$PositiveDec))
auc_xgbt = auc(roc_xgbt)
print(paste("The out-of-sample AUC for the XGBT is",auc_xgbt))
plot(roc_xgbt)


# 2. remove the dependent variable before clustering
cluster_training = stock_training
cluster_training$PositiveDec = NULL
cluster_testing = stock_testing
cluster_testing$PositiveDec = NULL
str(cluster_testing)

# 3. normalize the variables
# training set
cluster_training_changed=cluster_training
for (i in 1:11) {
  cluster_training_changed[,i]=(cluster_training_changed[i]-mean(cluster_training_changed[,i]))/sd(cluster_training_changed[,i])
}
str(cluster_training_changed)
# testing set
cluster_testing_changed=cluster_testing
for (i in 1:11) {
  cluster_testing_changed[,i]=(cluster_testing_changed[i]-mean(cluster_testing_changed[,i]))/sd(cluster_testing_changed[,i])
}
str(cluster_testing_changed)

# 4. Run k-means clustering with 3 clusters for the training set
cluster_stock_k = kmeans(cluster_training_changed, centers = 3)
str(cluster_stock_k)
cluster_stock_k$cluster
#  Report the number of observations in each cluster
length(which(cluster_stock_k$cluster==1))
length(which(cluster_stock_k$cluster==2))
length(which(cluster_stock_k$cluster==3))
# the average return of the stocks in January in each cluster
mean(cluster_training[,1][which(cluster_stock_k$cluster==1)])
mean(cluster_training[,1][which(cluster_stock_k$cluster==2)])
mean(cluster_training[,1][which(cluster_stock_k$cluster==3)])

# 5. make predictions about which cluster the data in the testing set should belong to
install.packages('flexclust')
library(flexclust)
cluster_stock_kcca=as.kcca(cluster_stock_k,cluster_training_changed)
cluster_testing_changed$predict_cluster=predict(cluster_stock_kcca,newdata=cluster_testing_changed) 
str(cluster_testing_changed)
# the number of observations in each cluster
length(which(cluster_testing_changed$predict_cluster==1))
length(which(cluster_testing_changed$predict_cluster==2))
length(which(cluster_testing_changed$predict_cluster==3))
# the average return of the stocks in January (of the testing set) in each cluster
mean(cluster_testing[,1][which(cluster_testing_changed$predict_cluster==1)])
mean(cluster_testing[,1][which(cluster_testing_changed$predict_cluster==2)])
mean(cluster_testing[,1][which(cluster_testing_changed$predict_cluster==3)])

# 6. Train and validate cluster-specific classification models to check whether clustering helps improve the prediction performance of the model
# split the training set into three sub-training-sets
cluster_training_one = stock_training[which(cluster_stock_k$cluster==1),]
cluster_training_two = stock_training[which(cluster_stock_k$cluster==1),]
cluster_training_three = stock_training[which(cluster_stock_k$cluster==1),]
# split the testing set into three sub-training-sets
cluster_testing_one = stock_testing[which(cluster_testing_changed$predict_cluster==1),]
cluster_testing_two = stock_testing[which(cluster_testing_changed$predict_cluster==2),]
cluster_testing_three = stock_testing[which(cluster_testing_changed$predict_cluster==3),]
str(cluster_testing_three)

# train and validate cluster-specific classification models 
# cluster 1
# Prepare the features and labels
X_training = data.matrix(cluster_training_one[,1:11])
Y_training = cluster_training_one[,12]
X_testing = data.matrix(cluster_testing_one[,1:11])
Y_testing = cluster_testing_one[,12]
# fit the model
#nrounds = 125, max_depth = 20, eta = 0.1, gamma = 0.01,lambda=100,colsample_bynode=0.7
set.seed(888)
stock_xgbt = xgboost(data = X_training, label = Y_training, nrounds = 100, max_depth = 5, eta = 0.1, gamma = 0.001,lambda=1,colsample_bynode=0.55,verbose=0,objective = "binary:logistic")
# predict
cluster_testing_one$predict_xgbt = predict(stock_xgbt, X_testing)
cluster_testing_one$predict_xgbt =as.numeric(cluster_testing_one$predict_xgbt > 0.5)
cluster_testing_one$accuracy_xgbt=cluster_testing_one$predict_xgbt==cluster_testing_one$PositiveDec
print(paste('The the out-of-sample accuracy is',colMeans(cluster_testing_one['accuracy_xgbt'])))
# AUC
roc_xgbt <- roc(cluster_testing_one$predict_xgbt, as.factor(cluster_testing_one$PositiveDec))
auc_xgbt = auc(roc_xgbt)
print(paste("The out-of-sample AUC for the XGBT is",auc_xgbt))

# cluster 2
# Prepare the features and labels
X_training = data.matrix(cluster_training_two[,1:11])
Y_training = cluster_training_two[,12]
X_testing = data.matrix(cluster_testing_two[,1:11])
Y_testing = cluster_testing_two[,12]
# fit the model
set.seed(888)
stock_xgbt = xgboost(data = X_training, label = Y_training, nrounds = 100, max_depth = 5, eta = 0.1, gamma = 0.001,lambda=1,colsample_bynode=0.65,verbose=0,objective = "binary:logistic")
# predict
str(cluster_testing_two)
cluster_testing_two$predict_xgbt = predict(stock_xgbt, X_testing)
cluster_testing_two$predict_xgbt =as.numeric(cluster_testing_two$predict_xgbt > 0.5)
cluster_testing_two$accuracy_xgbt=cluster_testing_two$predict_xgbt==cluster_testing_two$PositiveDec
print(paste('The the out-of-sample accuracy is',colMeans(cluster_testing_two['accuracy_xgbt'])))
# AUC
roc_xgbt <- roc(cluster_testing_two$predict_xgbt, as.factor(cluster_testing_two$PositiveDec))
auc_xgbt = auc(roc_xgbt)
print(paste("The out-of-sample AUC for the XGBT is",auc_xgbt))

# cluster 3
# Prepare the features and labels
X_training = data.matrix(cluster_training_three[,1:11])
Y_training = cluster_training_three[,12]
X_testing = data.matrix(cluster_testing_three[,1:11])
Y_testing = cluster_testing_three[,12]
# fit the model
set.seed(888)
stock_xgbt = xgboost(data = X_training, label = Y_training, nrounds = 100, max_depth = 5, eta = 0.1, gamma = 0.001,lambda=1,colsample_bynode=0.55,verbose=0,objective = "binary:logistic")
# predict
cluster_testing_three$predict_xgbt = predict(stock_xgbt, X_testing)
cluster_testing_three$predict_xgbt =as.numeric(cluster_testing_three$predict_xgbt > 0.5)
cluster_testing_three$accuracy_xgbt=cluster_testing_three$predict_xgbt==cluster_testing_three$PositiveDec
print(paste('The the out-of-sample accuracy is',colMeans(cluster_testing_three['accuracy_xgbt'])))
# AUC
roc_xgbt <- roc(cluster_testing_three$predict_xgbt, as.factor(cluster_testing_three$PositiveDec))
auc_xgbt = auc(roc_xgbt)
print(paste("The out-of-sample AUC for the XGBT is",auc_xgbt))


# the AUC of the cluster-specific models for the entire testing set
cluster_testing_final=rbind(cluster_testing_one,cluster_testing_two,cluster_testing_three)
str(cluster_testing_final)
roc_xgbt <- roc(cluster_testing_final$predict_xgbt, as.factor(cluster_testing_final$PositiveDec))
auc_xgbt = auc(roc_xgbt)
print(paste("The out-of-sample AUC for the XGBT is",auc_xgbt))
