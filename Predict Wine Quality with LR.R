# Predict Wine Quality

# load the data
setwd('D:/')
# wine_quality <- read.csv("winequality-white.csv")
wine_quality <- read.table( "winequality-white.csv",header=TRUE,sep=";" )  # Really tricky point. I first used read.csv(). However, when I run str(), I was told that there was only one variable. Then I opened the csv file, finding that all the separators are semicolon instead of colon.
head(wine_quality)
str(wine_quality)
summary(wine_quality)

# 1. Split the data into training and validation sets.
set.seed(12)  # make the result replicable
training.rows <- sample(1:nrow(wine_quality), 3500)
wine_quality.training = wine_quality[training.rows,]
wine_quality.validation = wine_quality[-training.rows,]

str(wine_quality.training)
summary(wine_quality.training)
str(wine_quality.validation)
summary(wine_quality.validation)

write.csv(wine_quality.training,file="D:/wine_quality_training.csv")
write.csv(wine_quality.validation,file="D:/wine_quality.validation.csv")


# 2. Regression and R²
model = lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data=wine_quality.training)
# summary(model)

# R2 of the training set
SSE = sum(model$residuals^2)
SST = sum((wine_quality.training$quality - mean(wine_quality.training$quality))^2)
1 - SSE/SST

# R2 of the validation set
predicted_y_new = predict(model, newdata=wine_quality.validation)
# predicted_y_new
OoS_SSE = sum((predicted_y_new-wine_quality.validation$quality)^2)   # out of sample SSE
# OoS_SSE

OoS_SST = sum((mean(hotel_pricing_validation$log_price)-hotel_pricing_training$log_price)^2)
# OoS_SST

1 - OoS_SSE/OoS_SST


# 3. Build 11 different linear regression models, each with one feature removed, to figure out which feature provide the most additional information
# define a function to calculate R²
reg = function(var) {
  model = lm(var, data=wine_quality.training)
  
  # R2 of the training set
  SSE = sum(model$residuals^2)
  SSE
  SST = sum((wine_quality.training$quality - mean(wine_quality.training$quality))^2)
  R2_in = 1 - SSE/SST
  # R2 of the validation set
  predicted_y_new = predict(model, newdata=wine_quality.validation)
  OoS_SSE = sum((predicted_y_new-wine_quality.validation$quality)^2)  
  OoS_SST = sum((mean(hotel_pricing_validation$log_price)-hotel_pricing_training$log_price)^2)
  R2_out = 1 - OoS_SSE/OoS_SST
  print(paste('R2_out is ', R2_out, sep=''))
  print('-----------------')
}

# define a function to generate variables
var=function(name){
  variable_all = list('fixed.acidity', 'volatile.acidity','citric.acid', 'residual.sugar', 'chlorides', 'free.sulfur.dioxide', 'total.sulfur.dioxide', 'density', 'pH', 'sulphates', 'alcohol')
  temp = ''
  for (item in variable_all) {
    if (item==variable_all[name]) {
      next
    }
    temp=paste(temp,item, sep = " + ")
  }
  paste('quality ~ ',substring(temp,4),sep = '')
}

# outcome
for (i in 1:11) {
  print(i)
  reg(var(i))
}


