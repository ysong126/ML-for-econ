library(MASS)
library(corrplot)
library(neuralnet)
library(glmnet)

# correlation plot 
corrplot(cor(Boston), method = "circle", type ="lower", diag=TRUE)

# split data into training and testing 90% and 10%
set.seed(790) 
index<-sample(nrow(Boston), nrow(Boston)*0.9)
Boston.train<-Boston[index,]
Boston.test <-Boston[-index,]


# OLS
# train
feature_names<-names(Boston.train)
f <- as.formula(paste("medv ~", paste(feature_names[!feature_names %in% "medv"], collapse = " + ")))
ols_model<-lm(f, data=Boston.train)

# test 
ols_predict<-predict(ols_model,newdata=Boston.test)

# Calculating MSE       out-of-sample test
OLS_MSE<- sum((ols_predict - Boston.test[,14])^2)/nrow(Boston.test)



# Neural Net
# re-scale standarlized
library(MASS)
library(neuralnet)
set.seed(790) 
index<-sample(nrow(Boston), nrow(Boston)*0.9)
Boston.train<-Boston[index,]
Boston.test <-Boston[-index,]

data=Boston
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

# sample splitting
# index = sample(nrow(scaled),nrow(scaled)*0.9)
train_ = scaled[index,]
test_ = scaled[-index,]

# training
feature_names= names(data)
f <- as.formula(paste("medv ~", paste(feature_names[!feature_names %in% "medv"], collapse = " + ")))
nn <-neuralnet(f, data=train_,hidden = c(3,3,3), linear.output = TRUE)

#plot

plot(nn,arrow.length = 0.2)
#quartz.save("neural_net_plot3by8.png", type="png")
nnet_pred_scaled <- neuralnet::compute(nn,test_[,1:13])

# Descaling 
# predicted values
nnet_pred <- nnet_pred_scaled$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
# true value
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

# Calculating MSE
NNet_MSE <- sum((test.r - nnet_pred)^2)/nrow(test_)


# Lasso standardized MSE = 48
library(MASS)
set.seed(790) 
index<-sample(nrow(Boston), nrow(Boston)*0.9)
Boston.train<-Boston[index,]
Boston.test <-Boston[-index,]

# scaled need to convert into matrix
Train_matrix = as.matrix(train_)

X = Train_matrix[,1:13]
Y = Train_matrix[,14]

lasso_model <- glmnet(x=X,y=Y,family = "gaussian", alpha = 1)
#summary(lasso_model)
plot(lasso_model,xvar="lambda",label=TRUE)
# test and validate

# with cross validation
cvfit= cv.glmnet(x=X, y=Y,family="gaussian",alpha=1,nfold=10)
plot(cvfit)

#cvfit$lambda.min

#scaled
Test_matrix = as.matrix(test_)
test_X = Test_matrix[,1:13]
test_Y = Test_matrix[,14]

# predict
lasso_pred <- predict(lasso_model,newx = test_X,s=cvfit$lambda.min)

# predict value de-scaled
descaled_lasso_pred<-lasso_pred*(max(data$medv)-min(data$medv))+min(data$medv)
# true value
descaled_lasso_test<- test_Y*(max(data$medv)-min(data$medv))+min(data$medv)

lasso_MSE = sum(( descaled_lasso_test - descaled_lasso_pred)^2)/nrow(test_X)


# Lasso  standardizing X
library(MASS)
set.seed(790) 
index<-sample(nrow(Boston), nrow(Boston)*0.9)
Boston.train<-Boston[index,]
Boston.test <-Boston[-index,]

index <- sample(nrow(Boston),nrow(Boston)*0.80)
Boston.X.std<- scale(select(Boston,-medv))
X.train<- as.matrix(Boston.X.std)[index,]
X.test<-  as.matrix(Boston.X.std)[-index,]
Y.train<- Boston[index, "medv"]
Y.test<- Boston[-index, "medv"]

lasso.fit<- glmnet(x=X.train, y=Y.train, family = "gaussian", alpha = 1)
plot(lasso.fit, xvar = "lambda", label=TRUE)

cv.lasso<- cv.glmnet(x=X.train, y=Y.train, family = "gaussian", alpha = 1, nfolds = 10)
plot(cv.lasso)

pred.lasso.train<- predict(lasso.fit, newx = X.train, s=cv.lasso$lambda.min)
pred.lasso.min<- predict(lasso.fit, newx = X.test, s=cv.lasso$lambda.min)
pred.lasso.1se<- predict(lasso.fit, newx = X.test, s=cv.lasso$lambda.1se)
#Lasso MSE
mean((Y.train-pred.lasso.train)^2)

mse1<-mean((Y.test-pred.lasso.min)^2)
## [1] 20.27092
mse2<-mean((Y.test-pred.lasso.1se)^2)


# random forest
library(randomForest)
library(MASS)
set.seed(790) 
index<-sample(nrow(Boston), nrow(Boston)*0.9)
Boston.train<-Boston[index,]
Boston.test <-Boston[-index,]

rf_model<-randomForest(f,data=Boston.train)
plot(rf_model)

# for each mtry
rf_mse<-double(13)
# randomly choose feature/variable m
for(m in 1:13){
  rf_fit <- randomForest(f,data = Boston.train, mtry=m,ntree=200)  
  
  pred_rf<-predict(rf_fit,Boston.test)
  
  rf_mse[m]<-sum((  pred_rf  -  Boston.test$medv   )^2)/nrow(Boston.test)
}
rf_mse


plot(rf_fit)

