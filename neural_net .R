library(MASS)
library(corrplot)
library(neuralnet)
library(glmnet)

# correlation plot 
#corrplot(cor(Boston), method = "circle", type ="lower", diag=TRUE)

# pseudo-random seed
set.seed(2020) 

# train test validation split
# 360 train
# 40 validation
# 106 test
train_num = 360
valid_num = 40
test_num =106

# training set index
index<-sample(train_num+valid_num, train_num)

# copy of 'Boston' dataset, and normalize
boston_data = Boston

maxs <- apply(boston_data, 2, max) 
mins <- apply(boston_data, 2, min)
boston_data <- as.data.frame(scale(boston_data, center = mins, scale = maxs - mins))

Boston_train<-boston_data[index,]

# validation index
valid_index<-vector(,40)
j=1
for(i in 1:400){
  if (is.element(i,index)!= TRUE){
    valid_index[j] <-i
    j=j+1
  }
}
# validation set
Boston_valid <-boston_data[valid_index,]

# test set
Boston_test<-boston_data[401:506,]


# training

# set random seed again, in case training calls a randome function 
set.seed(2020)

feature_names= names(boston_data)
f <- as.formula(paste("medv ~", paste(feature_names[!feature_names %in% "medv"], collapse = " + ")))
neural_net_fit <-neuralnet(f, rep=1,stepmax = 1e+06,data=Boston_train, hidden = c(8,8), algorithm = 'backprop' , learningrate = 0.002)

#plot
#plot(neural_net_fit,arrow.length = 0.2)
#quartz.save("neural_net_plot3by8.png", type="png")

# validation
valid_pred_medv <- neuralnet::compute(neural_net_fit,Boston_valid[,1:13])

#de-normalize
valid_pred_medv_denorm <- valid_pred_medv$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)

# compare with origional dataset medv
Boston_valid_denorm<-Boston[valid_index,]$medv
MSE_valid <- mean((valid_pred_medv_denorm - Boston_valid_denorm)^2)

# test
nnet_pred_medv <- neuralnet::compute(neural_net_fit,Boston_test[,1:13])

#de-normalize
pred_medv_denorm <- nnet_pred_medv$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)

# compare with origional dataset medv
Boston_test_denorm<-Boston[401:506,]$medv
MSE_test <- mean((pred_medv_denorm-Boston_test_denorm)^2)

