library(MASS)
library(corrplot)
library(neuralnet)
library(glmnet)

# correlation plot 
# corrplot(cor(Boston), method = "circle", type ="lower", diag=TRUE)

# train test validation split
# 360 train
# 40 validation
# 106 test
train_num <- 400
test_num <-106

#in sample

# copy of 'Boston' dataset, and normalize
boston_data <- Boston

maxs <- apply(boston_data, 2, max) 
mins <- apply(boston_data, 2, min)
boston_data <- as.data.frame(scale(boston_data, center = mins, scale = maxs - mins))

# set pseudo randome seed
set.seed(2020)

# k-fold validation
mse_df<-data.frame('valid_mse'=rep(-1,10),'test_mse'=rep(-1,10))

# timing the training

for(k in 1:10){
  # error-handling added
  # conjecture:
  # Back propagation has gradient vanishing/exploding issues
  # training can be slow or divergent depending on the gradient
  # error-handling skips the problemetic network   
  
  train_time_total <- 0
  set.seed(k)
  tryCatch({
    
    cat("training the",k,"th model","\n") 
    
    # training set index
    index<-sample(train_num+test_num, train_num)
    Boston_train<-boston_data[index,]
    
    # test set
    Boston_test<-boston_data[401:506,]
    
    
    # training
    feature_names<- names(boston_data)
    f <- as.formula(paste("medv ~", paste(feature_names[!feature_names %in% "medv"], collapse = " + ")))
    
    # self-defined function for approximation of ReLu
    softplus <- function(x) log(1 + exp(x))
    
    # timing starts
    start_time <- proc.time()
    neural_net_fit <-neuralnet(f, data=Boston_train, hidden = c(8), act.fct = softplus,  rep=5 , learningrate = 0.005)
    # timing ends
    end_time<-proc.time()
    train_time <- end_time - start_time
    cat("the",k,"th model","takes", train_time,"seconds" ,"\n\n") 
    
    train_time_total <- train_time_total+train_time
    #plot the network structure
    #plot(neural_net_fit,arrow.length = 0.2)
    #quartz.save("neural_net_plot.png", type="png")
    
    # test
    test_pred_medv <- neuralnet::compute(neural_net_fit,Boston_test[,1:13])
    
    #de-normalize
    test_pred_medv_denorm <- test_pred_medv$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
    
    # compare with origional dataset medv
    Boston_test_denorm<-Boston[401:506,]$medv
    MSE_test <- mean((test_pred_medv_denorm - Boston_test_denorm)^2)
    
    
    mse_df[k,'test_mse']<-MSE_test
  }, error=function(e){cat("ERROR : ", conditionMessage(e),"\n\n")})
  
}

