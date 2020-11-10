# https://fred.stlouisfed.org/series/GDP
library(ggplot2)

# getwd()  <- working directory
# setwd("file path")

gdp_df<-read.csv("GDPC1.csv")

plot(gdp_df$DATE,gdp_df$GDPC1,main="quarterly gdp ")

# de-trend data
gdp_growth<-diff(log(gdp_df$GDPC1))*100


plot(gdp_growth)
average_gdp_growth_rate<-mean(gdp_growth)
abline(h=average_gdp_growth_rate,col="red")

# ARIMA
# Auto-Regressive Integrated Moving Average
# GDP(this year) = p*past GDP values + q*randomness(disturbance)
# p=1,  q=1

# GDP this year = GDP last year + random last year

# two functions that determine p and q for the model
acf(gdp_growth)
pacf(gdp_growth)

# fit model
arima_fit<-arima(x=gdp_growth,order=c(1,0,1))

# predicted_gdp<-predict(arima_fit, 1)
pred_gdps<-rep(0,120)

for(i in (1+10):(100+10)){
  # correction: make a model every 10 quarters.
  arima_fit_10<-arima(x=gdp_growth[(i-10):i],order=c(1,0,1))
  predicted_gdp<-predict(arima_fit_10)
  pred_gdps[i-10+1]<-gdp_df$GDPC1[i-10+1]*(1+predicted_gdp$pred/100)
}

# throwing out the 1st element because we can't predict it
pred_gdps<-pred_gdps[2:101]


gdp_df2<-data.frame('date'=1:100,'gdp'=gdp_df$GDPC1[1:100],'predicted'=pred_gdps)

p<-ggplot(gdp_df2, mapping = aes(x=date))
# 2nd y values should be predicted values
p+geom_line(aes(y=gdp))+geom_line(aes(y=pred_gdps),color="blue")+xlab("Date")+ylab("Quarterly GDP from 1947 to 1972")

