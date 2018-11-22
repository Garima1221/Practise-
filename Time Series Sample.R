
#https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/
rm(list = ls())
setwd("C:/Users/Garima/Downloads/Train Series Sample")

data(AirPassengers)
class(AirPassengers)
#"ts"This tells you that the data series is in a time series format

start(AirPassengers)## Start of time series 

end(AirPassengers)## End of time series

frequency(AirPassengers)

summary(AirPassengers)

plot(AirPassengers)

abline(reg = lm(AirPassengers~time(AirPassengers)),col="red")

cycle(AirPassengers)

plot(aggregate(AirPassengers,FUN = mean)) ## Mean not constant 

boxplot(AirPassengers~cycle(AirPassengers))

##Important Inferences
#The year on year trend clearly shows that the #passengers have been increasing without fail.
#The variance and the mean value in July and August is much higher than rest of the months.
#Even though the mean value of each month is quite different their variance is small. Hence, we have strong seasonal effect with a cycle of 12 months or less.

################### ARMA (Auto Regression and Moving Average) ###########################
## applicable only for stationary dataset 

#AR(1) formulation: x(t) = alpha *  x(t - 1) + error (t)
#The numeral one (1) denotes that the next instance is solely dependent on the previous instance.
#Hence, any shock to x(t) will gradually fade off in future.
#For instance, let's say x(t) is the number of juice bottles sold in a city on a particular day. During winters, very few vendors purchased juice bottles. Suddenly, on a particular day, the temperature rose and the demand of juice bottles soared to 1000. However, after a few days, the climate became cold again. But, knowing that the people got used to drinking juice during the hot days, there were 50% of the people still drinking juice during the cold days. In following days, the proportion went down to 25% (50% of 50%) and then gradually to a small number after significant number of days. The following graph explains the inertia property of AR series:


#Let's take another case to understand Moving average time series model.

#  x(t) = beta *  error(t-1) + error (t)
#In MA model, noise / shock quickly vanishes with time. 
#The AR model has a much lasting effect of the shock.

#Difference between AR and MA models

#The primary difference between an AR and MA model is based on the correlation between time series objects at different time points. 
#The correlation between x(t) and x(t-n) for n > order of MA is always zero. 
#This directly flows from the fact that covariance between x(t) and x(t-n) is zero for MA models
#(something which we refer from the example taken in the previous section). 
#However, the correlation of x(t) and x(t-n) gradually declines with n becoming larger in the AR model. 
#This difference gets exploited irrespective of having the AR model or MA model. 
#The correlation plot can give us the order of MA model

## One Remove unequal variances,Two, we need to address the trend component. ###############
install.packages("tseries")
library(tseries)

adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)
## Stationary 
## k = lag order to calculate the test statistic

#### Parameters in  ARIMA ##############

#d = 1 as we need d = 1 for making series stationary 

## ACF plots 

acf(log(AirPassengers))
## Decay is slow ,thus population is not stationary 
##We have already discussed above that we now intend to regress on the difference of logs rather than log directly. Let's see how ACF and PACF curve come out after regressing on the difference.

acf(diff(log(AirPassengers)))

pacf(diff(log(AirPassengers)))

# After a few iterations, we found that (0,1,1) as (p,d,q) comes out to be the combination with least AIC and BIC.

#Let's fit an ARIMA model and predict the future 10 years. Also, we will try fitting in a seasonal component in the ARIMA formulation. Then, we will visualize the prediction along with the training data. You can use the following code to do the same :

(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

pred <- predict(fit, n.ahead = 10*12)

ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))



