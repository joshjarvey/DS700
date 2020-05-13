library(forecast)
library(tseries)
library(readr)

################# Reading dataset and visually inspecting for transformations ################

#Reading in the completed abbeville dataset
abbeville = read.csv("C:/Users/joshj/Documents/DS700-Intro to Data Science/Final Assignment/Dataset_clean.csv")

#creating the timeseries using the dataset.
tsabbeville = ts(abbeville$Incoming.Examinations, start = c(2006,1), end = c(2013,12), frequency = 12)

#first we will plot the dataset for a visual inspection
plot(tsabbeville,main = "Incoming Examinations for Abbeville HC", ylab="Examinations")

#there is a slight increase in variability as time goes on, so we will use a log transformation in fitting the models. 
plot(log(tsabbeville),main = "Log Transformed Incoming Examinations for Abbeville HC", ylab="Log(Examinations)")


################# Model 1: Holt-Winters Model ################################################

#Create a holt-winters model using the ets() function from the forecast package.
#create a holt-winters model with all exponential components activated: level/error, trend, and seasonal.
#setting the model parameters to "AAA".
#its important to point out this is LOG() incoming exams, so data will have to be re-transformed using EXP() on the way out.
hw_model = ets(log(tsabbeville), model = "AAA")

#AIC = 211.35.
summary(hw_model)

#evaluating the model fit
#first: complete a residual plot to verify no trend.
plot(hw_model$residuals,main = "Residual Plot - Holt-Winters", ylab="Residuals")
#next: does the data fit the theoritical normal distribution? Check residuals against a qqplot.
qqnorm(hw_model$residuals,main = "Normal Q-Q Plot - Holt-Winters")
qqline(hw_model$residuals)

#checking accuracy of my model. RMSE = 0.2570473 (log units)
accuracy(hw_model)

#using the log tranformed holt-winters model to forecast 12 months in advance.
hw_pred12 = forecast(hw_model, 12)

#back-transforming the log() model to original scale.
hw_pred12$mean = exp(hw_pred12$mean)
hw_pred12$lower = exp(hw_pred12$lower)
hw_pred12$upper = exp(hw_pred12$upper)
hw_pred12$x = exp(hw_pred12$x)

#creating a table to view the forcasted values in original scale. 
hw_prediction_results = cbind(hw_pred12$mean, hw_pred12$lower,hw_pred12$upper)
dimnames(hw_prediction_results)[[2]] = c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
#review the results table
hw_prediction_results

#plotting the original data with the forecasted data
plot(hw_pred12, main = "Holt-Winters Forecasted Examinations for Abbeville HC", ylab = "Number of Examinations", xlab = "Time")


############### Model 2: ARIMA method (attempted with seasonality)############################

#Checking dataset for stationarity
#first check the number of differencing to perform to make data stationary. This helps determine the d term in the model. d = 1
ndiffs(log(tsabbeville)) 

#using auto.arima() to identify my p,q values (confirms my differencing = 1)
auto.arima(log(tsabbeville))

#Now building and reviewing my ARIMA model, with an attempt to include the seasonality.
#setting my arima model using those suggested parameters, with seasonality = 12.
arima_model = arima(log(tsabbeville), seasonal = list(order = c(4,1,0), period = 12))

#now check for autocorrelation on the model's residuals - there are no significant spikes for the first few lags.
Acf(arima_model$residuals, main = "Auto-Correlation for Abbeville")
#now check for partial autocorrelation on the model's residuals - there are no significant spikes for the first few lags.
Pacf(arima_model$residuals,main = "Partial Auto-Correlation for Abbeville")

#AIC = 103.61
summary(arima_model)

#evaluating the model fit
#first: complete a residual plot to verify no trend.
plot(arima_model$residuals,main = "Residual Plot - ARIMA", ylab="Residuals")
#next: does the data fit the theoritical normal distribution? Check residuals against a qqplot.
qqnorm(arima_model$residuals,main = "Normal Q-Q Plot - ARIMA")
qqline(arima_model$residuals)
#last: complete a Ljung-Box test on the residuals. This tests if all autocorrelations differ from being 0. 
#because the p-value is 0.1176, we cannot reject the null that all autocorrelations differ from 0. This is a good thing!
Box.test(arima_model$residuals, type = "Ljung-Box")

#now lets look at the accuracy measures of the model. Specifically the RMSE to compare to the HW above. 
accuracy(arima_model)

#Finally, we can forecast
#forecasting 12 months using the arima_model.
arima_pred12 = forecast(arima_model, 12)
#transforming the data back to the original scale, so we can review and plot it.
arima_pred12$mean = exp(arima_pred12$mean)
arima_pred12$lower = exp(arima_pred12$lower)
arima_pred12$upper = exp(arima_pred12$upper)
arima_pred12$x = exp(arima_pred12$x)

#creating a table to view the forcasted values in original scale. 
arima_prediction_results = cbind(arima_pred12$mean, arima_pred12$lower,arima_pred12$upper)
dimnames(arima_prediction_results)[[2]] = c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
#review the predictions table
arima_prediction_results

#plotting the original data with the forecasted data
plot(arima_pred12,main = "ARIMA Forecasted Examinations for Abbeville HC", ylab = "Number of Examinations", xlab = "Time")