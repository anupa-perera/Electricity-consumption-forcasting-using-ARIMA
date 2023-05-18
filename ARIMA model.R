#load data set
data <- read_excel("C:/Users/Anupa/Desktop/BA/Data for ARIMA.xlsx")
View(data)

# Convert to time series
data_ts <- ts(data$`Electricity (MWh/month)`, frequency = 12, start = c(2011, 1))

# Load the forecast package
library(forecast)

#load tseries package
library(tseries)

#plot time series graph
plot(data_ts)

#adf test 
adf.test(data_ts) #suggest some stationarity

#checking auto correlation within the time series
acf(data_ts) #data is not stationary- crosses blue line

# Split the data into training and testing sets
train <- window(data_ts, end = c(2014, 8))
test <- window(data_ts, start = c(2014, 9))

#find the parameters of the best model 
model = auto.arima(train, ic="aic", trace = TRUE)
model

#checking auto correlation within the time series
acf(ts(model$residuals))  #data are stationary now

#checking partial auto correlation
pacf(ts(model$residuals))  #data are stationary now

#Make forecasts on the test data
forecasts <- forecast(model,level= c(95), h=length(test))
forecasts

plot(forecasts)

#Calculate the error metrics
mse <- mean((test - forecasts$mean)^2)
mad <- mean(abs(test - forecasts$mean))
mape <- mean(abs(test - forecasts$mean)/test)*100

# View the error metrics
print(paste("MSE:", mse))
print(paste("MAD:", mad))
print(paste("MAPE:", mape))


