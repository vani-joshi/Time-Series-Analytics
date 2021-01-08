
library(forecast)
library(zoo)

########################################################################################################

forest <- read.csv("Amazon.csv", stringsAsFactors = FALSE)
head(forest)
forest

forest$date <- as.Date(forest$date, format="%m/%d/%Y")
head(forest)

#Aggregating the fires
entire.fires <- aggregate(x = forest$fires,                
                          by = list(forest$date),              
                          FUN = sum)
head(entire.fires)
entire.fires

#time series dataset
fires.ts <- ts(entire.fires$x, 
               start = c(1999,1), end = c(2017,11), freq=12)
fires.ts
length(fires.ts)

## Use plot() to plot time series data  
plot(fires.ts, 
     xlab = "Year", ylab = "Number of Forest Fires", 
     ylim = c(1, 120000), main = "Brazil Annual Fires (from 1999 to 2017)", col = "blue")

#STL function
fires.stl <- stl(fires.ts, s.window = "periodic")
fires.stl

autoplot(fires.stl, main = "Brazil Fires Time Series Components")

#ACF plot of Time series dataset
Acf(fires.ts, lag.max = 12, main = "Autocorrelation for Fires Time Series Dataset")


#Data PArtition
nValid <- 45 
nTrain <- length(fires.ts) - nValid
train.ts <- window(fires.ts, start = c(1999, 1), end = c(1999, nTrain))
valid.ts <- window(fires.ts, start = c(1999, nTrain + 1), 
                   end = c(1999, nTrain + nValid))

length(valid.ts)
length(train.ts)


## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATED
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

hw.ZZZ <- ets(train.ts, model = "ZZZ", alpha = 0.2)
hw.ZZZ
#M,N,M

hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot HW predictions for original data, optimal smoothing parameters.
plot(hw.ZZZ.pred, 
     xlab = "Time", ylab = "Fires", ylim = c(1000, 90000), bty = "l",
     xaxt = "n", xlim = c(2014, 2018), 
     main = "Holt-Winter's Model with Automated Selection of Model Options", flty = 2) 
axis(1, at = seq(2014, 2018, 1), labels = format(seq(2014, 2018, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts)

## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

HW.ZZZ <- ets(fires.ts, model = "ZZZ", alpha =0.2)
HW.ZZZ 
#(M,N,M)

HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = c(0, 95))
HW.ZZZ.pred

plot(HW.ZZZ.pred, 
     xlab = "Time", ylab = "Fires", ylim = c(1000, 150000), bty = "l",
     xaxt = "n", xlim = c(2014, 2019), 
     main = "Holt-Winter's Model with Automated Selection of Model Options", flty = 2) 
axis(1, at = seq(2014, 2019, 1), labels = format(seq(2014, 2019, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)

## DE-TRENDING and DE-SEASONALIZING TIME SERIES USING REGRESSION
## CREATE TRAILING MA USING RESIDUALS.
## FORECAST USING REGRESSION AND TRAILING MA INTO FUTURE PERIODS.

# Fit a regression model with quadratic trend and seasonality.
reg.trend.seas <- tslm(fires.ts ~ trend + I(trend^2) + season)
summary(reg.trend.seas)
#Multiple R-squared:  0.7903,	Adjusted R-squared:  0.7775 
#season 7 to 12 statistically significant

# Create forecast for the 12 periods into the future.
reg.trend.seas.pred <- forecast(reg.trend.seas, h = 12, level = 0)
reg.trend.seas.pred

# Identify and display residuals for time series based on the regression
# (differences between actual and regression values in the same periods).
reg.trend.seas.res <- reg.trend.seas$residuals
reg.trend.seas.res

# Apply trailing MA with 12 periods in the window to residuals.
ma.trailing.res_12 <- rollmean(reg.trend.seas.res, k = 12, align = "right")
ma.trailing.res_12

# Create forecast for residuals for the 12 periods into the future.
ma.trailing.res_12.pred <- forecast(ma.trailing.res_12, h = 12, level = 0)
ma.trailing.res_12.pred

# To develop real forecast for 12 periods into the future, 
# combine regression forecast and trailing MA forecast for residuals.
ts.forecast.12 <- reg.trend.seas.pred$mean + ma.trailing.res_12.pred$mean
ts.forecast.12


round(accuracy(reg.trend.seas.pred$fitted+ma.trailing.res_12, fires.ts), 3) #two level model
round(accuracy(reg.trend.seas.pred$fitted, fires.ts), 3) #regression model
round(accuracy(snaive(fires.ts)$fitted, fires.ts), 3) #seasonal naive
round(accuracy(hw.ZZZ.pred, valid.ts), 3) #HW 
round(accuracy(HW.ZZZ.pred$fitted, fires.ts), 3)#HW  entire dataset 


#Two-level forecast with Regression model and AR(1) model for residuals.

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY. 

# Use tslm() function to create quadratic trend and seasonal model.
train.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.trend.season)
#Multiple R-squared:  0.7955,	Adjusted R-squared:  0.7797
#season 8 to 12 statistically significant

#Apply forecast() function to make predictions for ts with 
# trend and seasonal model in validation set.
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
train.trend.season.pred

# Plot predictions 
plot(train.trend.season.pred, 
     xlab = "Year", ylab = "Fires", ylim = c(1000, 150000), bty = "l",
     xaxt = "n", xlim = c(1999, 2019), 
     main = "Regression with Quadratic Trend and Seasonality", flty = 2) 
axis(1, at = seq(1999, 2019, 1), labels = format(seq(1999, 2019, 1)))
lines(train.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts)
legend(1999,125000, legend = c("Fires Time Series", "Regression for Training Data",
                             "Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(2017.15 - 3, 2017.15 - 3), c(0,  150000))
lines(c(2017.95, 2017.95), c(0,  150000))
text(2007, 150000, "Training")
text(2015.75, 150000, "Validation")
text(2018.95, 150000, "Future")
arrows(1998.05, 145000, 2014.2,  145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014, 145000, 2017.95,145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.95, 145000, 2019.5, 145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(train.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Fires Training Residuals")
Acf(valid.ts - train.trend.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Fires Validation Residuals")



## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH QUADRATIC TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## PLOT DATA AND IDENTIFY ACCURACY MEASURES.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)

res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 12).

Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Fires Training Residuals of Residuals")

# Create two-level model's forecast with quadratic trend and seasonality 
# regression + AR(1) for residuals for validation period.
#two level model
valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean

valid.df <- data.frame(valid.ts, train.trend.season.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Fires", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

round(accuracy(valid.two.level.pred, valid.ts), 3)


#FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY 
## FOR ENTIRE DATASET. FORECAST AND PLOT DATA, AND MEASURE ACCURACY.
# Use tslm() function to create quadratic trend and seasonality model.
trend.season <- tslm(fires.ts ~ trend + I(trend^2) + season)

# See summary of linear trend equation and associated parameters.
summary(trend.season)
#Multiple R-squared:  0.7903,	Adjusted R-squared:  0.7775

# Apply forecast() function to make predictions with quadratic trend and seasonal 
# model into the future 12 months.  
trend.season.pred <- forecast(trend.season, h = 12, level = 0)
trend.season.pred


# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 months.
residual.ar1 <- Arima(trend.season$residuals, order = c(1,0,0)) 
#residuals of trend and seasonality model with entire dataset..AR(1)--AR(1) with residuals of first level
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)
residual.ar1.pred
# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrealtion for the residual of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Fires Residuals of Residuals for Entire Data Set")

# Identify forecast for the future 12 periods as sum of quadratic trend and seasonal model
# and AR(1) model for residuals.
trend.season.ar1.pred <- trend.season.pred$mean + residual.ar1.pred$mean
trend.season.ar1.pred

# Create a data table with quadratic trend and seasonal forecast for 12 future periods,
# AR(1) model for residuals for 12 future periods, and combined two-level forecast for
# 12 future periods.
table.df <- data.frame(trend.season.pred$mean, 
                       residual.ar1.pred$mean, trend.season.ar1.pred)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df

# plot historical data, predictions for historical data, and forecast for 12 future periods.
plot(fires.ts, 
     xlab = "Year", ylab = "Fires", ylim = c(1000, 150000), bty = "l",
     xaxt = "n", xlim = c(1999, 2019), 
     main = "Two-Level Forecast: Regression with Trend and Seasonlity + AR(1)
     for Residuals", flty = 2) 
axis(1, at = seq(1999, 2019, 1), labels = format(seq(1999, 2019, 1)))
lines(trend.season$fitted, col = "blue", lwd = 2)
lines(trend.season.ar1.pred, col = "blue", lty = 5, lwd = 2)
legend(1999,125000, legend = c("Fires Series", 
                         "Two-Level Forecast for Entire Data", "Two-Level Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(2017.95, 2017.95), c(0,  150000))
text(2007, 150000, "Training")
text(2018.95, 150000, "Future")
arrows(1998.05, 145000, 2017.95,  145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.95, 145000, 2019.5, 145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(valid.two.level.pred, valid.ts), 3) #two level of train data
round(accuracy(trend.season$fitted + residual.ar1$fitted, fires.ts), 3) #two level model
round(accuracy(trend.season$fitted, fires.ts), 3) #quad trend and seasonality model
round(accuracy((snaive(fires.ts))$fitted, fires.ts), 3)


# Checking the Predictability of this TimeSeries data :

# Arima() function to fit AR(1) model for Amazon data :
fires.ar1<- Arima(fires.ts, order = c(1,0,0))
summary(fires.ar1)

# Creating differenced Amazon data using (lag-1) :
diff.fires.ts <- diff(fires.ts, lag = 1)
diff.fires.ts

# Use Acf() function to identify autocorrealtion for differenced 
# Amtrak Ridership, and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.fires.ts, lag.max = 12, 
    main = "Autocorrelation for Amazon Wildfire Data")

# ACF graph shows higher value for lags 1,2,3,11 & 12
# Implying that our timeseries data is predictable.

# Also, below is the summary equation for AR1 model 
# using historical data :
# Y(t) = 15435.127 + 0.6770 * Y(t-1)

# Here the Beta value is equal to 0.68 which is not close to 1
# Hence the Amazon data is definitely preditable.

# Data Partitioning :
length(fires.ts)

nValid <- 45 
nTrain <- length(fires.ts) - nValid
train.ts <- window(fires.ts, start = c(1999, 1), end = c(1999, nTrain))
valid.ts <- window(fires.ts, start = c(1999, nTrain + 1), 
                   end = c(1999, nTrain + nValid))

length(train.ts)
length(valid.ts)

# Identifying autocorrelation for training and validation
# using the Acf() function :

# Adjust margins
#par("mar")
#par(mar=c(1,1,1,1))

# 1. Seasonal ARIMA(1,1,2)(1,1,2) Model :

# Use Arima() function to fit ARIMA(1,1,2)(1,1,2) model for 
# trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
train.arima.seas <- Arima(train.ts, order = c(1,1,2), 
                          seasonal = c(1,1,2)) 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

# Use Acf() function to create autocorrelation chart of ARIMA(2,1,2)(1,1,2) 
# model residuals.
Acf(train.arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMA(2,1,2)(1,1,2) Model Residuals")
#  ->> Only Lag 11 is significant now.


# Plot ts data, ARIMA model, and predictions for validation period.
plot(train.arima.seas.pred, 
     xlab = "Time", ylab = "Fires", ylim = c(100, 150000), bty = "l",
     xaxt = "n", xlim = c(1999, 2020.25), 
     main = "Seasonal ARIMA(1,1,2)(1,1,2)[12] Model", lwd = 2, flty = 5) 
axis(1, at = seq(1999, 2020, 1), labels = format(seq(1999, 2020, 1)))
lines(train.arima.seas.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2000,2007, legend = c("Fires Time Series", 
                             "Seasonal ARIMA Forecast for Training Period",
                             "Seasonal ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017.15 - 3, 2017.15 - 3), c(0,  150000))
lines(c(2017.95, 2017.95), c(0,  150000))
text(2007, 150000, "Training")
text(2016, 150000, "Validation")
text(2018.95, 150000, "Future")
arrows(1998.05, 145000, 2014.1,  145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.2, 145000, 2017.95,145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.95, 145000, 2019.5, 145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# 2. AUTO ARIMA MODEL :

# We will use the auto.arima() function to fit the ARIMA model:
train.auto.arima <- auto.arima(train.ts)

# Summary() function to display Auto ARIMA model and its parameters :
summary(train.auto.arima)

# Applying forecast() function to make predictions for ts with 
# Auto ARIMA model for the validation set :  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")
#  ->> Lag 11 significant

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Fires", ylim = c(100, 150000), bty = "l",
     xaxt = "n", xlim = c(1999, 2020.25), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(1999, 2020, 1), labels = format(seq(1999, 2020, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2000,2007, legend = c("Fires Time Series", 
                             "Auto ARIMA Forecast for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017.95, 2017.95), c(0,  150000))
text(2007, 150000, "Training")
text(2016, 150000, "Validation")
text(2018.95, 150000, "Future")
arrows(1998.05, 145000, 2014.1,  145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.2, 145000, 2017.95,145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.95, 145000, 2019.5, 145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Accuracy metrics :
round(accuracy(train.arima.seas.pred, valid.ts), 3)
round(accuracy(train.auto.arima.pred, valid.ts), 3)

# FORECAST MODELS FOR THE ENTIRE DATA SET :

# 1. SEASONAL ARIMA FOR ENTIRE DATA SET :
# We will use the arima() function to fit a seasonal ARIMA(1,1,2)(1,1,2) model 
# for the entire data set :
arima.seas <- Arima(fires.ts, order = c(1,1,2), 
                    seasonal = c(1,1,2), method="CSS") 
# Summary() function to display auto ARIMA model and its parameters 
# for the entire data set :
summary(arima.seas)

# Applying forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 12 periods :
arima.seas.pred <- forecast(arima.seas, h = 12, level = c(80, 95))
arima.seas.pred

# Use Acf() function to create autocorrelation chart of seasonal ARIMA 
# model residuals :
Acf(arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of Seasonal ARIMA Model Residuals (Entire dataset)")

# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 12 future periods :
plot(fires.ts, 
     xlab = "Time", ylab = "Fires", ylim = c(100, 150000), bty = "l",
     xaxt = "n", xlim = c(1999, 2020.25), lwd = 2,
     main = "Seasonal ARIMA Model for the Entire Dataset") 
axis(1, at = seq(1999, 2020, 1), labels = format(seq(1999, 2020, 1)))
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1999,2300, legend = c("Fires Series", 
                             "Seasonal ARIMA Forecast", 
                             "Seasonal ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")


# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2017.95, 2017.95), c(0,  150000))
text(2007, 150000, "Training")
text(2018.95, 150000, "Future")
arrows(1998.05, 145000, 2017.95,  145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.95, 145000, 2019.5, 145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# 2. AUTO ARIMA MODELS FOR THE ENTIRE DATA SET :

# We will use the auto.arima() function to fit ARIMA model for entire data 
# set :
auto.arima.entire.data <- auto.arima(fires.ts)

# We will use the summary() functionto show auto ARIMA model and its 
# parameters for the entire data set :
summary(auto.arima.entire.data)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima.entire.data, h = 12, 
                            level = c(80,95))
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima.entire.data$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals - Entire dataset")
# -->> Lags 11 & 12 are significant

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 12 future periods.
plot(fires.ts, 
     xlab = "Time", ylab = "Amazon wildfires", ylim = c(100, 150000), bty = "l",
     xaxt = "n", xlim = c(1999, 2020.25), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(1999, 2020, 1), labels = format(seq(1999, 2020, 1)))
lines(auto.arima.entire.data$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2000,2007, legend = c("Fires Series", 
                             "Auto ARIMA Forecast", 
                             "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))

lines(c(2017.95, 2017.95), c(0,  150000))
text(2007, 150000, "Training")
text(2018.95, 150000, "Future")
arrows(1998.05, 145000, 2017.95,  145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.95, 145000, 2019.5, 145000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Accuracy metrics :
round(accuracy(arima.seas.pred$fitted, fires.ts), 3)
round(accuracy(auto.arima.pred$fitted, fires.ts), 3)
round(accuracy((snaive(fires.ts))$fitted, fires.ts), 3)
round(accuracy((naive(fires.ts))$fitted, fires.ts), 3)

