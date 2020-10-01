## ----echo = F, warning = F, message=F-----------------------------------------------------------------
library(zoo)
library(dplyr)
library(forecast)
library(tseries)
library(bnstruct)
library(lmtest)
library(ggplot2)


## ---- echo = F----------------------------------------------------------------------------------------
bos = read.csv("data/BOS.csv")
plot(bos$Date.Local, bos$O3.Mean, main = 'Daily average level of O3 in Boston', 
                                  xlab = 'Date', ylab = 'Kg/m3')


## ---- echo = F----------------------------------------------------------------------------------------
bos[148:153,]


## ---- echo=F------------------------------------------------------------------------------------------
day <- as.Date(bos$Date.Local[duplicated(bos$Date.Local)])
bos[bos$Date.Local %in% c(toString(day - 1), toString(day), toString(day + 1)),]


## ---- echo = F----------------------------------------------------------------------------------------
bos[413, "O3.Mean"] = mean(bos[bos$Date.Local == toString(day),]$O3.Mean)
bos <- distinct(bos, Date.Local, .keep_all = TRUE)


## ---- echo = F----------------------------------------------------------------------------------------
o3 <- zooreg(bos$O3.Mean, order.by = as.Date(as.character(bos$Date.Local), format = "%Y-%m-%d"))
o3.ts <- as.ts(o3)
dates = seq(from = as.Date(time(o3[1])), by = "days", to = as.Date(time(o3[length(o3)])))

print(sum(is.na(o3.ts))/length(o3.ts))


## -----------------------------------------------------------------------------------------------------
o3.clean <- knn.impute(as.matrix(o3.ts), k = 30)
o3.clean <- as.ts(o3.clean)
plot(o3.clean, main = 'Daily average level of O3 in Boston (after imputation)', 
                xlab = 'Date', ylab = 'Kg/m3')


## -----------------------------------------------------------------------------------------------------
o3_train = o3.clean[1:(length(o3.clean)[1] - 365),]
o3_test = o3.clean[(length(o3.clean)[1] - 365 + 1):length(o3.clean)[1],]


## -----------------------------------------------------------------------------------------------------
mae <- function(ytrue, ypred)
{
    return(mean(abs(ytrue - ypred)))
}


## ---- echo = F----------------------------------------------------------------------------------------
n <- length(o3_train)
t <- seq(1,n)
size <- 2*365 + 7 + 1


## ----baseline, echo=F---------------------------------------------------------------------------------
baseline_model_day <- function(t) {
  return(as.numeric(o3_train[t[(size-7)]]))
}

prediction = rollapply(t, width = size, baseline_model_day)
baseline_mae <- mae(o3_train[size:n], prediction)

output = function(mae, title, prediction, data, dates, size) {
  print(mae)
  plot(dates[1:length(data)], data,
       main = title, xlab = "t")
  lines(dates[size:length(data)], prediction, col="red", lwd = 1)
  legend(
    x = dates[1],
    y = 0.07,
    legend = c('Real', 'Prediction'),
    col = c('black', 'red'),
    pch = c('', ''),
    lty = c(1, 1)
  )
}

output(baseline_mae, "Baseline model prediction", prediction, o3_train, dates, size)


## ----echo = F-----------------------------------------------------------------------------------------
freq = 31
g <- rep(c(1:freq), ceiling(n/freq))[1:n]
kruskal.test(o3_train, g = g)


## ----echo = F-----------------------------------------------------------------------------------------
freq = 365
g <- rep(c(1:freq), ceiling(n/freq))[1:n]
kruskal.test(o3_train, g = g)


## ----additive-decompose, echo=F-----------------------------------------------------------------------
stl_add_model_day <- function(t) {
  model <- stl(ts(o3_train[t[-c((size-6):size)]], frequency = freq), 
               s.window = "periodic", robust = T)
  prediction = forecast(model, h=7)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, stl_add_model_day)
stl_add_mae <- mae(o3_train[size:n], prediction[,7])

output(stl_add_mae, "Additive decompose 7 day forward", prediction[,7], o3_train, dates, size)


## ---- echo = F----------------------------------------------------------------------------------------
mae_compare <- c(1:7)
for (i in mae_compare) {
  mae_compare[i] = mae(o3_train[(size-7+i):(n-7+i)], prediction[,i])
}
barplot(mae_compare, names.arg = c(1:7), 
        main = 'MAE in the train data for different lags', 
        xlab = 'Lags', ylab = 'MAE')


## ----echo=F-------------------------------------------------------------------------------------------
model <- stl(ts(o3_train, frequency = freq), 
             s.window = "periodic", t.window = 2*365 + 1, robust = T)
plot(model)


## ---- echo = F, warning = F---------------------------------------------------------------------------
checkresiduals(model$time.series[,'remainder'], lag = 2*freq, lag.max = 2*freq)


## ----multiplicative-decompose, echo=F-----------------------------------------------------------------
data = log(o3_train + 1)
stl_mul_model_day <- function(t) {
  model <- stl(ts(data[t[-c((size-6):size)]], frequency = freq), 
               s.window = "periodic", robust = T)
  prediction = forecast(model, h=7)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, stl_mul_model_day)
stl_mul_mae <- mae(o3_train[size:n], exp(prediction[,7]) - 1)

output(stl_mul_mae, "Multiplicative decompose 7 days forward", exp(prediction[,7]) - 1, o3_train, dates, size)


## ---- echo = F----------------------------------------------------------------------------------------
mae_compare <- c(1:7)
for (i in mae_compare) {
  mae_compare[i] = mae(o3_train[(size-7+i):(n-7+i)], exp(prediction[,i]) - 1)
}
barplot(mae_compare, names.arg = c(1:7), 
        main = 'MAE in the train data for different lags', 
        xlab = 'Lags', ylab = 'MAE')


## ----echo=F-------------------------------------------------------------------------------------------
model <- stl(ts(data, frequency = freq), 
             s.window = "periodic", t.window = 2*365 + 1, robust = T)
plot(model)


## ---- echo = F, warning=F-----------------------------------------------------------------------------
checkresiduals(model$time.series[,'remainder'])


## ----regression, echo=F-------------------------------------------------------------------------------
Q = factor(c(rep(c(1:365), n/365), c(1:(n%%365))))

regression_model_day <- function(t) {
  # if (t[1] %% 100 == 0) {
  #   print(t[1]/n)
  # }
  train = data.frame(
    t = t[1:(size-7)],
    o3_train = o3_train[t[1:(size-7)]],
    Q = Q[t[1:(size-7)]]
  )
  mod = lm(o3_train~t+Q, data = train)
  prediction = predict(mod, data.frame(t=t[c((size - 6):size)], Q=Q[t[c((size - 6):size)]]))
  return(prediction)
}

prediction = rollapply(t, width = size, regression_model_day)
regression_mae <- mae(o3_train[size:n], prediction[,7])

output(regression_mae, "Regression 7 days forward", prediction[,7], o3_train, dates, size)


## ---- echo = F----------------------------------------------------------------------------------------
mae_compare <- c(1:7)
for (i in mae_compare) {
  mae_compare[i] = mae(o3_train[(size-7+i):(n-7+i)], prediction[,i])
}
barplot(mae_compare, names.arg = c(1:7), 
        main = 'MAE in the train data for different lags', 
        xlab = 'Lags', ylab = 'MAE')


## -----------------------------------------------------------------------------------------------------
train = data.frame(
  t = t,
  o3_train = o3_train,
  Q = Q
)
mod = lm(o3_train~t+Q, data = train)
checkresiduals(mod, lag = 2*freq, lag.max = 2*freq)


## ----additive-hw, echo=F------------------------------------------------------------------------------
hw_add_model_day = function(t) {
  # if (t[1] %% 100 == 0) {
  #   print(t[1]/n)
  # }
  # We had many difficulties fitting a Holt-Winters model. So we chose alpha and beta using the whole training dataset (doing little modifications) and we let the HoltWinters function optimize gamma. We found it a reasonable choice.
  mod = HoltWinters(ts(o3_train[t[1:(size-7)]], frequency = 365), 
                    beta = 0.001, gamma = NULL, seasonal = "additive", alpha = 0.02216133,
                    optim.start = c(gamma = 0.2962739))
  prediction = forecast(mod, 7)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, hw_add_model_day)
hw_add_mae <- mae(o3_train[size:n], prediction[,7])

output(hw_add_mae, "Additive Holt-Winters 7 day forward", prediction[,7], o3_train, dates, size)


## ---- echo = F----------------------------------------------------------------------------------------
mae_compare <- c(1:7)
for (i in mae_compare) {
  mae_compare[i] = mae(o3_train[(size-7+i):(n-7+i)], prediction[,i])
}
barplot(mae_compare, names.arg = c(1:7), 
        main = 'MAE in the train data for different lags', 
        xlab = 'Lags', ylab = 'MAE')


## ---- warning=F---------------------------------------------------------------------------------------
mod = HoltWinters(ts(o3_train, frequency = 365), seasonal = "additive")
checkresiduals(mod, lag = 2*freq, lag.max = 2*freq)


## ----multiplicative-hw, echo=F------------------------------------------------------------------------
data = o3_train + 1
hw_mult_model_day = function(t) {
  # if (t[1] %% 100 == 0) {
  #   print(t[1]/n)
  # }
  # We had many difficulties fitting a Holt-Winters model. So we chose alpha and beta using the whole training dataset (doing little modifications) and we let the HoltWinters function optimize gamma. We found it a reasonable choice.
  mod = HoltWinters(ts(data[t[1:(size-7)]], frequency = 365), alpha = 0.02306014, beta = 0.001, gamma = NULL, seasonal = "multiplicative")
  prediction = forecast(mod, 7)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, hw_mult_model_day)
hw_mult_mae <- mae(o3_train[size:n], prediction[,7] - 1)

output(hw_mult_mae, "Multiplicative Holt-Winters 7 days forward", prediction[,7] - 1, o3_train, dates, size)


## ---- echo = F----------------------------------------------------------------------------------------
mae_compare <- c(1:7)
for (i in mae_compare) {
  mae_compare[i] = mae(o3_train[(size-7+i):(n-7+i)], prediction[,i] - 1)
}
barplot(mae_compare, names.arg = c(1:7), 
        main = 'MAE in the train data for different lags', 
        xlab = 'Lags', ylab = 'MAE')


## ---- warning = F-------------------------------------------------------------------------------------
mod = HoltWinters(ts(o3_train+1, frequency = 365), seasonal = "multiplicative")
checkresiduals(mod, lag = 2*freq, lag.max = 2*freq)


## ---- echo = F----------------------------------------------------------------------------------------
par(mfrow = c(2,2))
acf(o3_train, lag.max = 365, main = 'ACF of the data')
pacf(o3_train, lag.max = 365, main = 'PACF of the data')
acf(o3_train, lag.max = 30, main = 'ACF of the data')
pacf(o3_train, lag.max = 30, main = 'PACF of the data')


## ---- echo=F------------------------------------------------------------------------------------------
arma_print <- function(ar, ma){
  model <- arima(o3_train, order = c(ar,0,ma))
  print(paste('Model ARMA(', ar, ',', ma, ')', sep = ''))
  print(paste('AIC = ', model$aic, sep = ''))
  print('p-values')
  print(coeftest(model)[,'Pr(>|z|)'])
  print('')
}
arma_print(1,2)
arma_print(2,1)
arma_print(3,1)
arma_print(3,2)
arma_print(2,3)


## ----arma-1, echo=F-----------------------------------------------------------------------------------
arma_model_day <- function(t) {
  model <- arima(o3_train[t[1:(size-7)]], order = c(3,0,1))
  prediction = forecast(model, h = 7)
  return(as.numeric(prediction$mean))
}
prediction = rollapply(t, width = size, arma_model_day)
arma_mae <- mae(o3_train[size:n], prediction[,7])
output(arma_mae, "ARIMA(3,0,1) 7 days forward", prediction[,7], o3_train, dates, size)


## ---- echo = F----------------------------------------------------------------------------------------
mae_compare <- c(1:7)
for (i in mae_compare) {
  mae_compare[i] = mae(o3_train[(size-7+i):(n-7+i)], prediction[,i])
}
barplot(mae_compare, names.arg = c(1:7), 
        main = 'MAE in the train data for different lags', 
        xlab = 'Lags', ylab = 'MAE')


## ----arma-2, echo=F, warning = F----------------------------------------------------------------------
arma_model_day <- function(t) {
  model <- arima(o3_train[t[1:(size-7)]], order = c(2,0,3))
  prediction = forecast(model, h=7)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, arma_model_day)
arma_mae <- mae(o3_train[size:n], prediction[,7])
output(arma_mae, "ARIMA(2,0,3) 7 days forward", prediction[,7], o3_train, dates, size)


## ---- echo = F----------------------------------------------------------------------------------------
mae_compare <- c(1:7)
for (i in mae_compare) {
  mae_compare[i] = mae(o3_train[(size-7+i):(n-7+i)], prediction[,i])
}
barplot(mae_compare, names.arg = c(1:7), 
        main = 'MAE in the train data for different lags', 
        xlab = 'Lags', ylab = 'MAE')


## -----------------------------------------------------------------------------------------------------
model <- arima(o3_train, order = c(3,0,1))
checkresiduals(model, lag = 2*freq, lag.max = 2*freq)


## ---- echo=F------------------------------------------------------------------------------------------
stl_arma_model_day <- function(t) {
  model <- stlm(ts(o3_train[t[-c((size-6):size)]], frequency = freq), 
               s.window = "periodic", robust = T)
  prediction <- forecast(model, h = 7, 
                         forecastfunction=function(x,h,level){
                              fit <- arima(x, order=c(3,0,1), include.mean=FALSE)
                         return(forecast(fit,h=N,level=level))})
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, stl_arma_model_day)
stl_arma_mae <- mae(o3_train[size:n], prediction[,7])
output(stl_arma_mae, "STL + ARIMA prediction", prediction[,7], o3_train, dates, size)


## ---- echo = F----------------------------------------------------------------------------------------
# n - k + 1 = size - 1 => k = n + 2 - size
# we want o3_test(new)[size] = o3_test(old)[size] 
o3_test = c(o3_train[(n+2-size):n], o3_test)
dates = dates[(n+2-size):length(dates)]

# updating n and t
n <- length(o3_test)
t <- c(1:n)


## ----baseline-test, echo=F----------------------------------------------------------------------------
baseline_model_day <- function(t) {
  return(as.numeric(o3_test[t[(size-7)]]))
}

prediction = rollapply(t, width = size, baseline_model_day)
baseline_mae <- mae(o3_test[size:n], prediction)
output(baseline_mae, "Baseline prediction", prediction, o3_test, dates = dates, size = size)


## ----multiplicative-decompose-test, echo=F------------------------------------------------------------
data = log(o3_test + 1)
stl_mul_model_day <- function(t) {
  model <- stl(ts(data[t[-c((size-6):size)]], frequency = freq), 
               s.window = "periodic", robust = T)
  prediction = forecast(model, h=7)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, stl_mul_model_day)
stl_mul_mae <- mae(o3_test[size:n], exp(prediction[,7]) - 1)
output(stl_mul_mae, "Multiplicative decompose prediction", exp(prediction[,7]) - 1, o3_test, dates = dates, size = size)


## ----regression-test, echo=F--------------------------------------------------------------------------
Q = factor(c(rep(c(1:365), n/365), c(1:(n%%365))))

regression_model_day <- function(t) {
  train = data.frame(
    t = t[1:(size-7)],
    o3_test = o3_test[t[1:(size-7)]],
    Q = Q[t[1:(size-7)]]
  )
  mod = lm(o3_test~t+Q, data = train)
  prediction = predict(mod, data.frame(t=t[size], Q=Q[t[size]]))
  return(prediction)
}

prediction = rollapply(t, width = size, regression_model_day)
regression_mae <- mae(o3_test[size:n], prediction)

output(regression_mae, "Regression prediction", prediction, o3_test, dates = dates, size = size)


## ----multiplicative-hw-test, echo=F-------------------------------------------------------------------
data = o3_test + 1
hw_mult_model_day = function(t) {
  # if (t[1] %% 100 == 0) {
  #   print(t[1]/n)
  # }
  mod = HoltWinters(ts(data[t[1:(size-7)]], frequency = 365), alpha = 0.02306014, beta = 0.001, gamma = NULL, seasonal = "multiplicative")
  prediction = forecast(mod, 7)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, hw_mult_model_day)
hw_mult_mae <- mae(o3_test[size:n], prediction[,7] - 1)

output(hw_mult_mae, "Multiplicative Holt-Winters prediction", prediction[,7] - 1, o3_test, dates = dates, size = size)


## ----arma-1-test, echo=F------------------------------------------------------------------------------
arma_model_day <- function(t) {
  model <- arima(o3_test[t[1:(size-7)]], order = c(3,0,1))
  prediction = forecast(model, h = 7)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, arma_model_day)
arma_mae <- mae(o3_test[size:n], prediction[,7])
print('MAE ARIMA(3,0,1)')
output(arma_mae, "ARIMA(3,0,1) prediction", prediction[,7], o3_test, dates = dates, size = size)


## ---- echo=FALSE--------------------------------------------------------------------------------------
models = c('Baseline','STL', 'Regression', 'HW', 'ARIMA')
mae_result <- c(baseline_mae, stl_mul_mae, regression_mae, hw_mult_mae, arma_mae)
mae_all <- data.frame(Model = models, MAE = mae_result)
ggplot(mae_all) + 
  geom_bar( aes(x = Model, y = MAE), stat = 'identity', fill="forestgreen", alpha=0.5) + 
  labs(x = 'Models', y = 'MAE', 
      title = "Comparing model's MAE in test data") + 
  theme(plot.title = element_text(hjust = 0.5))


## -----------------------------------------------------------------------------------------------------
o3_week <- c(1:floor(length(o3.clean)/7))
for(i in seq(1,length(o3.clean)-7, 7)){
  o3_week[ceiling(i/7)] = mean(o3.clean[i:(i+6)])
}
dates = seq(from = as.Date(time(o3[1])), to = as.Date(time(o3[length(o3)])), by = "weeks")[1:782]

plot(ts(o3_week), main = 'Weekly average level of O3 in Boston (after imputation)', 
     xlab = 'Week', ylab = 'Kg/m3')

o3_train_week = o3_week[1:(length(o3_week)[1] - 52)]
o3_test_week = o3_week[-c(1:(length(o3_week)[1] - 52))]


## ---- echo = F----------------------------------------------------------------------------------------
n <- length(o3_train_week)
t <- seq(1,n)
size <- 2*52 + 4 + 1


## ----baseline-week, echo=F----------------------------------------------------------------------------
baseline_model_week <- function(t) {
  return(as.numeric(o3_train_week[t[(size-4)]]))
}

prediction = rollapply(t, width = size, baseline_model_week)
baseline_mae <- mae(o3_train[size:n], prediction)

output_week = function(mae, title, prediction, data, dates, size) {
  print(mae)
  plot(dates[1:length(data)], data,
       main = title, xlab = "t")
  lines(dates[size:length(data)], prediction, col="red", lwd = 1)
  legend(
    x = dates[1],
    y = 0.07,
    legend = c('Real', 'Prediction'),
    col = c('black', 'red'),
    pch = c('', ''),
    lty = c(1, 1)
  )
}

output_week(baseline_mae, "Baseline model prediction", prediction, o3_train_week, dates, size)


## ----echo = F-----------------------------------------------------------------------------------------
freq = 4
g <- rep(c(1:freq), ceiling(n/freq))[1:n]
kruskal.test(o3_train_week, g = g)


## ----echo = F-----------------------------------------------------------------------------------------
freq = 52
g <- rep(c(1:freq), ceiling(n/freq))[1:n]
kruskal.test(o3_train_week, g = g)


## ----additive-decompose-week, echo=F------------------------------------------------------------------
stl_add_model_week <- function(t) {
  model <- stl(ts(o3_train_week[t[1:(size-4)]], frequency = freq), 
               s.window = "periodic", robust = T)
  prediction = forecast(model, h=4)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, stl_add_model_week)
stl_add_mae <- mae(o3_train_week[size:n], prediction[,4])

output_week(stl_add_mae, "Additive decompose prediction", prediction[,4], 
            o3_train_week, dates, size)


## ----echo=F-------------------------------------------------------------------------------------------
model <- stl(ts(o3_train_week, frequency = freq), 
             s.window = "periodic", t.window = 2*52 + 4, robust = T)
plot(model)


## ---- echo = F, warning = F---------------------------------------------------------------------------
checkresiduals(model$time.series[,3])


## ----multiplicative-decompose-week, echo=F------------------------------------------------------------
data = log(o3_train_week + 1)
stl_mul_model_week <- function(t) {
  model <- stl(ts(data[t[1:(size-4)]], frequency = freq), 
               s.window = "periodic", robust = T)
  prediction = forecast(model, h=4)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, stl_mul_model_week)
stl_mul_mae <- mae(o3_train_week[size:n], exp(prediction[,4]) - 1)


output_week(stl_mul_mae, "Multiplicative decompose prediction", exp(prediction[,4]) - 1,
            o3_train_week, dates, size)


## ----echo=F-------------------------------------------------------------------------------------------
model <- stl(ts(data, frequency = freq), 
             s.window = "periodic", t.window = 2*52 + 1, robust = T)
plot(model)


## ---- echo = F----------------------------------------------------------------------------------------
par(mfrow = c(1,2))
acf(model$time.series[,'remainder'], lag.max = size, main = 'ACF Remainder')
pacf(model$time.series[,'remainder'], lag.max = size, main = 'PACF Remainder')


## ----regression-week, echo=F--------------------------------------------------------------------------
Q = factor(c(rep(c(1:52), n/52), c(1:(n%%52))))

regression_model_week <- function(t) {
  # if (t[1] %% 100 == 0) {
  #   print(t[1]/n)
  # }
  train = data.frame(
    t = t[1:(size-4)],
    o3_train_week = o3_train_week[t[1:(size-4)]],
    Q = Q[t[1:(size-4)]]
  )
  mod = lm(o3_train_week~t+Q, data = train)
  prediction = predict(mod, data.frame(t=t[size], Q=Q[t[size]]))
  return(prediction)
}

prediction = rollapply(t, width = size, regression_model_week)
regression_mae <- mae(o3_train_week[size:n], prediction)

output_week(regression_mae, "Regression prediction", prediction, o3_train_week, dates, size)


## -----------------------------------------------------------------------------------------------------
train = data.frame(
  t = t,
  o3_train_week = o3_train_week,
  Q = Q
)
mod = lm(o3_train_week~t+Q, data = train)
checkresiduals(mod, lag = 2*freq, lag.max = 2*freq)


## ----additive-hw-week, echo=F-------------------------------------------------------------------------
hw_add_model_week = function(t) {
  # if (t[1] %% 100 == 0) {
  #   print(t[1]/n)
  # }
  # We had many difficulties fitting a Holt-Winters model. So we chose alpha and beta using the whole training dataset (doing little modifications) and we let the HoltWinters function optimize gamma. We found it a reasonable choice.
  mod = HoltWinters(ts(o3_train_week[t[1:(size-4)]], frequency = 52), alpha = 0.03640345, beta = 0.003648801, gamma = NULL, seasonal = "additive")
  prediction = forecast(mod, h=4)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, hw_add_model_week)
hw_add_mae <- mae(o3_train_week[size:n], prediction[,4])

output_week(hw_add_mae, "Additive Holt-Winters prediction", prediction[,4], o3_train_week, dates, size)


## ---- warning=F---------------------------------------------------------------------------------------
mod = HoltWinters(ts(o3_train_week, frequency = 52), seasonal = "additive")
checkresiduals(mod, lag = 2*freq, lag.max = 2*freq)


## ----multiplicative-hw-week, echo=F-------------------------------------------------------------------
data = o3_train_week + 1
hw_mult_model_week = function(t) {
  # if (t[1] %% 100 == 0) {
  #   print(t[1]/n)
  # }
  # We had many difficulties fitting a Holt-Winters model. So we chose alpha and beta using the whole training dataset (doing little modifications) and we let the HoltWinters function optimize gamma. We found it a reasonable choice.
  mod = HoltWinters(ts(data[t[1:(size-4)]], frequency = 52), alpha = 0.01237447, beta = 0.003307011, gamma = NULL, seasonal = "multiplicative")
  prediction = forecast(mod, h=4)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, hw_mult_model_week)
hw_mult_mae <- mae(o3_train_week[size:n], prediction[,4] - 1)

output_week(hw_mult_mae, "Multiplicative Holt-Winters prediction", 
            prediction[,4] - 1, o3_train_week, dates, size)


## -----------------------------------------------------------------------------------------------------
mod = HoltWinters(ts(o3_train_week, frequency = 52), seasonal = "multiplicative")
checkresiduals(mod, lag = 2*freq, lag.max = 2*freq)


## ---- echo = F----------------------------------------------------------------------------------------
par(mfrow = c(1,2))
acf(o3_train_week, lag.max = 52, main = 'ACF of the data')
pacf(o3_train_week, lag.max = 52, main = 'PACF of the data')


## ---- echo=F------------------------------------------------------------------------------------------
arma_print <- function(ar, ma){
  model <- arima(o3_train_week, order = c(ar,0,ma))
  print(paste('Model ARMA(', ar, ',', ma, ')', sep = ''))
  print(paste('AIC = ', model$aic, sep = ''))
  print('p-values')
  print(coeftest(model)[,'Pr(>|z|)'])
  print('')
}
arma_print(1,2)
arma_print(2,1)
arma_print(3,1)
arma_print(1,3)
arma_print(2,3)


## ----arma-1-week, echo=F------------------------------------------------------------------------------
arma_model_week <- function(t) {
  model <- arima(o3_train_week[t[1:(size-4)]], order = c(1,0,3))
  prediction = forecast(model, h = 4)
  return(as.numeric(prediction$mean))
}
prediction = rollapply(t, width = size, arma_model_week)
arma_mae <- mae(o3_train_week[size:n], prediction)
output_week(arma_mae, "ARIMA(1,0,3) prediction", prediction[,4], o3_train_week, dates, size)


## ----arma-2-week-2, echo=F, warning = F---------------------------------------------------------------
arma_model_week <- function(t) {
  model <- arima(o3_train_week[t[1:(size-4)]], order = c(3,0,1))
  prediction = forecast(model, h=4)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, arma_model_week)
arma_mae <- mae(o3_train_week[size:n], prediction[,4])
output_week(arma_mae, "ARIMA(3,0,1) prediction", prediction[,4], o3_train_week, dates, size)


## -----------------------------------------------------------------------------------------------------
model <- arima(o3_train_week, order = c(3,0,1))
checkresiduals(model, lag = 2*freq, lag.max = 2*freq)


## ---- echo=F------------------------------------------------------------------------------------------
stl_arma_model_week <- function(t) {
  model <- stl(ts(o3_train_week[t[1:(size-4)]], frequency = freq), 
               s.window = "periodic", robust = T)
  prediction <- forecast(model, h = 4, method = 'arima')
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, stl_arma_model_week)
stl_arma_mae <- mae(o3_train_week[size:n], prediction[,4])
output_week(stl_arma_mae, "STL + ARIMA prediction", prediction[,4], o3_train_week, dates, size)


## ---- echo = F----------------------------------------------------------------------------------------
# n - k + 1 = size - 1 => k = n + 2 - size
# we want o3_test(new)[size] = o3_test(old)[size] 
o3_test_week = c(o3_train_week[(n+2-size):n], o3_test_week)
dates = dates[(n+2-size):length(dates)]

# updating n and t
n <- length(o3_test_week)
t <- c(1:n)


## ----baseline-week-test, echo=F-----------------------------------------------------------------------
baseline_model_day <- function(t) {
  return(as.numeric(o3_test_week[t[(size-4)]]))
}

prediction = rollapply(t, width = size, baseline_model_day)
baseline_mae <- mae(o3_test_week[size:n], prediction)
output(baseline_mae, "Baseline prediction", prediction, o3_test_week, dates = dates, size = size)


## ----multiplicative-decompose-test-week, echo=F-------------------------------------------------------
data = log(o3_test_week + 1)
stl_mul_model_week <- function(t) {
  model <- stl(ts(data[t[1:(size-4)]], frequency = freq), 
               s.window = "periodic", robust = T)
  prediction = forecast(model, h=4)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, stl_mul_model_week)
stl_mul_mae <- mae(o3_test_week[size:n], exp(prediction[,4]) - 1)
output_week(stl_mul_mae, "Multiplicative decompose prediction", 
            exp(prediction[,4]) - 1, o3_test_week, dates, size)


## ----regression-test-week, echo=F---------------------------------------------------------------------
Q = factor(c(rep(c(1:52), n/52), c(1:(n%%52))))

regression_model_week <- function(t) {
  train = data.frame(
    t = t[1:(size-4)],
    o3_test_week = o3_test_week[t[1:(size-4)]],
    Q = Q[t[1:(size-4)]]
  )
  mod = lm(o3_test_week~t+Q, data = train)
  prediction = predict(mod, data.frame(t=t[size], Q=Q[t[size]]))
  return(prediction)
}

prediction = rollapply(t, width = size, regression_model_week)
regression_mae <- mae(o3_test_week[size:n], prediction)

output_week(regression_mae, "Regression prediction", prediction, 
            o3_test_week, dates, size)


## ----multiplicative-hw-test-week, echo=F--------------------------------------------------------------
data = o3_test_week + 1
hw_mult_model_week = function(t) {
  # if (t[1] %% 100 == 0) {
  #   print(t[1]/n)
  # }
  # We had many difficulties fitting a Holt-Winters model. So we chose alpha and beta using the whole training dataset (doing little modifications) and we let the HoltWinters function optimize gamma. We found it a reasonable choice.
  mod = HoltWinters(ts(data[t[1:(size-4)]], frequency = 52), alpha = 0.01237447, beta = 0.003307011, gamma = NULL, seasonal = "multiplicative")
  prediction = forecast(mod, 4)
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, hw_mult_model_week)
hw_mult_mae <- mae(o3_test_week[size:n], prediction[,4] - 1)

output_week(hw_mult_mae, "Multiplicative Holt-Winters prediction", 
            prediction[,4] - 1, o3_test_week, dates, size)


## ---- echo=F------------------------------------------------------------------------------------------
stl_arma_model_week <- function(t) {
  model <- stl(ts(o3_test_week[t[1:(size-4)]], frequency = freq), 
               s.window = "periodic", robust = T)
  prediction <- forecast(model, h = 4, method = 'arima')
  return(as.numeric(prediction$mean))
}

prediction = rollapply(t, width = size, stl_arma_model_week)
arma_mae <- mae(o3_test_week[size:n], prediction[,4])
output_week(arma_mae, "STL + ARIMA prediction", prediction[,4], o3_test_week, dates, size)


## ---- echo=FALSE--------------------------------------------------------------------------------------
models = c('Baseline', 'STL', 'Regression', 'HW', 'ARIMA + STL')
mae_result <- c(baseline_mae, stl_mul_mae, regression_mae, hw_mult_mae, arma_mae)
mae_all <- data.frame(Model = models, MAE = mae_result)
ggplot(mae_all) + 
  geom_bar( aes(x = Model, y = MAE), stat = 'identity', fill="forestgreen", alpha=0.5) + 
  labs(x = 'Models', y = 'MAE', 
      title = "Comparing model's MAE in test data") + 
  theme(plot.title = element_text(hjust = 0.5))

