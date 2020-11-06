## ----load_libraries, echo=F, message=F, warning=F-----------------------------
library(fpp2)
library(forecast)
library(tseries)


## ---- echo=F------------------------------------------------------------------
autoplot(wmurders, main = 'Total de mulheres assassinadas por 100 mil habitantes (EUA)',
               xlab = 'Tempo (anual)', ylab = 'Total')


## ---- echo=F------------------------------------------------------------------
lambda <- BoxCox.lambda(wmurders)
bcwmurders <- BoxCox(wmurders, lambda)
autoplot(bcwmurders, 
         main = 'Total de mulheres assassinadas por 100 mil habitantes (EUA)',
         xlab = 'Tempo (anual)', ylab = 'Total') + 
annotate("text", x=1955, y=1.3, label= paste('lambda = ', round(lambda, digits = 3)))


## ----echo=F-------------------------------------------------------------------
adf.test(bcwmurders, alternative = 'stationary')


## ---- echo=F------------------------------------------------------------------
dwmurders <- diff(bcwmurders)
autoplot(dwmurders, main = 'Total de mulheres assassinadas por 100 mil habitantes (EUA)',
         xlab = 'Tempo (anual)', ylab = 'Total anual')


## ----echo=F-------------------------------------------------------------------
adf.test(dwmurders)


## ---- echo=F------------------------------------------------------------------
par(mfrow = c(1,2))
acf(dwmurders)
pacf(dwmurders)


## ---- echo = F----------------------------------------------------------------
ARMA.res <- data.frame()
## valor máximo de p,q.
K <- trunc(log(length(dwmurders)))
L <- K
for (p in 0:K) {
    for (q in 0:K) {
        model <- Arima(y = dwmurders, order = c(p, 0, q))
        
        ARMA.res <- rbind(ARMA.res, c(p,q,model$aic, model$bic, model$aicc))
    }
}
names(ARMA.res) = c('p', 'q','AIC', 'BIC', 'AICC')


## ---- echo = F, message=F-----------------------------------------------------
library(knitr)
kable(ARMA.res)


## ---- echo=F------------------------------------------------------------------
model <- Arima(wmurders, order = c(0,1,2), lambda = "auto")
summary(model)


## ---- echo=F------------------------------------------------------------------
checkresiduals(model)
jarque.bera.test(model$residuals)


## ---- echo=F------------------------------------------------------------------
forecast(model, h = 3) %>% autoplot()


## ---- echo = F----------------------------------------------------------------
autoplot(debitcards, main = 'Uso de cartão de débito no varejo na Islândia',
               xlab = 'Tempo (mensal)', ylab = 'million ISK')


## ---- echo=F------------------------------------------------------------------
lambda <- BoxCox.lambda(debitcards)
bcdebitcards <- BoxCox(debitcards, lambda)
autoplot(bcdebitcards, main = 'Uso de cartão de débito no varejo na Islândia',
               xlab = 'Tempo (mensal)', ylab = 'million ISK') + 
annotate("text", x=2002, y=3.6, label= paste('lambda = ', round(lambda, digits = 3)))


## ---- echo=F------------------------------------------------------------------
adf.test(bcdebitcards)


## ---- echo=F------------------------------------------------------------------
ddebitcards <- diff(bcdebitcards)
autoplot(ddebitcards, main = 'Uso de cartão de débito no varejo na Islândia',
               xlab = 'Tempo (mensal)', ylab = 'million ISK mensal')


## ----echo=F-------------------------------------------------------------------
adf.test(ddebitcards)


## ---- echo=F------------------------------------------------------------------
par(mfrow = c(1,2))
acf(ddebitcards)
pacf(ddebitcards)


## ---- echo=F------------------------------------------------------------------
freq = 12
g <- factor(rep(c(1:freq), ceiling(length(ddebitcards)/freq))[1:length(ddebitcards)])
kruskal.test(ddebitcards, g)


## ---- echo=F------------------------------------------------------------------
ddebitcards <- diff(ddebitcards, 12)
autoplot(ddebitcards, main = 'Uso de cartão de débito no varejo na Islândia',
               xlab = 'Tempo (mensal)', ylab = 'million ISK mensal')


## ---- echo=F------------------------------------------------------------------
adf.test(ddebitcards)


## ---- echo=F------------------------------------------------------------------
par(mfrow = c(1,2))
acf(ddebitcards, lag = 48)
pacf(ddebitcards, lag = 48)


## ---- echo = F----------------------------------------------------------------
ARMA.res <- data.frame()
## valor máximo de p,q.
K <- trunc(log(length(ddebitcards)))
L <- K
for (p in 0:K) {
    for (q in 0:K) {
        model1 <- Arima(y = ddebitcards, order = c(p, 0, q))
        model2 <- Arima(y = ddebitcards, order = c(p, 0, q), seasonal = c(0,0,1))
        ARMA.res <- rbind(ARMA.res, c(p,q,model1$aic, model1$bic, model1$aicc,
                                          model2$aic, model2$bic, model2$aicc))
    }
}
names(ARMA.res) = c('p', 'q','AIC', 'BIC', 'AICc', 'AIC (S)', 'BIC (S)', 'AICc (S)')


## ---- echo = F, message=F-----------------------------------------------------
library(knitr)
kable(ARMA.res)


## ---- echo=F------------------------------------------------------------------
model <- Arima(debitcards, order = c(2,1,3), seasonal = c(0,1,1), lambda = "auto")
summary(model)


## ---- echo=F------------------------------------------------------------------
checkresiduals(model)
jarque.bera.test(model$residuals)


## ---- echo=F------------------------------------------------------------------
model <- Arima(debitcards, order = c(2,1,3), seasonal = c(0,1,0), lambda = "auto")
checkresiduals(model)
jarque.bera.test(model$residuals)


## ---- echo=F------------------------------------------------------------------
forecast(model, h = 3) %>% autoplot()


## ---- echo=F------------------------------------------------------------------
autoplot(auscafe, main = 'Gastos mensais em comer fora na Austrália',
               xlab = 'Tempo (mensal)', ylab = 'Billion dolars')


## ---- echo=F------------------------------------------------------------------
lambda <- BoxCox.lambda(auscafe)
bcauscafe <- BoxCox(auscafe, lambda)
autoplot(bcauscafe, 
         main = 'Gastos mensais em comer fora na Austrália',
         xlab = 'Tempo (mensal)', ylab = 'Billion dolars') + 
annotate("text", x=1985, y=1.3, label= paste('lambda = ', round(lambda, digits = 3)))


## ---- echo=F------------------------------------------------------------------
adf.test(bcauscafe)


## ---- echo=F------------------------------------------------------------------
dbcauscafe <- diff(bcauscafe)
autoplot(dbcauscafe, main = 'Gastos mensais em comer fora na Austrália',
         xlab = 'Tempo (mensal)', ylab = 'Billion dolars')


## ---- echo=F------------------------------------------------------------------
adf.test(dbcauscafe)


## ---- echo=F------------------------------------------------------------------
par(mfrow = c(1,2))
acf(dbcauscafe)
pacf(dbcauscafe)


## ---- echo=F------------------------------------------------------------------
freq = 12
g <- factor(rep(c(1:freq), ceiling(length(dbcauscafe)/freq))[1:length(dbcauscafe)])
kruskal.test(dbcauscafe, g)


## ---- echo=F------------------------------------------------------------------
ddbcauscafe <- diff(dbcauscafe, 12)
autoplot(ddbcauscafe, main = 'Gastos mensais em comer fora na Austrália',
         xlab = 'Tempo (mensal)', ylab = 'Billion dolars')


## ---- echo=F------------------------------------------------------------------
adf.test(ddbcauscafe)


## ---- echo=F------------------------------------------------------------------
par(mfrow = c(1,2))
acf(ddbcauscafe, lag = 48)
pacf(ddbcauscafe, lag = 48)


## ---- echo = F----------------------------------------------------------------
ARMA.res <- data.frame()
## valor máximo de p,q.
K <- trunc(log(length(ddbcauscafe)))
L <- K
for (p in 0:K) {
    for (q in 0:K) {
        model1 <- Arima(y = ddbcauscafe, order = c(p, 0, q))
        model2 <- Arima(y = ddbcauscafe, order = c(p, 0, q), seasonal = c(0,0,1))
        ARMA.res <- rbind(ARMA.res, c(p,q,model1$aic, model1$bic, model1$aicc,
                                          model2$aic, model2$bic, model2$aicc))
    }
}
names(ARMA.res) = c('p', 'q','AIC', 'BIC', 'AICc', 'AIC (S)', 'BIC (S)', 'AICc (S)')


## ---- echo = F, message=F-----------------------------------------------------
library(knitr)
kable(ARMA.res)


## ---- echo=F------------------------------------------------------------------
model <- Arima(auscafe, order = c(5, 1, 4), seasonal = c(0,1,1), lambda = "auto")
summary(model)


## ---- echo=F------------------------------------------------------------------
checkresiduals(model)
jarque.bera.test(model$residuals)


## ---- echo=F------------------------------------------------------------------
forecast(model, h = 3) %>% autoplot()

