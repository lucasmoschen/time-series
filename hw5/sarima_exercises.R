## ----setup, include=FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- echo=F, warning=F, message=F--------------------------------------------------------------
library(readxl)
library(tseries)
library(fpp2)

consumo <- ts(read_excel("CONSUMO.XLS")$consumo, start = c(1984), frequency = 12)


## ----plot-consumo, echo=F-----------------------------------------------------------------------
autoplot(consumo, main = "Consumo trimestral de gasolina na Califórnia", 
                  xlab = 'Tempo (trimestre)', 
                  ylab = 'milhões de galões')


## ---- echo=F------------------------------------------------------------------------------------
x <- acf(consumo, lag.max = 48, plot = F)
x$lag <- x$lag * 12
plot(x, main = 'Autocorrelação Zt', 
        xlab="Lag (meses)")


## ---- echo=F------------------------------------------------------------------------------------

x <- acf(diff(consumo), lag.max = 48, plot = F)
x$lag <- x$lag * 12
plot(x, main = 'Autocorrelação Zt', 
        xlab="Lag (meses)")


## ---- echo=F------------------------------------------------------------------------------------
id <- auto.arima(consumo)
summary(id)


## ---- echo=F------------------------------------------------------------------------------------
lambda <- BoxCox.lambda(consumo)
consumo.bc <- BoxCox(consumo, lambda)

adf.test(consumo.bc)


## ---- echo=F------------------------------------------------------------------------------------
consumo.diff = diff(consumo.bc) 
adf.test(consumo.diff)


## ---- echo=F------------------------------------------------------------------------------------
consumo.diff = diff(consumo.diff, 12)
ggtsdisplay(consumo.diff)


## ---- echo = F----------------------------------------------------------------------------------
ARMA.res <- data.frame()
## valor máximo de p,q.
K <- 3
L <- K
for (p in 0:K) {
    for (q in 0:K) {
        model <- Arima(y = consumo,  
                        order = c(p,1,q), 
                        seasonal = c(0,1,1), 
                        lambda = "auto")
        ARMA.res <- rbind(ARMA.res, c(p,q,model$aic, model$bic, model$aicc))
    }
}
names(ARMA.res) = c('p', 'q','AIC', 'BIC', 'AICc')
print('Modelo com menor AIC')
print(ARMA.res[order(ARMA.res$AIC)[1],])
print('Modelo com menor AICc')
print(ARMA.res[order(ARMA.res$AICc)[1],])
print('Modelo com menor BIC')
print(ARMA.res[order(ARMA.res$BIC)[1],])


## ---- echo=F------------------------------------------------------------------------------------
est <- Arima(consumo, order = c(1,0,1), seasonal =  c(0,1,1), lambda = "auto")
summary(est)


## ---- echo=F------------------------------------------------------------------------------------
checkresiduals(est)


## ---- echo=F------------------------------------------------------------------------------------
jarque.bera.test(est$residuals)


## ---- echo=F------------------------------------------------------------------------------------
ggtsdisplay(est$residuals)


## ---- echo=F------------------------------------------------------------------------------------
est2 <- Arima(consumo, order = c(2,1,0), seasonal =  c(0,1,1), lambda = "auto")
summary(est2)


## ---- echo=F------------------------------------------------------------------------------------
checkresiduals(est2)


## ---- echo=F------------------------------------------------------------------------------------
jarque.bera.test(est2$residuals)


## ---- echo=F------------------------------------------------------------------------------------
autoplot(forecast(est, h=12, level = c(80, 90, 95)))


## ----visualize-serie-auscafe, echo=F------------------------------------------------------------
autoplot(auscafe, main = 'Gasto em cafés, restaurantes e comidas para levar na Austrália', 
                  ylab = '$ bilhões', 
                  xlab = 'Tempo')


## ---- echo=F------------------------------------------------------------------------------------
lambda <- BoxCox.lambda(auscafe)
bc_auscafe <- BoxCox(auscafe, lambda)
autoplot(bc_auscafe, main = 'Gasto em cafés, restaurantes e comidas para levar na Austrália (var. estabilizada)',
         ylab = '$ bilhões',
         xlab = 'Tempo') + 
annotate("text", x=1985, y=1.3, label= paste('lambda = ', round(lambda, digits = 3)))


## ---- echo = F----------------------------------------------------------------------------------
adf.test(bc_auscafe)


## ---- echo = F----------------------------------------------------------------------------------
d_bc_auscafe = diff(bc_auscafe, 1)
adf.test(d_bc_auscafe)


## ---- echo=F------------------------------------------------------------------------------------
par(mfrow = c(1,2))
acf(d_bc_auscafe)
pacf(d_bc_auscafe)


## ---- echo = F----------------------------------------------------------------------------------
dd_bc_auscafe = diff(d_bc_auscafe, 12)
adf.test(dd_bc_auscafe)


## ---- echo=F------------------------------------------------------------------------------------
par(mfrow = c(1,2))
acf(dd_bc_auscafe)
pacf(dd_bc_auscafe)


## ---- echo = F----------------------------------------------------------------------------------
ARMA.res <- data.frame()
K = 3
for (p in 0:K) {
  for (P in 0:K) {
    if (!(p == 1 & P == 3) & !(p == 3 & P == 1)){
      mod <- Arima(y = dd_bc_auscafe, order = c(p, 0, 1), seasonal = c(P, 0, 1))
      ARMA.res <- rbind(ARMA.res, c(p, P, mod$aic, mod$bic, mod$aicc))
    }
  }
}
names(ARMA.res) = c('p', 'P','AIC', 'BIC', 'AICc')


## ---- echo = F, message=F-----------------------------------------------------------------------
library(knitr)
kable(ARMA.res)


## ---- echo=F------------------------------------------------------------------------------------
mod1 <- Arima(auscafe, order = c(2, 1, 1), seasonal = c(3, 1, 1), lambda = "auto")
summary(mod1)


## ---- echo=F------------------------------------------------------------------------------------
mod2 <- Arima(auscafe, order = c(0, 1, 1), seasonal = c(0, 1, 1), lambda = "auto")
summary(mod2)


## ---- echo=F------------------------------------------------------------------------------------
checkresiduals(mod1)
jarque.bera.test(mod1$residuals)


## ---- echo=F------------------------------------------------------------------------------------
checkresiduals(mod2)
jarque.bera.test(mod2$residuals)


## ---- echo = F----------------------------------------------------------------------------------
auto.arima(auscafe)

