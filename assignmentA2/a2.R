## ---- echo=FALSE, warning=FALSE, message=FALSE----
library(ggplot2)
library(forecast)
library(TSA)
library(tseries)
library(Metrics)
library(lmtest)


## ---- echo=F---------------------------
tbs <- read.csv("data/TOTBUSSMNSA.csv")
tbs <- ts(tbs$TOTBUSSMNSA, start = c(1992, 1), end = c(2020, 9), frequency = 12)
tbs <- window(tbs, start = c(2002, 1), end = c(2014, 12))

autoplot(tbs, main = "Total de vendas nos negócios", 
              xlab = "Tempo", 
              ylab = "Milhões de dólares") + 
  geom_vline(xintercept = (2008 + 7/12), 
             color = 'red', 
             linetype = "dashed", 
             size = 1) + 
  annotate(geom="text", x=2008, y=750000, label="Crise",
              color="red")


## ---- echo=FALSE-----------------------
tbs_train <- window(tbs, start = c(2002, 1), end = c(2012,12))
tbs_test <- window(tbs, start = c(2013,1), end = c(2014,12))


## ---- echo=F---------------------------
pre_intervention <- window(tbs_train, start = c(2002,1), end = c(2008,6))
pos_intervention <- window(tbs_train, start = c(2008,7), end = c(2012,12))
autoplot(pre_intervention, main = "Série pré-intervenção", 
              xlab = "Tempo", 
              ylab = "Milhões de dólares")


## ---- echo=F---------------------------
lambda <- BoxCox.lambda(pre_intervention)
pre_intervention.bc <- BoxCox(pre_intervention, lambda)
print(paste("Obtemos lambda = ", lambda))
autoplot(pre_intervention.bc, main = "Série após transformação Box-Cox", 
              xlab = "Tempo", 
              ylab = "Milhões de dólares")


## ---- echo=F---------------------------
pre_intervention.d = diff(pre_intervention.bc)
autoplot(pre_intervention.d, main = "Diferença mensal de vendas nos negócios", 
              xlab = "Tempo", 
              ylab = "Milhões de dólares")


## ---- echo=F---------------------------
kruskal.test(pre_intervention.d, g = cycle(pre_intervention.d))


## ---- echo=F---------------------------
pre_intervention.ds <- diff(pre_intervention.d, 12)
ggtsdisplay(pre_intervention.ds)


## ---- echo=F---------------------------
adf.test(pre_intervention.bc)


## ---- echo = F-------------------------
ARMA.res <- data.frame()
## valor máximo de p,q.
K <- 3
L <- 2
for (p in 0:K) {
    for (q in 0:L) {
        model1 <- Arima(y = pre_intervention.bc, order = c(p, 1, q), seasonal = c(0,1,0))
        model2 <- Arima(y = pre_intervention.bc, order = c(p, 1, q), seasonal = c(0,1,1))
        ARMA.res <- rbind(ARMA.res, c(p,q,model1$aic, model1$bic, model1$aicc,
                                          model2$aic, model2$bic, model2$aicc))
    }
}
names(ARMA.res) = c('p', 'q','AIC', 'BIC', 'AICc', 'AIC (S)', 'BIC (S)', 'AICc (S)')


## ---- echo=F---------------------------
print(ARMA.res)


## ---- echo=F---------------------------
model.errors <- data.frame()

error.metrics <- function(actual, predicted, name){
  e1 <- mape(actual, predicted)
  e2 <- mae(actual, predicted)
  e3 <- rmse(actual, predicted)
  return(c(name, e1, e2, e3))
}


## ---- echo=FALSE-----------------------
model1 <- Arima(pre_intervention, 
                order = c(2,1,0), 
                seasonal = c(0,1,1),
                lambda = "auto")
summary(model1)

model.errors <- rbind(model.errors, 
                      error.metrics(pre_intervention, model1$fitted, 1))
colnames(model.errors) <- c('Modelo', 'MAPE', 'MAE', 'RMSE')


## ---- echo=F---------------------------
checkresiduals(model1)
jarque.bera.test(model1$residuals)


## ---- echo=FALSE-----------------------
model2 <- auto.arima(pre_intervention, 
           lambda = "auto")
summary(model2)

model.errors <- rbind(model.errors, 
                      error.metrics(pre_intervention, model2$fitted, 2))


## ---- echo=F---------------------------
checkresiduals(model2)
jarque.bera.test(model2$residuals)


## ---- echo=FALSE-----------------------
model3 <- auto.arima(pre_intervention, 
                     lambda = 0)
summary(model3)

model.errors <- rbind(model.errors, 
                      error.metrics(pre_intervention, model3$fitted, 3))


## ---- echo=F---------------------------
checkresiduals(model3)
jarque.bera.test(model3$residuals)


## ---- echo=FALSE-----------------------
model4 <- Arima(pre_intervention, 
                order = c(2,1,0), 
                seasonal = c(0,1,1),
                lambda = 0)
summary(model4)


## ---- echo=F---------------------------
checkresiduals(model4)
jarque.bera.test(model4$residuals)

model.errors <- rbind(model.errors, 
                      error.metrics(pre_intervention, model4$fitted, 4))


## ---- echo = F-------------------------
p1 <- ggplot(model.errors, aes(x=Modelo, y=MAPE)) + 
  geom_bar(stat = "identity")
p2 <- ggplot(model.errors, aes(x=Modelo, y=RMSE)) + 
  geom_bar(stat = "identity")
p3 <- ggplot(model.errors, aes(x=Modelo, y=MAE)) + 
  geom_bar(stat = "identity")

gridExtra::grid.arrange(p1,p2,p3, nrow = 1, top = 'Métricas de Resíduos')


## ---- echo=FALSE-----------------------
autoplot(tbs, main = "Total de vendas nos negócios", 
              xlab = "Tempo", 
              ylab = "Milhões de dólares") + 
  autolayer(forecast(model4, h = 78)) +
  geom_vline(xintercept = (2008 + 7/12), 
             color = 'red', 
             linetype = "dashed", 
             size = 1) + 
  annotate(geom="text", x=2008, y=750000, label="Crise",
              color="red")


## ---- echo=F---------------------------
n = length(pre_intervention)
m = length(pos_intervention)
a <- rep(0, m)
for (j in 1:m) {
  w <- ts(tbs_train[1:(n+j-1)], start = c(2002,1), frequency = 12)
  model <- Arima(w, order = c(2,1,0), 
                 seasonal = c(0,1,1),
                 lambda = 0)
  z_hat_j <- forecast(model, h = 1)$mean
  a[j] <- pos_intervention[j] - z_hat_j
  if(j==1){
    sigma2 <- model$sigma2
  }
}
Q <- sum(a^2)/sigma2
p_valor <- 1 - pchisq(q = Q, df = m)
print(paste("p-valor = ", p_valor, ", Estatística Q = ", Q))


## ---- echo=F---------------------------
tbs_train.bc <- BoxCox(tbs_train, lambda = 0)
mod.arimax <- arimax(tbs_train.bc, 
                     order = c(2, 1, 0),
                     seasonal = c(0, 1, 1),
                     xtransf=data.frame(Crise_a=1*
                                          (seq(tbs_train)==(n + 1)), 
                                        Crise_b=1*
                                          (seq(tbs_train)==(n + 1))),
                     transfer=list(c(0,0), c(1,0)))
summary(mod.arimax)


## ---- echo = F-------------------------
coeftest(mod.arimax)


## ---- echo=F, warning=F----------------
checkresiduals(mod.arimax)
jarque.bera.test(mod.arimax$residuals[-1])


## ---- echo = F, warning = F, message=F----
x <- time(tbs_train.bc)
tbs_train.bc_df <- data.frame(x = x, y = tbs_train.bc)
fitted_model <- fitted(mod.arimax)

ggplot(tbs_train.bc_df, aes(x,y)) + 
  geom_point() +
  geom_line(aes(x, fitted_model), colour = 'blue') + 
  ggtitle("Comparação dos dados reais com o modelo") + 
  ylab('Valores log') + xlab('Tempo') 


## ---- echo=F---------------------------
k <- length(tbs_test)


## ---- echo = F-------------------------
delta <- as.numeric(mod.arimax$coef['Crise_b-AR1'])
xreg <- cbind(
  omega_0 = 1 * (seq(n+m+k)==(n + 1)), 
  omega_1 = filter(1 * (seq(n+m+k)==(n + 1)), 
                   filter = delta, 
                   method = "rec", 
                   sides = 1)
)
mod.arima = Arima(tbs_train, 
                order = c(2,1,0), 
                seasonal = c(0,1,1),
                lambda = 0,
                xreg = xreg[1:(n+m),])
mod.arima.pred <- forecast(mod.arima, h=24, xreg = xreg[(n+m+1):(n+m+k),])
summary(mod.arima)


## ---- echo=F---------------------------
autoplot(mod.arima.pred,
         level = c(90, 95),
         main = "Total de vendas nos negócios",
         xlab = "Tempo",
         ylab = "Milhões de dólares")


## ---- echo = F-------------------------
mape(tbs_test, mod.arima.pred$mean)


## ---- echo = F-------------------------
autoplot(window(tbs, start = c(2005, 1), end = c(2012,12)), 
         main = "Total de vendas nos negócios",
         xlab = "Tempo",
         ylab = "Milhões de dólares") +
  geom_vline(xintercept = 2008 + 7/12,
             color = 'red',
             linetype = "dashed",
             size = 0.5) +
  geom_vline(xintercept = 2009,
             color = 'red',
             linetype = "dashed",
             size = 0.5)


## ---- echo=F---------------------------
mod2.arimax <- arimax(tbs_train.bc,
                     order = c(2, 1, 0),
                     seasonal = c(0, 1, 1),
                     xtransf=data.frame(Crise=1*
                                       ((seq(tbs_train) >= n + 1) &
                                        seq(tbs_train) <= n + 1 + 6)),
                     transfer=list(c(1, 1)),
                     method = "ML")
summary(mod2.arimax)


## ---- echo = F-------------------------
coeftest(mod2.arimax)


## ---- echo=F, warning=F----------------
checkresiduals(mod2.arimax)
jarque.bera.test(mod2.arimax$residuals[-1])


## ---- echo = F, warning = F, message=F----
x <- time(tbs_train.bc)
tbs_train.bc_df <- data.frame(x = x, y = tbs_train.bc)
fitted_model <- fitted(mod2.arimax)
ggplot(tbs_train.bc_df, aes(x,y)) + 
  geom_point() +
  geom_line(aes(x, fitted_model), colour = 'blue') + 
  ggtitle("Comparação dos dados reais com o modelo") + 
  ylab('Valores log') + xlab('Tempo') 


## ---- echo = F-------------------------
delta <- as.numeric(mod2.arimax$coef['Crise-AR1'])
xreg <- cbind(
  omega_1 = filter(1 * ((seq(n+m+k)>=(n + 1)) & (seq(n+m+k)<=(n + 1 + 6))), 
                   filter = delta, 
                   method = "rec", 
                   sides = 1)
)
mod2.arima = Arima(tbs_train, 
                order = c(2,1,0), 
                seasonal = c(0,1,1),
                lambda = 0,
                xreg = xreg[1:(n+m)])
mod2.arima.pred <- forecast(mod2.arima, h=24, xreg = xreg[(n+m+1):(n+m+k)])
summary(mod2.arima)


## ---- echo=F---------------------------
autoplot(mod2.arima.pred,
         level = c(90, 95),
         main = "Total de vendas nos negócios",
         xlab = "Tempo",
         ylab = "Milhões de dólares")


## ---- echo = F-------------------------
mape(tbs_test, mod2.arima.pred$mean)

