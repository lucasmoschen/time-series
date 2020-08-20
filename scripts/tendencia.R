library(tseries)
#chicken?
# setwd(')
# carrego base de consumo (24 obs)
consumo <- readxl::read_xls('CONSUMO.XLS', n_max = 24)
plot(consumo)

# modelo polinomial
t = c(1:24)
consumo$t = t
mod <- lm(consumo~t, data=consumo)
summary(mod)
plot(consumo$t, consumo$consumo)
abline(mod$coefficients)
# PREDICTION
zhat <- predict(mod, newdata = data.frame(t=c(25, 26, 27)))

# modelo polinomial
modpoli <- lm(consumo ~ poly(t,3),data =consumo)
plot(modpoli)
yhat <- modpoli$fitted.values
plot(yhat,consumo$consumo)
abline(0,1, col=4, lwd=2)
yhat <- predict(modpoli, newdata = data.frame(t=seq(1,24,by=0.5)))
plot(consumo$t, consumo$consumo)
lines(seq(1,24,by=0.5),yhat, col = 2, lwd = 2)

# Médias moveis h=2n+1
w1 <- filter(consumo$consumo, sides=2, filter = rep(1/5,5)) # h=5
w2 <- filter(consumo$consumo, sides=2, filter = rep(1/13,13)) # h=13
plot(consumo$consumo, pch = '*', main = 'Consumo Energia  1984 - 1985')
lines(w1, col=2, lwd = 2)
lines(w2, col=4, lwd = 2)

# lowess
lw1 <- lowess(consumo$consumo, f=.6, iter = 0)
lw2 <- lowess(consumo$consumo, f=.2, iter = 0)
lw3 <- lowess(consumo$consumo, f=.2, iter = 2)
plot(consumo$consumo, lwd = 2, pch = '*', main = 'Consumo Energia  1984 - 1985', col = 'black')
lines(lw1, col='red', lwd = 2, pch='.')
lines(lw2, col='blue', lwd = 2, pch='.')
lines(lw3, col='green', lwd = 2, pch='.')
legend(x=2, y=220,
       legend = c('Data', 'lw1', 'lw2', 'lw3'), 
       col = c('black', 'red', 'blue', 'green'), 
       lty = c(0,1,1,1), pch=c('*','.','.','.'))

# diferenciação
dz = diff(consumo$consumo,1)
plot(dz)
acf(dz)
plot(dz)
points(mod$residuals, col=2, pch = '+')
abline(0,0)
mean(dz)
mean(mod$residuals)
abline(mean(dz),0)

# testando por tendencia usando o runs test (wald Wolfowitz)
library(randtests)
#?runs.test
randtests::runs.test(consumo$consumo, alternative = 'two.sided', pvalue = 'exact', plot = T)

# usando aproximação normal
randtests::runs.test(consumo$consumo, alternative = 'two.sided', pvalue = 'normal')

  
