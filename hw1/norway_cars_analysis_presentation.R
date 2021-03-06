## ---- include = FALSE, message=FALSE--------------------------------------------------------------
#Importando bibliotecas
library(dplyr)
library(zoo)
library(forecast)
library(ggplot2)

setwd("~/Documents/GitHub/Time_Series/trabalho1")


## ---- warning=FALSE, message = FALSE, highlight=TRUE----------------------------------------------
cars_df  = read.csv('norway_new_car_sales_by_make.csv')
make = 'Ford'

make_df = subset(cars_df, Make == make)
make_df$Date <- zoo::as.yearmon(paste(make_df$Year, make_df$Month), "%Y %m")


## ---- echo = FALSE--------------------------------------------------------------------------------
ggplot(data = make_df, mapping = aes(x = Date, y = Quantity)) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method = 'loess') + 
  labs(title = 'Vendas de carros da marca Ford por mês', 
       x = 'Mês/Ano', 
       y = 'Vendas') + 
  theme(plot.title = element_text(hjust = 0.5))


## ---- include = FALSE-----------------------------------------------------------------------------
decomposition = stl(zooreg(make_df$Quantity, start = c(2007,1), frequency = 12), s.window = 12)


## ---- echo = FALSE--------------------------------------------------------------------------------
plot(decomposition, main = 'Loess (STL)')


## ---- include = FALSE-----------------------------------------------------------------------------
decomposition = decompose(zooreg(make_df$Quantity, start = c(2007,1), frequency = 12), 
                          type = 'additive'
                          )


## ---- echo = FALSE--------------------------------------------------------------------------------
par(mfrow=c(4,1), mai = c(0.1, 0.6, 0.3, 0.1))
plot(decomposition$x, xlab = 'Ano', ylab = 'Dados', 
     main = 'Moving Average (Aditivo)')
plot(decomposition$trend, xlab = 'Ano', ylab = 'T')
plot(decomposition$seasonal, xlab = 'Ano', ylab = 'S')
plot(decomposition$random, xlab = 'Ano', ylab = 'e')


## ---- include = FALSE-----------------------------------------------------------------------------
decomposition = decompose(zooreg(make_df$Quantity, start = c(2007,1), frequency = 12), 
                          type = 'multiplicative'
                          )


## ---- echo = FALSE--------------------------------------------------------------------------------
par(mfrow=c(4,1), mai = c(0.1, 0.6, 0.3, 0.1))
plot(decomposition$x, xlab = 'Ano', ylab = 'Dados')
plot(decomposition$trend, xlab = 'Ano', ylab = 'T')
plot(decomposition$seasonal, xlab = 'Ano', ylab = 'S')
plot(decomposition$random, xlab = 'Ano', ylab = 'e')


## ---- message = FALSE, warning=FALSE--------------------------------------------------------------
D <- factor(cycle(make_df$Date))
t <- seq(1:length(make_df$Date))

make_df_model <- data.frame(Q = make_df$Quantity, t = t, D = D)

pm = function(z, degree){
  j <- nrow(z)-1
  model <- lm(formula = Q ~ poly(t, degree) + D, data = as.data.frame(z)[1:j,])
  y <- as.data.frame(z)[nrow(z),]
  yhat <- predict(model, y)
  return(yhat)
}

width = 25

mape_poly <- rep(0, 5)
yhat <- matrix(nrow = nrow(make_df_model)-width+1, ncol = 5)
for(degree in seq(1,5)){
  for(i in seq(1, nrow(make_df_model)-width+1)){
    j <- width + i - 1
    y <- make_df_model[i:j,]
    yhat[i,degree] <- pm(y, degree)
    y <- make_df_model[j,1]
    mape_poly[degree] <- mape_poly[degree] + abs((y - yhat[i,degree])/y)
  }
  mape_poly[degree] <- mape_poly[degree]/(nrow(make_df_model)-width+1)  
}
mape_poly <- data.frame(degree = seq(1,5), MAPE = mape_poly)


## ---- warning=FALSE, highlight=TRUE---------------------------------------------------------------
qtd <- zooreg(make_df$Quantity, frequency = 12, start = c(2007,1))
Q <- factor(cycle(qtd))
t <- c(1:length(qtd))
make_df.new <- data.frame(Quantity=qtd, t, Q)
mod <- lm(Quantity~poly(t,3)+Q, data = make_df.new)
zhat <- predict(mod, newdata = data.frame(t=t[length(t)]+c(1,2,3), Q=levels(Q)[1:3]))
zhat


## ---- echo = FALSE, messsage = FALSE--------------------------------------------------------------
ggplot(mape_poly) + 
  geom_bar( aes(x = degree, y = MAPE), stat = 'identity', fill="forestgreen", alpha=0.5) + 
  labs(x = 'Ordem polinomial', y = 'MAPE', 
      title = 'Comparando MAPE para diferentes polinômios') + 
  theme(plot.title = element_text(hjust = 0.5))


## ---- echo = F------------------------------------------------------------------------------------
poly_prevision <- data.frame(yhat)
poly_prevision$index <- make_df[25:nrow(make_df),]$Date

ggplot(data = make_df, mapping = aes(x = Date, y = Quantity)) + 
  geom_point() + 
  labs(title = 'Modelo Polinomial', 
       x = 'Mês/Ano', 
       y = 'Vendas',
       colour = 'Legend') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_line(data = poly_prevision, mapping = aes(x = index, y = X1, color = '1'), 
            linetype = "dashed") + 
  geom_line(data = poly_prevision, mapping = aes(x = index, y = X2, color = '2'),
            linetype = "dotted") + 
  geom_line(data = poly_prevision, mapping = aes(x = index, y = X3, color = '3'), 
            alpha = 0.5) + 
  scale_color_manual(values = c("blue","red", "green")) + 
  guides(fill=guide_legend(ncol=3), label = TRUE) + 
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))


## -------------------------------------------------------------------------------------------------
exp.smooth <- HoltWinters(ts(make_df_model, frequency = 12, start = 2007), beta = F, gamma = F)
plot(exp.smooth$fitted, main= 'Exponencial Smoothing')


## -------------------------------------------------------------------------------------------------
es <- function(x){
  model <- HoltWinters(x, beta = F, gamma = F)
  coef(model)
}

make_df.ts <- ts(make_df_model$Q, frequency = 12, start = 2007)

r.exp <- rollapply(make_df.ts, FUN = es, 
                   width = 24, align = 'right')

mape_exp <- mean(abs((make_df.ts - r.exp)/make_df.ts))


## ---- echo = F------------------------------------------------------------------------------------
plot(make_df$Date, make_df$Quantity, xlab = 'Anos', ylab = 'Quantidade de vendas', 
                                     main = 'Previsão do Primeiro Passo ES')
lines(r.exp)


## ---- echo = F------------------------------------------------------------------------------------
holt <- HoltWinters(make_df.ts)
plot(holt$fitted, main = 'Holt Winters')


## -------------------------------------------------------------------------------------------------
h <- function(x){
  model <- HoltWinters(x, gamma = F)
  model$fitted
}

r.h <- rollapply(make_df.ts, FUN = h, width = 24, align = 'right')

mape_hw <- mean(abs((make_df.ts - r.h[,1])/make_df.ts))


## -------------------------------------------------------------------------------------------------
plot(make_df$Date, make_df$Quantity, xlab = 'Anos', ylab = 'Quantidade de vendas', 
                                     main = 'Previsão do Primeiro Passo Holt')
lines(r.h[,1])


## -------------------------------------------------------------------------------------------------
h2 <- function(x){
  model <- HoltWinters(ts(x, frequency = 12), seasonal = 'multiplicative')
  model$fitted
}

r.h2 <- rollapply(make_df.ts, FUN = h2, width = 24, align = 'right')

mape_hw2 <- mean(abs((make_df.ts - r.h2[,1])/make_df.ts))


## -------------------------------------------------------------------------------------------------
plot(make_df$Date, make_df$Quantity, xlab = 'Anos', ylab = 'Quantidade de vendas', 
                                     main = 'Previsão do Primeiro Passo Holt')
lines(r.h2[,1])


## ---- echo=FALSE----------------------------------------------------------------------------------
models = c('Poly 1', 'Poly 2', 'Poly 3', 'Poly 4', 'Poly 5', 
          'ES', 'Holt', 'Holt Winters')
mape <- c(mape_poly$MAPE, mape_exp, mape_hw, mape_hw2)
mape_all <- data.frame(Model = models, MAPE = mape)

ggplot(mape_all) + 
  geom_bar( aes(x = Model, y = MAPE), stat = 'identity', fill="forestgreen", alpha=0.5) + 
  labs(x = 'Modelos', y = 'MAPE', 
      title = 'Comparando MAPE para os modelos') + 
  theme(plot.title = element_text(hjust = 0.5))

