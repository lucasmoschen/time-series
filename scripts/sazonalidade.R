# sazonalidade
air <- AirPassengers;
plot(air)

# Z(t) = T(t) + S(t) + a_t

# dummies sazonais
# em series temporais sazonais pode usar a funcao cycle
Q <- factor(cycle(air))
t <- c(1:length(air))
air.new <- data.frame(air=air,t,Q)

mod <- lm( air~t+Q, data = air.new)
summary(mod)
yhat <- ts(mod$fitted.values, frequency = 12, start = c(1949, 1))
plot(yhat, col = 'red',  main = 'Valores Ajustados')
points(air.new$air)

# Médias moveis
# tendencia
trend <- filter(air.new$air, sides = 2, filter = rep(1/12, 12))

par(mfrow = c(3,1))
plot(air.new$air)
plot(trend)
plot(air.new$air-trend)

mod.S <- lm(air ~ Q-1, data=air.new)
summary(mod.S)
S <- ts(mod.S$fitted.values - mean(mod.S$coefficients), frequency = 12, start = c(1949,1))

par(mfrow = c(4,1))
plot(air.new$air)
plot(trend)
plot(S)
plot(air.new$air - trend - S)

# mais facilmente, podemos usar funçao decompose (usa Medias Moveis)
plot(decompose(air.new$air))

# podemos usar o LOESS para isso também
plot(stl(air.new$air, s.window = 12))

# testes para sazonalidade
# Kruskal-Wallis 
kruskal.test(air~Q, data = air.new )

# removemos tendencia
dec  <- decompose(air.new$air)
Z <- air.new$air-dec$trend
air.new$detrend <- Z
kruskal.test(detrend~Q, data = air.new )

# Friedman (vou ficar devendo)

# anova nos dados detrend
mod <- lm(Z~Q-1, na.action = na.omit)
summary(mod)

