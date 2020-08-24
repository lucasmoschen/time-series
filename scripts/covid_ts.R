#Importando bibliotecas
setwd("~/OneDrive/FGV/Eletivas ou Complementares/Séries Temporais")
library(zoo)
library(randtests)

#1. A séries é estacionária?
#2. Existe alguma sazonalidade?
#3. Existe alguma tendência?
#4. O modelo fica melhor na escala logaritmica? (modelo multiplicativo)
#5. O teste de sequencias aponta tendência?
#6. Algum dos testes estudados em sala aponta sazonalidade?

covid <- readxl::read_xlsx('dados/covidrj.xlsx')
deaths <- zooreg(diff(covid$deaths), frequency = 7)

# Normalização dos dados:
deaths_norm <- deaths#/max(deaths)
plot(deaths_norm, xlab = 'Semana Epidemiológica')

plot.new()
acf(deaths_norm, lag.max = length(deaths_norm), xlab = "lag #", ylab = 'ACF')

# 1. A série não é estacionácia. Pelo gráfico já é perceptível um fator de tendência e um fator sazonal. 
#    A variabilidade dos dados parece menor também no início. O gráfico de autocorrelação também mostra 
#    uma relação temporal nos dados. 

g <- factor(cycle(deaths_norm))
test_sazonality <- kruskal.test(as.numeric(deaths_norm), g)
print(test_sazonality)

# 2. Para testar sazonalidade, eu utilizo o teste Krustal para testar a hipótese dos coeficientes serem todos
#    nulos. A hipótese de independência entre cada amostra (no caso o dia da semana) faz sentido, dado que o 
#    registro de mortes pode ser considerado aleatório. A tendência pode atrapalhar essa hipótese. Mas nesse
#    teste tivemos p-valor << 0.05, então a hipótese nula é rejeitada. Ao usar as funções stl ou 
#    decompose, a série é dita não periódica ou tem menos do que 2 períodod. 

t <- seq(1, length(deaths_norm))
deaths.df <- data.frame(deaths = deaths_norm, t = t, g = g)
model <- lm(deaths ~ poly(t,3) + g, data = deaths.df)
summary(model)

plot.new()
y_hat <- ts(as.numeric(model$fitted.values), frequency = 7)
plot(deaths_norm)
lines(y_hat, col = 'red',  main = 'Valores Ajustados')

#3. Se fizermos um modelo polinomial, já podemos perceber que existe uma tendência crescente 
#   de início e de decréscimo de descida. Ao usar um filtro linear, também é possível visualizar
#   uma tendência. 

trend <- filter(deaths.df$deaths, sides = 2, filter = rep(1/7, 7)) # centred around lag 0
randtests::runs.test(deaths.df$deaths, alternative = 'two.sided', pvalue = 'exact', plot = T)

plot.new()
par(mfrow = c(3,1))
plot(deaths.df$deaths)
plot(trend)
plot(deaths.df$deaths-trend)

n <- end(deaths.df$deaths)[1]-4

test_sazonality2 <- kruskal.test((deaths.df$deaths-trend)[4:n], 
                                 g[4:n])
blocks <- rep(1,7)
for(i in seq(2,ceiling(length(deaths.df$deaths)/7))){
  blocks = c(blocks, rep(i, 7))
}

# Não consegui utilizar esse teste ainda. 
#test_sazonality3 <- friedman.test((deaths.df$deaths-trend)[4:n],
#                                  g[4:n], blocks[4:n])

print(test_sazonality2)
#print(test_sazonality3)

#4. O R^2 pioroui para esse modelo utilizando a mesma ordem polinomial. Ele parece ter
#   superestimado os valores intermediários. 

deaths.df$logdeaths = log(deaths.df$deaths + 1e-15)  #somar é interessante? 
model2 <- lm(logdeaths ~ poly(t,3) + g, data = deaths.df)
summary(model2)

plot.new()
par(mfrow = c(1,1))
y_hat2 <- ts(as.numeric(model2$fitted.values), frequency = 7)
plot(deaths.df$logdeaths)
lines(as.numeric(y_hat2), col = 'red',  main = 'Valores Ajustados')

#5. O teste de sequências indicou um p-valor baixo, logo rejeitamos a hipótese nula, o que 
#   indica uma não aleatoriedade no processo. Em particular uma tendência na série temporal. 

#6. O teste Krustal indica sazonalidade, dado que o p-valor (tanto da série com tendência, 
#   quanto a série sem tendência) é suficientemente baixo. 