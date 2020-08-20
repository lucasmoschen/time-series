#Importando bibliotecas
setwd("~/OneDrive/FGV/Eletivas ou Complementares/Séries Temporais")
library(zoo)

#1. A séries é estacionária?
#2. Existe alguma sazonalidade?
#3. Existe alguma tendência?
#4. O modelo fica melhor na escala logaritmica? (modelo multiplicativo)
#5. O teste de sequencias aponta tendência?
#6. Algum dos testes estudados em sala aponta sazonalidade?

covid <- readxl::read_xlsx('dados/covidrj.xlsx')
covid.df <- zoo(diff(covid$deaths, 1), order.by = covid$date[-1], frequency = 7)
plot(covid.df)