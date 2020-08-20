library(zoo)

covid <- readxl::read_xlsx('/home/lucasmoschen/Downloads/covidrj.xlsx')
covid.df <- zoo(diff(covid$deaths, 1), calendar = covid$date[-1], frequency = 7)
plot(covid.df)

Q <- rep(seq(1,7), length(covid.df)/7)
dec <- decompose(factor(covid.df))

kruskal.test()