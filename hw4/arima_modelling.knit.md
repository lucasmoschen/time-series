---
title: "Modelagem ARIMA e Análise de resíduos"
author: "Lucas Resck e Lucas Moschen"
date: \today
output:
  pdf_document:
    template: null
bibliography: references.bib
link-citations: yes
---



# Método Box-Jenkins 

0. Tranformação dos dados para estabilizar a variância. 

1. Identificação

1.1 Checar a estacionaridade e diferencial $d$ vezes;

1.2 Visualizar autocorrelação e autocorrelação parcial dos dados;

1.3 Comparar informações AIC, BIC e AICc e selectionar $p$ e $q$.

2. Estimação 

2.1 Estimar os valores de $\phi$ e $\theta$ do modelo através de máxima verossimilhança.

3. Diagnóstico 

3.1 Visualizar os resíduos do fitting; 

3.2 Plotar histograma, autocorrelação e autocorrelação parcial dos resíduos; 

3.3 Testes de estacionaridade e de normalidade. 

# Assassinato de mulheres

Primeiro, vamos visualizar a série anual. Peecebemos uma clara tendência, com crescimento acentuado ao longo das décadas de 1960 e 1970, bem como um decréscimo após os anos de 1990. 

![](arima_modelling_files/figure-latex/unnamed-chunk-1-1.pdf)<!-- --> 

## Transformação Box-Cox

Dada a nossa série, é importante que visualizemos a variância ao longo dela. Visualmente ela aparenta não ter variância constante, dado que no início da série, ela aparenta ser menor. Para isso vamos calcular a transformação ótima de Box-Cox. 

A transformação de Box-Cox é a seguinte, se $\{y_t\}$ for uma série temporal, 

$$
y_t^{(\lambda)} = \begin{cases} \frac{y_t^{\lambda} - 1}{\lambda}, \text{ se } \lambda \neq 0\\ \log(y_t),  \text{ se } \lambda = 0\end{cases}
$$

A escolha de $\lambda$ ótimo utiliza o método de Guerrero, que escolhe $\lambda$ que minimize o coeficiente de variação ($c_v = \frac{\sigma}{\mu}$) para subséries de $y_t$. A seguir podemos conferir o resultado da transformação. 

![](arima_modelling_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 

## Identificação 

### Teste de Estacionaridade ADF


```r
adf.test(bcwmurders)
```

```
## Warning in adf.test(bcwmurders): p-value greater than printed p-value
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  bcwmurders
## Dickey-Fuller = -0.027131, Lag order = 3, p-value = 0.99
## alternative hypothesis: stationary
```

Como $\text{p-valor} \ge 0.05$, inferimos a não estacioanridade.  

### Diferenciando 

![](arima_modelling_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 


```r
adf.test(dwmurders)
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  dwmurders
## Dickey-Fuller = -3.6064, Lag order = 3, p-value = 0.04084
## alternative hypothesis: stationary
```

![](arima_modelling_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

É inconclusivo, piriri, pororo. 




|  p|  q|       AIC|       BIC|      AICC|
|--:|--:|---------:|---------:|---------:|
|  0|  0| -157.8102| -153.8322| -157.5749|
|  0|  1| -156.0378| -150.0709| -155.5578|
|  0|  2| -159.8467| -151.8907| -159.0303|
|  0|  3| -158.0477| -148.1028| -156.7977|
|  1|  0| -156.1778| -150.2109| -155.6978|
|  1|  1| -156.6976| -148.7416| -155.8813|
|  1|  2| -157.9735| -148.0285| -156.7235|
|  1|  3| -156.1456| -144.2117| -154.3583|
|  2|  0| -159.4687| -151.5127| -158.6523|
|  2|  1| -157.6280| -147.6831| -156.3780|
|  2|  2| -158.9166| -146.9827| -157.1294|
|  2|  3| -157.2561| -143.3332| -154.8213|
|  3|  0| -157.5952| -147.6503| -156.3452|
|  3|  1| -155.4728| -143.5389| -153.6856|
|  3|  2| -157.4078| -143.4849| -154.9730|
|  3|  3| -155.9103| -139.9984| -152.7103|

Baseado no AIC e AICc, o modelo é (0,2). O BIC também caracteriza (0,2) como um bom modelo. 

## Estimação 


```r
model <- Arima(wmurders, order = c(0,1,2), lambda = "auto")
summary(model)
```

```
## Series: wmurders 
## ARIMA(0,1,2) 
## Box Cox transformation: lambda= -0.09529835 
## 
## Coefficients:
##           ma1     ma2
##       -0.0907  0.3770
## s.e.   0.1283  0.1723
## 
## sigma^2 estimated as 0.002701:  log likelihood=83.92
## AIC=-161.85   AICc=-161.37   BIC=-155.88
## 
## Training set error measures:
##                        ME      RMSE      MAE        MPE     MAPE      MASE
## Training set -0.001070576 0.1996771 0.154647 -0.1341953 4.430353 0.9510067
##                    ACF1
## Training set 0.03120906
```

## Diagnóstico 


```r
checkresiduals(model)
```

![](arima_modelling_files/figure-latex/unnamed-chunk-10-1.pdf)<!-- --> 

```
## 
## 	Ljung-Box test
## 
## data:  Residuals from ARIMA(0,1,2)
## Q* = 8.6023, df = 8, p-value = 0.3769
## 
## Model df: 2.   Total lags used: 10
```

```r
jarque.bera.test(model$residuals)
```

```
## 
## 	Jarque Bera Test
## 
## data:  model$residuals
## X-squared = 0.13536, df = 2, p-value = 0.9346
```

Não rejeitamos nenhuma das hipóteses. Estamos satisfeitos. 

## Projeção 


```r
forecast(model, h = 3) %>% autoplot()
```

![](arima_modelling_files/figure-latex/unnamed-chunk-11-1.pdf)<!-- --> 


# Uso de cartão de débito

Primeiro, vamos visualizar a série. Percebemos uma sutil sazonalidade, aparentemente anual, além de uma clara tendência. 

![](arima_modelling_files/figure-latex/unnamed-chunk-12-1.pdf)<!-- --> 

## Transformação Box-Cox

Pelo gráfico, parece que teremos que fazer alguma transformação de estabilidade da variância. Para isso, vamos utilizar a transformação Box-Cox.

![](arima_modelling_files/figure-latex/unnamed-chunk-13-1.pdf)<!-- --> 

## Identificação 

Agora, com a variância da série estabilizada, podemos fazer o teste de estacionaridade. Observe que o teste ADF possui possibilidade de tendência. Assim

### Teste de Estacionaridade ADF


```r
adf.test(bcdebitcards)
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  bcdebitcards
## Dickey-Fuller = -3.0873, Lag order = 5, p-value = 0.1227
## alternative hypothesis: stationary
```

Como $\text{p-valor} \ge 0.05$, não podemos rejeitar a hipótese nula de que a série é não estacionária. Portanto, vamos usar a primeira diferenciação.  

### Diferenciando 

![](arima_modelling_files/figure-latex/unnamed-chunk-15-1.pdf)<!-- --> 


```r
adf.test(ddebitcards)
```

```
## Warning in adf.test(ddebitcards): p-value smaller than printed p-value
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  ddebitcards
## Dickey-Fuller = -8.3838, Lag order = 5, p-value = 0.01
## alternative hypothesis: stationary
```

Assim, podemos rejeitar a hipótese nula, o que suporta a ideia de que a série é estacionária. Vamos considerar

### ACF e PACF

![](arima_modelling_files/figure-latex/unnamed-chunk-17-1.pdf)<!-- --> 

Percebemos dois fatores bem destacados: uma grande correlação quando o $\text{Lag} = 12$, o que indica que existe uma sazonalidade anual; e que a PACF decresce exponencialmente, enquanto a ACF morre após $\text{Lag} = 1$, o que nos levaria a um modelo MA(1). Antes disso, vamos fazer uma diferenciação a cada 12 meses. 

![](arima_modelling_files/figure-latex/unnamed-chunk-18-1.pdf)<!-- --> 


```
## Warning in adf.test(ddebitcards): p-value smaller than printed p-value
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  ddebitcards
## Dickey-Fuller = -5.4939, Lag order = 5, p-value = 0.01
## alternative hypothesis: stationary
```

Observe que ainda temos uma série estacionária. Vejamos a ACF e PACF novamente: 

![](arima_modelling_files/figure-latex/unnamed-chunk-20-1.pdf)<!-- --> 

O pico diminuiu consideravelmente. Agora, sim, vamos considerar os critérios de informação para identificar o modelo.

### Critérios de Informação




|  p|  q|       AIC|       BIC|      AICC|
|--:|--:|---------:|---------:|---------:|
|  0|  0| -277.1636| -271.1291| -277.0825|
|  0|  1| -339.0449| -329.9930| -338.8816|
|  0|  2| -339.0065| -326.9374| -338.7326|
|  0|  3| -342.2090| -327.1226| -341.7952|
|  0|  4| -342.0094| -323.9057| -341.4261|
|  0|  5| -345.7949| -324.6739| -345.0116|
|  1|  0| -311.2501| -302.1983| -311.0869|
|  1|  1| -337.9786| -325.9095| -337.7046|
|  1|  2| -338.9584| -323.8720| -338.5446|
|  1|  3| -341.1438| -323.0401| -340.5604|
|  1|  4| -342.3731| -321.2522| -341.5899|
|  1|  5| -343.8243| -319.6860| -342.8102|
|  2|  0| -343.2455| -331.1764| -342.9716|
|  2|  1| -343.9712| -328.8848| -343.5574|
|  2|  2| -342.0010| -323.8973| -341.4177|
|  2|  3| -340.0692| -318.9482| -339.2859|
|  2|  4| -345.1572| -321.0189| -344.1431|
|  2|  5| -354.2751| -327.1196| -352.9985|
|  3|  0| -343.8279| -328.7415| -343.4141|
|  3|  1| -341.9907| -323.8870| -341.4073|
|  3|  2| -343.4891| -322.3682| -342.7059|
|  3|  3| -341.8366| -317.6984| -340.8226|
|  3|  4| -339.8344| -312.6788| -338.5578|
|  3|  5| -350.4613| -320.2885| -348.8899|
|  4|  0| -341.9974| -323.8937| -341.4140|
|  4|  1| -340.0049| -318.8839| -339.2217|
|  4|  2| -341.7904| -317.6522| -340.7763|
|  4|  3| -339.8401| -312.6846| -338.5635|
|  4|  4| -350.3646| -320.1918| -348.7932|
|  4|  5| -353.3164| -320.1263| -351.4171|
|  5|  0| -340.0364| -318.9154| -339.2531|
|  5|  1| -338.0020| -313.8638| -336.9879|
|  5|  2| -341.2724| -314.1169| -339.9958|
|  5|  3| -344.0011| -313.8283| -342.4297|
|  5|  4| -345.4699| -312.2798| -343.5706|
|  5|  5| -349.3232| -313.1158| -347.0623|

Baseado no AIC e AICc, o modelo é ARMA(2,5). Baseado no BIC escolhemos um modelo bem mis simples, ARMA(2,0). Vou seguir com o modelo com mais parâmetros. 

## Estimação 

Estimamos os parâmetros do modelo, considerando uma diferenciação sazonal. 


```r
model <- Arima(debitcards, order = c(2,1,5), seasonal = c(0,1,0), lambda = "auto")
summary(model)
```

```
## Series: debitcards 
## ARIMA(2,1,5)(0,1,0)[12] 
## Box Cox transformation: lambda= 0.09078485 
## 
## Coefficients:
##          ar1      ar2      ma1     ma2      ma3      ma4     ma5
##       1.1563  -0.7891  -2.0007  1.9044  -0.3988  -0.5641  0.4453
## s.e.  0.0746   0.0632   0.1042  0.2100   0.2417   0.1881  0.0951
## 
## sigma^2 estimated as 0.004985:  log likelihood=186.12
## AIC=-356.23   AICc=-355.22   BIC=-332.09
## 
## Training set error measures:
##                       ME      RMSE       MAE        MPE     MAPE      MASE
## Training set -0.03028165 0.8570868 0.6404297 -0.2238406 3.952716 0.4998296
##                   ACF1
## Training set 0.0607404
```

## Diagnóstico 

Aqui podemos conferir os resíduos. É importante detacar a aparência normal dos resíduos, o que é interessante. Porém a ACF ainda apresenta picos, o mais incomodativo é do período 12. 

O test Ljung-Box coorrobora esse fato, rejeitando a hipótese nula de que os coeficientes de correlação são iguais. Já Jarque Bera não rejeita a normalidade, como esperávamos. 


```r
checkresiduals(model)
```

![](arima_modelling_files/figure-latex/unnamed-chunk-24-1.pdf)<!-- --> 

```
## 
## 	Ljung-Box test
## 
## data:  Residuals from ARIMA(2,1,5)(0,1,0)[12]
## Q* = 42.875, df = 17, p-value = 0.0005007
## 
## Model df: 7.   Total lags used: 24
```

```r
jarque.bera.test(model$residuals)
```

```
## 
## 	Jarque Bera Test
## 
## data:  model$residuals
## X-squared = 2.6451, df = 2, p-value = 0.2664
```

O persistente pico pode ser um indicativo de que a série sazonal precise de coeficientes ARMA. Em particular, poderíamos colocar ARMA sazonal. Isso fica para as próximas análises. 

## Projeção 

Agora vamos conferir as projeções três passos a frente.


```r
forecast(model, h = 3) %>% autoplot()
```

![](arima_modelling_files/figure-latex/unnamed-chunk-25-1.pdf)<!-- --> 

# Comer fora na Austrália

Primeiro, vamos visualizar a série. 

![](arima_modelling_files/figure-latex/unnamed-chunk-26-1.pdf)<!-- --> 
