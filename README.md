# Time Series 

Time series is a series of data points indexed in time order. Mathematically,
a time series is a stochastic process with a discrete-time observation
support. In this repository, we study several topics included in this topic,
as ARIMA models, trend and seasonality. Each homework has a pratical study and
some theorical questions. 

The homeworks and assigments are coded in R language and the presentations and
texts are in portuguese. If requested, the author can translate it. 

## Homework 1 

A pratical study of Car's Sales in Norway. In this study we practice the
following modelling: 

- Decomposition in [Seasonal, Trend and Remainder](https://otexts.com/fpp2/stl.html) of a Time Series. 
- Polynomial regression for trend and seasonality. 
- Holt Winters and Exponential Smoothing 

After each modelling, we compared the models using the [Mean Absolute
Percentage
Error](https://en.wikipedia.org/wiki/Mean_absolute_percentage_error). 

Above you can see the moving average decomposition and the final comparison
among the models. 

![decomposition moving average](images/moving_average_decomposition_hw1.png)

![comparison among models](images/comparison_model_hw1.png)

## Homework 2

Identification of ARMA models, considering the Autocorrelation and Partial
Autocorrelation of the series. 

By looking the image, the behaviour of the graphic can identify, initially, a
model. 

![acf and pacf](images/acf_pacf_hw2.png) 

## Homework 3

We have some theorical exercises about differenciation and stationarity of
ARIMA process. Also we construct by simulation the distribution of the
statistic of [Adjusted Dickey-Fuller (ADF)
Test](https://en.wikipedia.org/wiki/Augmented_Dickey%E2%80%93Fuller_test). In
the end we identify an ARIMA model for the ipca (inflation) series. We
considered a variance stabilizer. So as to identify the model, we use the ACF
and PACF graphics combined by the [Information
Criterion](https://en.wikipedia.org/wiki/Model_selection#Criteria), as AIC,
BIC and AICc.

![ipca monthly](images/ipca_monthly_hw3.png) 

For each considered model, we studied the residuals of the estimated model. 

![residual analysis](images/residual_analysis_hw3.png)

## Homework 4 



## Homework 5 






