# Gerar séries aleatórias para trabalho.
library(polynom)

gen.coef <- function(n, im = 0) {
  if (n == 0) return(list(c(),c()))
  
  if (im < 0) im <- floor(runif(1)*n)
  
  im <- im - (im %% 2)
  if (im == 0) {
    raizes <- 1 / runif(n, -1, 1)
  } else {
    x <- exp(complex(imaginary = pi*runif(im %/% 2, -1, 1))) / runif(im %/% 2)
    raizes <- c(x, Conj(x))
    if (n - im > 0) {
      raizes <- c(1 / runif(n - im,-1, 1), raizes)
    }
  }
  p <- polynom::poly.from.zeros(raizes)
  v <- c()
  for (i in 2:length(p)) {
    v <- c(v, p[i] / p[1])
  }
  
  return(list(v,raizes))
}


set.seed(14092020)
X<-list()
for(i in 1:9){
  p=q=0;
  while( p+q > 5 || p+q ==0 ){
    p <- rgeom(1,.4) 
    q <- rgeom(1,.4)
  } 
  coef.ma <- gen.coef(q,-1)[[1]]
  coef.ar <- gen.coef(p,-1)[[1]]
  n = 50+rgeom(1,.008)
  X[[i]] <- arima.sim(model = list(ma=coef.ma), n = n)
  #cat(n,'p =',p,coef.ar,'q =',q,coef.ma,'\n')
}

