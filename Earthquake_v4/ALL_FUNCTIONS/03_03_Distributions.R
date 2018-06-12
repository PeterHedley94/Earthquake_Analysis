#GEV
qgev<-function(p, loc = 0, scale = 1, shape = 0, lower.tail = TRUE){
    if(!lower.tail) p <- 1 - p
    if(shape == 0) return(loc - scale * log(-log(p)))
    else return(loc + scale * ((-log(p))^(-shape) - 1)/shape)
}
#GEV
pgev<-function(q, loc = 0, scale = 1, shape = 0, lower.tail = TRUE){
    q <- (q - loc)/scale
    if(shape == 0) p <- exp(-exp(-q))
    else p <- exp( - pmax(1 + shape * q, 0)^(-1/shape))
    if(!lower.tail) p <- 1 - p
    return(p)
}
#GEV
dgev <- function(x, loc = 0, scale = 1, shape = 0, log = FALSE){
    x <- (x - loc)/scale
    if(shape == 0)
      d <- log(1/scale) - x - exp(-x) 
    else {
      nn <- length(x)
      xx <- 1 + shape*x
      xxpos <- xx[xx>0 | is.na(xx)]
      scale <- rep(scale, length.out = nn)[xx>0 | is.na(xx)]
      d <- numeric(nn)
      d <- log(1/scale) - xxpos^(-1/shape) -(1/shape + 1)*log(xxpos)
      #d[xx<=0 & !is.na(xx)] <- -Inf
    }  
    if(!log) d <- exp(d)
    d
}
rgev<-function(n, loc = 0, scale = 1, shape = 0){
  return(loc + scale * (rexp(n)^(-shape) - 1)/shape)
}




