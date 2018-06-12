require(ggplot2)
library(stats4)
require(QRM)


#Function to generate Cumulative Distribution
Gumbel1cdf <- function(x,mu,alpha){
  fx = exp(-exp(-(x-mu)/alpha))
  return(fx)
}

#Get a value (f(x)) for a set x
Gumbel1pdf <- function(x,mu,alpha){
  fx = (1/alpha) * exp( -(x-mu)/alpha -exp(-(x-mu)/alpha) )
  return(fx)
}
x = AMAX$AMAX
#Censored Max_Likelihood function
LL <- function(x,mu,alpha){
  #print(k2)
  if(length(x[x<=4])>0){
    R = Gumbel1pdf(x[x>4],mu,alpha)
    k2 = Gumbel1cdf(4,mu,alpha)
    -sum(log(R))-sum( log( length(x[x<=4])*k2 ) )
  }else if (length(x[x<=4]) == length(x)){
    k2 = Gumbel1cdf(4,mu,alpha)
    -sum( log( length(x[x<=4])*k2 ) )
  }else{
    R = Gumbel1pdf(x[x>4],mu,alpha)
    -sum(log(R))
  }
}


LL2 <- function(x, loc, scale, shape, log = FALSE){
  #print(k2)

    R = dgev(x, loc, scale, shape)
    -sum(log(R))
}





library("lmomco")
library("fitdistrplus")
## reproducible:
month <- c(27.6, 97.9, 100.6, 107.3, 108.5,
           109, 112.4, 120.9, 137.8)

  
lmom <- lmoms(month,nmom=5)     #from lmomco package
para <- pargev(lmom, checklmom=TRUE)
dgev <- pdfgev   #functions which are included in lmomco
pgev <- cdfgev
fitgev <- fitdist(month, "gev", start=para[[2]])
## Error in mledist(data, distname, start, fix.arg, ...) : 
##   'start' must specify names which are arguments to 'distr'

dgev <- function(x,xi,alpha,kappa) {
  pdfgev(x,list(type="gev",para=c(xi,alpha,kappa),source="pargev"))
}
fitgev <- fitdist(month, "gev", start=para[[2]])   












#*********************************************************************#

get_Gumbel <- function(AMAX){
  mlevl <- mle(LL, start = list(mu = 4, alpha=3),fixed = list(x=AMAX$AMAX))
  mlevl2 <- mle(LL2, start = list(loc=0, scale=4, shape=0),fixed = list(x=AMAX$AMAX))
  values <- data.frame(summary(mlevl)@coef)
  return(values)
}

#***********************************************************************#

plot_Gumbel <- function(values,ret_p){
  #check this!!!!! is it 1-?
  Gumbel_Mw = qGumbel((1-1/ret_p),mu = MLE_values['mu','Estimate'],sigma = MLE_values['alpha','Estimate'])
  toplot <- data.frame(ret_p,Gumbel_Mw)
  pdf('EQ_Gumbel_Plot.pdf')
  graph <- ggplot(aes(x=toplot$ret_p,y=toplot$Gumbel_Mw,color=toplot$Gumbel_Mw),data=toplot,size=0.5 ) + geom_point()
  graph <- graph + scale_color_gradient(low='dark green',high='dark red') + guides(fill=FALSE)
  print(graph)
  dev.off()
}



x = seq()
x = rGumbel(1000,mu = 4,sigma = 0.6)
hist(x)
x[x<4] <- 0
length(x[x<4])
y <- rGumbel(100, mu=4,sigma = 0.6)
y[y<4] <- 0
x <- c(x,y)
summary(mle(LL, start = list(mu = 4, alpha=3),fixed = list(x=x)))
