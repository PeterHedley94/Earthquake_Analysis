require(fitdistrplus)
require(evd)

fit_distribution <- function(AMAX){
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  dgumbel <- function(x, a, b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
  pgumbel <- function(q, a, b) exp(-exp((a-q)/b))
  qgumbel <- function(p, a, b) a-b*log(-log(p))
  
  if (!is.element(0,AMAX$AMAX)){
    
    source('03_03_Distributions.R')
    
    fw <- fitdist(AMAX$AMAX, "weibull")
    fgev <- fitdist(AMAX$AMAX, 'gev',  start = list(loc = 6.158 , scale = 0.53,shape = 0))
    ffrech <- fitdist(AMAX$AMAX, dfrechet,  start = list(loc = 1 , scale = 1,shape = 1),optim.method =  "L-BFGS-B",lower = c(-Inf,-Inf,-Inf))
    flogn <- fitdist(AMAX$AMAX, 'lnorm')
    fgamma <- fitdist(AMAX$AMAX, 'gamma')
    fpois <- fitdist(round_any(AMAX$AMAX*100,1), 'pois')
  }else{
    #THEN USE CENSORED
    AMAX2 <- AMAX
    AMAX$Year <- AMAX$AMAX
    AMAX$AMAX[AMAX$AMAX == 0] <- 4
    colnames(AMAX) <- c('left','right')
    fgev <- tryCatch({
      fgev <- fitdistcens(censdata = AMAX, distr = "gev",start = list(loc = 6.158 , scale = 0.53,shape = 0))
    }, error = function(err){
      print('failed')
    })
    
  }
  values <- tryCatch({
    values <- data.frame(fgev$estimate)
  }, error = function(err){
    values <- data.frame(c(0,0,0))
  })
  values
  colnames(values) <- c('Estimate')
  rownames(values) <- c('loc','scale','shape')
  #rownames(values) <- c('meanlog','sdlog')
  
  return(values)
}




plot_Gamma <- function(values,ret_p){
  #check this!!!!! is it 1-?
  values <- MLE_values
  Gamma_Mw = qgamma((1-1/ret_p),meanlog =values['shape','Estimate'],sdlog = values['rate','Estimate'])
  print('Return Period Values')
  print(Gamma_Mw)
  toplot <- data.frame(ret_p,Gamma_Mw)
  pdf('EQ_Gamma_Plot.pdf')
  graph <- ggplot(aes(x=toplot$ret_p,y=toplot$Gamma_Mw,color=toplot$Gamma_Mw),data=toplot,size=0.5 ) + geom_point()
  graph <- graph + scale_color_gradient(low='dark green',high='dark red') + guides(fill=FALSE)+ scale_x_continuous(name = 'Return Period (Years)')+
    scale_y_continuous(name = 'Magnitude (Mw)') + theme_bw()
  print(graph)
  dev.off()
}


# 
# boot.tee = function(data, i) {
#   d = data[i]
#   fw.temp <- fitdist(d, "weibull")
#   #gofstat(fw.temp)["ks"][[1]]
#   return(sum(d))
#   #d = data[i,]
#   #c(t.test(d$extra[1:10], d$extra[11:20], var.eq=T)$statistic, t.test(d$extra[1:10], d$extra[11:20], var.eq=T)$p.value)
# }
# boot.out = boot(data=AMAX$AMAX, statistic=boot.tee, R=1000)
# plot(boot.out)
# hist(boot.out$t)



# require(propagate)
# fitDistr(AMAX$AMAX)