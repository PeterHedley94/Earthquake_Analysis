#EXPERIMENT WITH THE DISTRIBUTIONS

plot_Experiment1 <- function(AMAX,ret_p){
  
  source('03_03_Distributions.R')
  
  AMAX <- data.frame(c(7.1,rep(0,106)),c(7.1,rep(0,106)))
  colnames(AMAX) <- c('Year','AMAX')
  
  #THEN USE CENSORED
  AMAX2 <- AMAX
  AMAX$Year <- AMAX$AMAX
  AMAX$AMAX[AMAX$AMAX == 0] <- 4
  colnames(AMAX) <- c('left','right')
  
  
  fw <- fitdistcens(AMAX, "weibull")
  fgev <- fitdistcens(AMAX, 'gev',  start = list(loc = 6.158 , scale = 0.53,shape = 0))
  ffrech <- fitdistcens(AMAX, dfrechet,  start = list(loc = 1 , scale = 1,shape = 1),optim.method =  "L-BFGS-B",lower = c(-Inf,-Inf,-Inf))
  flogn <- fitdistcens(AMAX, 'lnorm')
  fgamma <- fitdistcens(AMAX, 'gamma')
  
  
  summary(fw)
  summary(fgev)
  summary(ffrech)
  summary(flogn)
  summary(fgamma)
  
  
  
  
  rfw <- qweibull((1-1/ret_p),fw$estimate[[1]],fw$estimate[[2]])
  rfgev <- qgev((1-1/ret_p),fgev$estimate[[1]],fgev$estimate[[2]],fgev$estimate[[3]])
  rffrech <- qfrechet((1-1/ret_p),ffrech$estimate[[1]],ffrech$estimate[[2]],ffrech$estimate[[3]])
  rflogn <- qlnorm((1-1/ret_p),flogn$estimate[[1]],flogn$estimate[[2]])
  rfgamma <- qgamma((1-1/ret_p),fgamma$estimate[[1]],fgamma$estimate[[2]])
  
  data <- c(rfw,rfgev,rffrech,rflogn,rfgamma)
  nameddata <- c(rep('Weibull', length(rfw)),rep('GEV', length(rfgev)),rep('Frechet', length(rffrech)),rep('Log-Normal', length(rflogn)),rep('Gamma', length(rfgamma)))
  ret_p <- c(rep(ret_p,5))
  combined <- data.frame(ret_p,data,nameddata)
  colnames(combined) <- c('RP','Mw', 'Distribution')
  
  graph <- ggplot(aes(group = Distribution,x = RP),data = combined) + geom_line(aes(y = Mw,color = Distribution),size = 1,data = combined)
  graph <- graph + scale_x_log10(name = 'Return Period (Years)') + scale_y_continuous(name = 'Magnitude (Mw)')
  graph <- graph + theme_bw() + coord_fixed(ratio = 0.02)# + guide_legend()
  graph
  pdf('Distribution Ret_p Comp 1FL.pdf')
  plot(graph)
  dev.off()
}

