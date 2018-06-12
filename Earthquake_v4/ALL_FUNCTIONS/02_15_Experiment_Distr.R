#EXPERIMENT WITH THE DISTRIBUTIONS

plot_Experiment1 <- function(AMAX,ret_p){

  source('03_03_Distributions.R')

  fw <- fitdist(AMAX$AMAX, "weibull")
  fgev <- fitdist(AMAX$AMAX, 'gev',  start = list(loc = 6.158 , scale = 0.53,shape = 0))
  ffrech <- fitdist(AMAX$AMAX, dfrechet,  start = list(loc = 1 , scale = 1,shape = 1),optim.method =  "L-BFGS-B",lower = c(-Inf,-Inf,-Inf))
  flogn <- fitdist(AMAX$AMAX, 'lnorm')
  fgamma <- fitdist(AMAX$AMAX, 'gamma')
  
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
  graph <- graph + theme_bw() + coord_fixed(ratio = 0.25)# + guide_legend()
  pdf('Distribution Ret_p Comp.pdf')
  plot(graph)
  dev.off()
}

plot_ret_p <- function(values,ret_p){
  #check this!!!!! is it 1-?
  values <- MLE_values
  Log_Norm_Mw = qlnorm((1-1/ret_p),meanlog =values[1,1],sdlog = values['sdlog','Estimate'])
  print('Return Period Values')
  print(Log_Norm_Mw)
  toplot <- data.frame(ret_p,Log_Norm_Mw)
  pdf('EQ_Log_Normal_Plot.pdf')
  graph <- ggplot(aes(x=toplot$ret_p,y=toplot$Log_Norm_Mw,color=toplot$Log_Norm_Mw),data=toplot,size=0.5 ) + geom_point()
  graph <- graph + scale_color_gradient(low='dark green',high='dark red') + guides(fill=FALSE)+ scale_x_continuous(name = 'Return Period (Years)')+
    scale_y_continuous(name = 'Magnitude (Mw)') + theme_bw()
  print(graph)
  dev.off()
}