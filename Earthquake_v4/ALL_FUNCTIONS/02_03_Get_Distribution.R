

fit_distribution <- function(AMAX){
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  dgumbel <- function(x, a, b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
  pgumbel <- function(q, a, b) exp(-exp((a-q)/b))
  qgumbel <- function(p, a, b) a-b*log(-log(p))
  
  if (!is.element(0,AMAX$AMAX)){

    pdf('EQ_CullenandFrey.pdf')
    par(mfrow = c(1, 1))
    descdist(AMAX$AMAX, boot = 1000,method = 'unbiased')
    dev.off()
    
    pdf('EQ_AMAX_DIST.pdf')
    plotdist(AMAX$AMAX)
    dev.off()
    
    #NOT CENSORED
    
    pdf('EQ_GandW_Plot.pdf')
    fw <- fitdist(AMAX$AMAX, "weibull")
    fg <- fitdist(AMAX$AMAX, "gumbel", start=list(a=4, b=1))
    par(mfrow = c(2, 2))
    plot.legend <- c("Weibull", "Gumbel")
    denscomp(list(fw, fg), legendtext = plot.legend)
    qqcomp(list(fw, fg), legendtext = plot.legend)
    cdfcomp(list(fw, fg), legendtext = plot.legend)
    ppcomp(list(fw, fg), legendtext = plot.legend)
    dev.off()
    

    
    
  }else{
    #THEN USE CENSORED
    AMAX2 <- AMAX
    AMAX$Year <- AMAX$AMAX
    AMAX$AMAX[AMAX$AMAX == 0] <- 4
    colnames(AMAX) <- c('left','right')
    fw <- fitdistcens(censdata = AMAX, distr = "weibull",start = list(shape = 6,scale = 0.5))
  }
  
  values <- data.frame(fw$estimate)
  colnames(values) <- c('Estimate')
  rownames(values) <- c('shape','scale')
  
  return(values)
}
