setwd('C:/Users/Peter/Documents/Earthquake_v3/Earthquake_v3/ALL_FUNCTIONS')
require(fitdistrplus)
require (QRM)

#Get the Historical EQ Data
source('02_01_Get_Data_Ready.R')
EQ_Data <- get_EQ_data()
#GET THE AMAX SERIES FOR ALL DATA
source('02_04_GetAMAXSERIES.R')
AMAX <- get_AMAX(EQ_Data)


fit_distribution <- function(){
  
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
    
    pdf('EQ_Gumbel_Plot.pdf')
    fw <- fitdist(AMAX$AMAX, "weibull")
    fg <- fitdist(AMAX$AMAX, "gumbel", start=list(a=4, b=1))
    par(mfrow = c(2, 2))
    plot.legend <- c("Weibull", "Gumbel")
    denscomp(list(fw, fg), legendtext = plot.legend)
    qqcomp(list(fw, fg), legendtext = plot.legend)
    cdfcomp(list(fw, fg), legendtext = plot.legend)
    ppcomp(list(fw, fg), legendtext = plot.legend)
    dev.off()
    
    values <- data.frame(fg$estimate)
    colnames(values) <- c('Estimate')
    rownames(values) <- c('mu','alpha')
    
    return(values)
    
    
  }else{
    #THEN USE CENSORED
    AMAX$Year <- AMAX$AMAX
    AMAX$AMAX[AMAX$AMAX == 0] <- 4
    fw <- fitdist(censdata = AMAX, "weibull")
    values <- data.frame(fw$estimate)
  }
}


