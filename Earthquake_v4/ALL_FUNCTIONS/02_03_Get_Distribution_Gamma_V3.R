fit_distribution <- function(AMAX){
  setwd(workingdir2)
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

    # gofstat(fw)
    # gofstat(fgev)
    # gofstat(ffrech)
    # gofstat(flogn)
    # gofstat(fgamma)

    pdf('EQ_CullenandFrey.pdf')
    par(mfrow = c(1, 1))
    descdist(AMAX$AMAX, boot = 1000,method = 'unbiased')
    dev.off()
    
    pdf('EQ_AMAX_DIST.pdf')
    plotdist(AMAX$AMAX,histo = FALSE,demp = TRUE,breaks = 20)
    dev.off()
    
    #NOT CENSORED
    pdf('EQ_FandLogn_Plot.pdf')
    #fg <- fitdist(AMAX$AMAX, "gumbel", start=list(a=4, b=1))
    
    par(mfrow = c(2, 2))
    plot.legend <- c("Frechet","Logn")
    denscomp(list(ffrech,flogn), legendtext = plot.legend)
    qqcomp(list(ffrech,flogn), legendtext = plot.legend)
    cdfcomp(list(ffrech,flogn), legendtext = plot.legend)
    ppcomp(list(ffrech,flogn), legendtext = plot.legend)
    dev.off()
    print(summary(ffrech))
    
    
    pdf('EQ_Bootstrap_plot.pdf')
    par(mfrow = c(1, 1))
    bootstrap_flogn <- bootdist(flogn,niter = 1001)
    plot(bootstrap_flogn)
    dev.off()
    print(quantile(bootstrap_flogn,probs = 0.05))

    pdf('EQ_MLE_vs_MGOF_plot.pdf')
    par(mfrow = c(1, 1))
    flogn.ln.ADR <- fitdist(AMAX$AMAX, 'lnorm', method = "mge", gof = "ADR")
    flogn.ln.AD2R <- fitdist(AMAX$AMAX, 'lnorm', method = "mge", gof = "AD2R")
    cdfcomp(list(fgev, flogn.ln.ADR, flogn.ln.AD2R),xlogscale = TRUE, ylogscale = TRUE,
            main = "Fitting a Log-Normal distribution", xlegend = "bottomright",legendtext = c("MLE", "Right-tail AD", "Right-tail AD 2nd order"))
    dev.off()
    
    pdf('EQ_Log_Likelihood_plot.pdf')
    llplot(flogn, loglik = TRUE, expansion = 1, lseq = 50,
           back.col = TRUE, nlev = 10, pal.col = terrain.colors(100),
           fit.show = FALSE, fit.pch = 4)
    dev.off()
    
    pdf('EQ_MLE_Bootstrap_Confint_plot.pdf')
    CIcdfplot(bootstrap_flogn, CI.output = 'probability', CI.type = "two.sided", CI.level = 0.95, CI.col = "red",
              CI.lty = 2, CI.fill = NULL, CI.only = FALSE, xlogscale = FALSE,
              ylogscale = FALSE,horizontals = TRUE, verticals = FALSE, do.points = TRUE, use.ppoints = TRUE,
              a.ppoints = 0.5, lines01 = FALSE)
    dev.off()
    
    print(gofstat(flogn))
  
    
  }else{
    #THEN USE CENSORED
    AMAX2 <- AMAX
    AMAX$Year <- AMAX$AMAX
    AMAX$AMAX[AMAX$AMAX == 0] <- 4
    colnames(AMAX) <- c('left','right')
    fgamma <- fitdistcens(censdata = AMAX, distr = "gamma")
  }
  values <- data.frame(fgamma$estimate)
  colnames(values) <- c('Estimate')
  rownames(values) <- c('shape','rate')
  return(values)
}


plot_Gamma <- function(values,ret_p){
  values <- MLE_values
  Gamma_Mw = qgamma((1-1/ret_p),shape =values['shape','Estimate'],rate = values['rate','Estimate'])
  toplot <- data.frame(ret_p,Gamma_Mw)
  pdf('EQ_Gamma_Plot.pdf')
  graph <- ggplot(aes(x=toplot$ret_p,y=toplot$Gamma_Mw,color=toplot$Gamma_Mw),data=toplot,size=0.5 ) + geom_point()
  graph <- graph + scale_color_gradient(low='dark green',high='dark red') + guides(fill=FALSE)+ scale_x_continuous(name = 'Return Period (Years)')+
    scale_y_continuous(name = 'Magnitude (Mw)') + theme_bw()
  print(graph)
  dev.off()
}