

fit_distribution <- function(AMAX){
  setwd('C:/Users/Peter/Documents/Earthquake_v3/Earthquake_v3/ALL_FUNCTIONS')
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
    print(summary(fw))
    pdf('EQ_Bootstrap_plot.pdf')
    par(mfrow = c(1, 1))
    bootstrap_fw <- bootdist(fw,niter = 1001)
    dev.off()
    print(quantile(bootstrap_fw,probs = 0.05))

    pdf('EQ_MLE_vs_MGOF_plot.pdf')
    par(mfrow = c(1, 1))
    weibull.ln.ADR <- fitdist(AMAX$AMAX, "weibull", method = "mge", gof = "ADR")
    weibull.ln.AD2R <- fitdist(AMAX$AMAX, "weibull", method = "mge", gof = "AD2R")
    cdfcomp(list(fw, weibull.ln.ADR, weibull.ln.AD2R),xlogscale = TRUE, ylogscale = TRUE,
            main = "Fitting a Weibull distribution", xlegend = "bottomright",legendtext = c("MLE", "Right-tail AD", "Right-tail AD 2nd order"))
    dev.off()
    
    pdf('EQ_Log_Likelihood_plot.pdf')
    llplot(fw, loglik = TRUE, expansion = 1, lseq = 50,
           back.col = TRUE, nlev = 10, pal.col = terrain.colors(100),
           fit.show = FALSE, fit.pch = 4)
    dev.off()
    
    pdf('EQ_MLE_Bootstrap_Confint_plot.pdf')
    CIcdfplot(bootstrap_fw, CI.output = 'probability', CI.type = "two.sided", CI.level = 0.95, CI.col = "red",
              CI.lty = 2, CI.fill = NULL, CI.only = FALSE, xlogscale = FALSE,
              ylogscale = FALSE,horizontals = TRUE, verticals = FALSE, do.points = TRUE, use.ppoints = TRUE,
              a.ppoints = 0.5, lines01 = FALSE)
    dev.off()
    print(gofstat(fw))
    test <- (gofstat(fw))[1]
    x <- test['ks'][[1]]
    
    
    
    
    
    library(boot) #load the package
    # Now we need the function we would like to estimate
    # In our case the beta:
    betfun = function(data){
      # b is the random indexes for the bootstrap sample
      d = data
      fw.temp <- fitdist(AMAX$AMAX, "weibull")
      length(AMAX$AMAX)
      fw.temp['data'] <- d
      #print(names(fw.temp))
      
      return(gofstat(fw.temp)['ks'][1])  
      # thats for the beta coefficient
    }
    betfun(data=AMAX$AMAX)
    # now you can bootstrap:
    bootbet = boot(data=AMAX$AMAX, betfun, R=5000,stype = 'w')
    city <- bigcity
    ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
    boot(city, ratio, R = 999, stype = "w")
    # R is how many bootstrap samples
    names(bootbet)
    plot(bootbet)
    hist(bootbet$t, breaks = 100)
    
    
    
    
    
    
    x <- gofstat(fw.temp)['ks'][1]
    x <- gofstat(fw.temp)['ks'][[1]]
    
    boot.tee = function(data, i) {
          d = data[i]
          fw.temp <- fitdist(d, "weibull")
          #gofstat(fw.temp)['ks'][[1]]
          return(sum(d))
          #d = data[i,]
          #c(t.test(d$extra[1:10], d$extra[11:20], var.eq=T)$statistic, t.test(d$extra[1:10], d$extra[11:20], var.eq=T)$p.value)
      }
    boot.out = boot(data=AMAX$AMAX, statistic=boot.tee, R=1000)
    plot(boot.out)
    hist(boot.out$t)
    
    
    
    
    
    
    
    
    
    
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
