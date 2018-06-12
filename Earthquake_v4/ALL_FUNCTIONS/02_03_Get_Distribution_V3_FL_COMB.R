require(fitdistrplus)
require(evd)

fit_distribution <- function(AMAX){
  setwd(workingdir2)
  dgumbel <- function(x, a, b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
  pgumbel <- function(q, a, b) exp(-exp((a-q)/b))
  qgumbel <- function(p, a, b) a-b*log(-log(p))
    
  #AMAX <- AMAX.df
  fw <- fitdistcens(censdata = AMAX, distr = "weibull",start = list(shape = 6,scale = 0.5))
  
  fw <- fitdistcens(AMAX, "weibull")
  fgev <- fitdistcens(AMAX, 'gev',  start = list(loc = 6.158 , scale = 0.53,shape = 0))
  ffrech <- fitdistcens(AMAX, dfrechet,  start = list(loc = 1 , scale = 1,shape = 1),optim.method =  "L-BFGS-B",lower = c(-Inf,-Inf,-Inf))
  flogn <- fitdistcens(AMAX, 'lnorm')
  fgamma <- fitdistcens(AMAX, 'gamma')
  #fpois <- fitdistcens(round_any(AMAX*100,1), 'pois')
  
  summary(fw)
  summary(fgev)
  summary(ffrech)
  summary(flogn)
  summary(fgamma)
  
  pdf('EQ_ALLFL_AMAX_HIST_COMB.pdf')
  graph <- plotdist(AMAX.df$left[AMAX.df$left>4],histo = TRUE,demp = TRUE,breaks = 20)
  hist(AMAX.df$left[AMAX.df$left>4])
  dev.off()
  
  pdf('EQ_ALLFL_AMAX_HISTvsDist_COMB.pdf')
  #flogn$estimate
  AMAX[AMAX$left<4,] <- runif(dim(AMAX[AMAX$left<4,])[1],0,4)
  x <- data.frame(rlnorm(50000,flogn$estimate[[1]],flogn$estimate[[2]]))
  colnames(x) <- c('values')
  graph <- ggplot(data=AMAX, aes(AMAX$left)) + geom_histogram(aes(y =..density..),bins = 20,fill = NA,color = 'black')# + geom_density(fill = 'red',col=2,alpha = 0.4)
  graph <- graph +  geom_density(aes(values),alpha = 0.4,fill = 'blue',data = x) + theme_bw() + scale_x_continuous(name = 'AMAX (Mw)') + scale_y_continuous(name = 'Density | Probability')
  graph
  plot(graph)
  dev.off()
  
  
  pdf('EQ_ALLFL_AMAX_CDF_COMB.pdf')
  plotdistcens(AMAX.df,leftNA = 0)
  dev.off()
  
  values <- data.frame(flogn$estimate)
  colnames(values) <- c('Estimate')
  rownames(values) <- c('meanlog','sdlog')
  
  return(values)
}











# if (!is.element(0,AMAX$AMAX)){
#   
#   source('03_03_Distributions.R')
#   
#   fw <- fitdist(AMAX$AMAX, "weibull")
#   fgev <- fitdist(AMAX$AMAX, 'gev',  start = list(loc = 6.158 , scale = 0.53,shape = 0))
#   ffrech <- fitdist(AMAX$AMAX, dfrechet,  start = list(loc = 1 , scale = 1,shape = 1),optim.method =  "L-BFGS-B",lower = c(-Inf,-Inf,-Inf))
#   flogn <- fitdist(AMAX$AMAX, 'lnorm')
#   fgamma <- fitdist(AMAX$AMAX, 'gamma')
#   fpois <- fitdist(round_any(AMAX$AMAX*100,1), 'pois')
#   
#   gofstat(fw)
#   gofstat(fgev)
#   gofstat(ffrech)
#   gofstat(flogn)
#   gofstat(fgamma)
#   x <- gofstat(fpois)
#   
#   
#   
#   pdf('EQ_CullenandFrey.pdf')
#   par(mfrow = c(1, 1))
#   descdist(AMAX$AMAX, boot = 1000,method = 'unbiased')
#   dev.off()
#   
#   pdf('EQ_AMAX_DIST.pdf')
#   plotdist(AMAX$AMAX,histo = TRUE,demp = TRUE,breaks = 20)
#   dev.off()
#   
#   #NOT CENSORED
#   pdf('EQ_FandGEV_Plot.pdf')
#   fw <- fitdist(AMAX$AMAX, "weibull")
#   fg <- fitdist(AMAX$AMAX, "gumbel", start=list(a=4, b=1))
#   
#   
#   par(mfrow = c(2, 2))
#   plot.legend <- c("GEV", "Frechet")
#   denscomp(list(fgev, ffrech), legendtext = plot.legend)
#   qqcomp(list(fgev, ffrech), legendtext = plot.legend)
#   cdfcomp(list(fgev, ffrech), legendtext = plot.legend)
#   ppcomp(list(fgev, ffrech), legendtext = plot.legend)
#   dev.off()
#   print(summary(ffrech))
  
  # pdf('EQ_GEV,GandW_Plot.pdf')
  # par(mfrow = c(2, 2))
  # plot.legend <- c("GEV", "Gamma","Weibull")
  # denscomp(list(fgev,fgamma, fw), legendtext = plot.legend)
  # qqcomp(list(fgev,fgamma, fw), legendtext = plot.legend)
  # cdfcomp(list(fgev,fgamma, fw), legendtext = plot.legend)
  # ppcomp(list(fgev,fgamma, fw), legendtext = plot.legend)
  # dev.off()
  # print(summary(ffrech))
  
  
  # pdf('EQ_Bootstrap_plot.pdf')
  # par(mfrow = c(1, 1))
  # bootstrap_fgev <- bootdist(fgev,niter = 1001)
  # dev.off()
  # print(quantile(bootstrap_gev,probs = 0.05))
  # 
  # pdf('EQ_MLE_vs_MGOF_plot.pdf')
  # par(mfrow = c(1, 1))
  # fgev.ln.ADR <- fitdist(AMAX$AMAX, 'gev', method = "mge", gof = "ADR",start = list(loc = 6.158 , scale = 0.53,shape = 0))
  # fgev.ln.AD2R <- fitdist(AMAX$AMAX, 'gev', method = "mge", gof = "AD2R",start = list(loc = 6.158 , scale = 0.53,shape = 0))
  # cdfcomp(list(fgev, fgev.ln.ADR, fgev.ln.AD2R),xlogscale = TRUE, ylogscale = TRUE,
  #         main = "Fitting a Frechet distribution", xlegend = "bottomright",legendtext = c("MLE", "Right-tail AD", "Right-tail AD 2nd order"))
  # dev.off()
  # 
  # pdf('EQ_Log_Likelihood_plot.pdf')
  # llplot(fgev, loglik = TRUE, expansion = 1, lseq = 50,
  #        back.col = TRUE, nlev = 10, pal.col = terrain.colors(100),
  #        fit.show = FALSE, fit.pch = 4)
  # dev.off()
  # 
  # pdf('EQ_MLE_Bootstrap_Confint_plot.pdf')
  # CIcdfplot(bootstrap_fgev, CI.output = 'probability', CI.type = "two.sided", CI.level = 0.95, CI.col = "red",
  #           CI.lty = 2, CI.fill = NULL, CI.only = FALSE, xlogscale = FALSE,
  #           ylogscale = FALSE,horizontals = TRUE, verticals = FALSE, do.points = TRUE, use.ppoints = TRUE,
  #           a.ppoints = 0.5, lines01 = FALSE)
  # dev.off()
  # 
  # 
  # print(gofstat(ffrech))
  # #test <- (gofstat(fw))[1]
  # #x <- test['ks'][[1]]
  # #require(evd)
  # poissonpar(mfrow = c(2, 2))
  # plot.legend <- c("Weibull","GEV","Frechet")
  # denscomp(list(fw, fgev,ffrech),breaks = 20, legendtext = plot.legend)
  # qqcomp(list(fw, fgev,ffrech), legendtext = plot.legend)
  # cdfcomp(list(fw, fgev,ffrech), legendtext = plot.legend)
  # ppcomp(list(fw, fgev,ffrech), legendtext = plot.legend)
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