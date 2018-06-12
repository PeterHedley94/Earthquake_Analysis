plot_FL_Mw <- function(Map,FL_Data,Return_Period_Mw,Return_Period_Mw2,Distribution){
  Newdir <- paste(getwd(),'/',Distribution,'/FL_DISS_RASTER',sep = '')
  setwd(Newdir)
  FL_Data2 <- FL_Data[,1]
  FL_Data2$IDBG <- Return_Period_Mw$X1000
  FL_Data2$id <- seq(from = 0,to = (length(FL_Data2$IDBG)-1),by=1)
  names(FL_Data2) <- c('Mw','id')
  #Convert spatial dataframe to dataframe for ggplot2
  FL_Data3 <- fortify(FL_Data2)
  colnames(FL_Data3) <- c("lon","lat","order","piece","id","group")
  #Add Mw data to the dataframe
  FL_Data3 <- join(FL_Data3,data.frame(FL_Data2@data),by='id')
  #FL_Data3$Mw <- as.numeric(FL_Data3$Mw)
  #CHANGE THIS WHEN FIX MW PROBLEM!!!!!
  FL_Data3 <- FL_Data3[FL_Data3$Mw>1,]
  #plot(FL_Data3)
  #Plots
  pdf('Fault_Line_Mw_general.pdf')
  graph <- Map + geom_path(aes(group=group,x = lon, y = lat,color = Mw), size = 0.75,data=FL_Data3) + scale_color_gradient(low='dark green',high = 'red')
  print(graph)
  dev.off()
  
  
  FL_Data2 <- FL_Data[,1]
  FL_Data2$IDBG <- Return_Period_Mw2$X1000
  FL_Data2$id <- seq(from = 0,to = (length(FL_Data2$IDBG)-1),by=1)
  names(FL_Data2) <- c('Mw','id')
  #Convert spatial dataframe to dataframe for ggplot2
  FL_Data3 <- fortify(FL_Data2)
  colnames(FL_Data3) <- c("lon","lat","order","piece","id","group")
  #Add Mw data to the dataframe
  FL_Data3 <- join(FL_Data3,data.frame(FL_Data2@data),by='id')
  #FL_Data3$Mw <- as.numeric(FL_Data3$Mw)
  #CHANGE THIS WHEN FIX MW PROBLEM!!!!!
  FL_Data3 <- FL_Data3[FL_Data3$Mw>1,]
  
  pdf('Fault_Line_Mw_Hist.pdf')
  plot1 <- ggplot(data = FL_Data3) + geom_histogram(aes(FL_Data3$Mw),fill = NA,color = 'black') + scale_x_continuous(name = 'Mw') + theme_bw() + scale_y_continuous(name = 'Frequency')
  print(plot1)
  dev.off()
}

