
Plot_EQ_Events <- function(EQ_Data,FL_Data,Map){
  #Convert FL_Data to a db for ggplot2
  FL_Data2 <- fortify(FL_Data)
  colnames(FL_Data2) <- c("lon","lat","order","piece","id","group")
  graph <- Map + geom_point(aes(x=lon,y=lat),colour = 'dark orange',data=EQ_Data,size=0.1)
  graph <- graph + geom_path(aes(group=group),color = 'blue', size = 0.75,data=FL_Data2)
  pdf('EQ_Event_Plot.pdf')
  print(graph)
  dev.off()
}

Plot_EQ_Events_Mw <- function(EQ_Data,Map){
  pdf('EQ_Event_Mw_Plot.pdf')
  graph <- Map + geom_point(aes(x=lon,y=lat,colour = Mw),size=0.1,alpha = 0.5,data=subset(EQ_Data,EQ_Data$Mw <6))
  print(graph + geom_point(aes(x=lon,y=lat,colour = Mw,size=Mw),data=subset(EQ_Data,EQ_Data$Mw>6)) + scale_color_continuous(low = muted('green'), high = muted('red')) + scale_size_continuous(limits = c(6,9),range = c(3,5))) 
  dev.off()
}

Plot_EQ_Data <- function(EQ_Data){
  #Some data is weirdly at 3.95Mw so correct this to the min of 4Mw for a better histogram
  EQ_Data$Mw[EQ_Data$Mw<4] <- 4.05
  pdf('EQ_Event_Histogram.pdf')
  plot1 <- ggplot(data = EQ_Data) + geom_histogram(aes(EQ_Data$Mw),fill = NA,color = 'black',binwidth = 0.2) + scale_x_continuous(name = 'Mw') + theme_bw() + scale_y_continuous(name = 'Frequency')
  print(plot1)
  dev.off()
  pdf('EQ_EventvsYearPlot.pdf')
  graph <- ggplot(aes(x=EQ_Data$Year,y=EQ_Data$Mw),data=EQ_Data,size=0.1,color=NA) + geom_point()
  print(plot)
  dev.off()
}

Plot_Fault_Lines <- function(FL_Data,Map){
  FL_Data2 <- fortify(FL_Data)
  colnames(FL_Data2) <- c("lon","lat","order","piece","id","group")
  pdf('EQ_Fault_Line_Plot.pdf')
  print(Map + geom_path(aes(group=group),color = 'blue', size = 0.75,data=FL_Data2))
  dev.off()
}

Plot_Fault_Lines_Mw <- function(FL_Data,Map){

  FL_Data2 <- FL_Data[,1]
  FL_Data2$IDBG <- FL_Data@data$MAXMW
  FL_Data2$id <- seq(from = 0,to = (length(FL_Data2$IDBG)-1),by=1)
  names(FL_Data2) <- c('Mw','id')
  #Convert spatial dataframe to dataframe for ggplot2
  FL_Data3 <- fortify(FL_Data2)
  colnames(FL_Data3) <- c("lon","lat","order","piece","id","group")
  #Add Mw data to the dataframe
  FL_Data2 <- join(FL_Data3,data.frame(FL_Data2@data),by='id')
  
  pdf('EQ_Fault_Line_Plot_MaxMw.pdf')
  graph <- Map + geom_path(aes(group=group,color = FL_Data2$Mw), size = 0.75,data=FL_Data2) + scale_color_gradient(name = 'Max Mw',low='dark green',high = 'red')
  plot(graph)
  dev.off()
}

