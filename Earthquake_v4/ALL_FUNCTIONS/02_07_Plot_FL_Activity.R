plot_FL_Act <- function(Map,FL_Data,Matched_Data){

  
  FL_Data2 <- FL_Data[,1]
  #Create Dataframe to store FL Values
  len_unique <- length(FL_Data2@lines)
  vec <- vector(mode='numeric',length=len_unique)
  Matched_FL_Data <- data.frame(vec)
  colnames(Matched_FL_Data) <- c('Activity')
  
  #Match number of EQ's to FL's
  for (i2 in Matched_Data[,2]){
    Matched_FL_Data$Activity[i2] = Matched_FL_Data$Activity[i2]+1
  }
  
  #Edit Fault line Spacial Dataframe removing irrelevant values
  FL_Data2$IDBG <- Matched_FL_Data$Activity
  FL_Data2$id <- seq(from = 0,to = (length(FL_Data2$IDBG)-1),by=1)
  names(FL_Data2) <- c('Activity','id')
  
  #Convert spatial dataframe to dataframe for ggplot2
  FL_Data3 <- fortify(FL_Data2)
  colnames(FL_Data3) <- c("lon","lat","order","piece","id","group")
  #Add activity data to the dataframe
  FL_Data3 <- join(FL_Data3,data.frame(FL_Data2@data),by='id')
  #Plots
  pdf('Fault_Line_Activity.pdf')
  graph <- Map + geom_path(aes(group=group,color = Activity), size = 0.75,data=FL_Data3) + scale_color_gradient(name = 'Activity',trans = 'log',breaks = c(0,10,50,400),low='dark green',high = 'red')
  print(graph)
  dev.off()
  
  pdf('Fault_Line_Activity_Hist.pdf')
  graph <- hist(FL_Data3$Activity[FL_Data3$Activity>0],breaks=25)
  print(graph)
  dev.off()
}

