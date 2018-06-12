Export_Fl_Spec_Data <- function(EQ_Data,FL_Data){
  
  Matched_Faults = Matched_Data[,2]
  ALL_Data = data.frame(Matched_Faults,EQ_Data$Year,EQ_Data$Mw)
  colnames(ALL_Data) <- c('Fault','Year','Mw')
  faults = c(1:length(FL_Data@lines))
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/FL_AMAX',sep = '')
  setwd(Newdir)
  count <- 0
  for (i in faults){
    Datatocsv = ALL_Data[ALL_Data$Fault==i,]
    if(length(Datatocsv[,1])>0){
      count = count + 1
    }
    filename = paste('AMAX FAULT_', as.character(1000 + faults[i]),'.csv')
    write.csv(Datatocsv,filename,row.names = FALSE)
  }
  print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
  print(count)
  setwd(Olddir)
}
