

Dissipate_Fault_Lines <- function(Return_Period_Mw,FL_Data,Population_Data,Dissipation_coeff,Distribution){
  #Return_Period_Mw <- Fault_Spec_Gamma
  setwd(workingdir2)
  FL_Data2 <- FL_Data[,1]
  FL_Data2$IDBG <- Return_Period_Mw$X500
  FL_Data2$id <- seq(from = 0,to = (length(FL_Data2$IDBG)-1),by=1)
  names(FL_Data2) <- c('Mw','id')
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/',Distribution,'/FL_DISS_RASTER/',Dissipation_coeff,sep = '')
  dir.create(Newdir)
  
  
  #Convert spatial dataframe to dataframe for ggplot2
  FL_Data3 <- fortify(FL_Data2)
  colnames(FL_Data3) <- c("lon","lat","order","piece","id","group")
  #Add Mw data to the dataframe
  FL_Data3 <- join(FL_Data3,data.frame(FL_Data2@data),by='id')
  
  #CHANGE THIS WHEN FIX MW PROBLEM!!!!!
  FL_Data3 <- FL_Data3[FL_Data3$Mw>4,]
  FL_Data3 <- FL_Data3[is.finite(FL_Data3$Mw),]
  longest_dist <- 0
  
  
  for (i in unique(FL_Data3$id)){

    tempdata <- FL_Data3[FL_Data3$id == i,]
    if (dim(tempdata)[1] > longest_dist){
      print(i)
      longest_dist <- dim(tempdata)[1]
      }
    Loncheck <- dim(tempdata[tempdata$lon>Population_Data@bbox[1,1],])[1]
    Loncheck2 <- dim(tempdata[tempdata$lon<Population_Data@bbox[1,2],])[1]
    Latcheck <- dim(tempdata[tempdata$lat<Population_Data@bbox[2,2],])[1]
    Latcheck2 <- dim(tempdata[tempdata$lat>Population_Data@bbox[2,1],])[1]
    if(Loncheck >0 & Loncheck2 >0 & Latcheck >0 & Latcheck2>0){
      #print('no change')
    }else{
      FL_Data3 <- FL_Data3[!FL_Data3$id == i,]
    }
  }
  for (i in unique(FL_Data3$id)){
    #print(i)
  }
  
  source('03_02_Data_Dissipate_FL_Matrix_TESTING_v5.R')
  #for (i in c(438,696,439,250,670)){
  #for (i in c(696)){
  for (i in unique(FL_Data3$id)){
    tempFL_Data <- FL_Data3[FL_Data3$id == i,]
    coordsx <- tempFL_Data[,'lon']
    coordsy <- tempFL_Data[,'lat']
    df <- data.frame(coordsx,coordsy)
    Map + geom_path(aes(x = coordsx, y =coordsy),data = df)
    plot(coordsx,coordsy)
    Mw <- tempFL_Data[1,'Mw']
    print(paste('FAULT NO. : ', as.character(i),sep = ''))
    matrix <- dissipate_FL_Mw(Population_Data,coordsx,coordsy,Mw,Dissipation_coeff)
    print(paste('Matrix Max <- ' , as.character(max(matrix)),sep = ''))
    #plot(raster(matrix))
    setwd(Newdir)
    filename = paste('AMAX FAULT_', as.character(i),'.csv')
    #print('colnames')
    #print(colnames(matrix))
    #print('rownames m1')
    #print(rownames(matrix))
    #write.asc()
    #epi.asc(matrix, filename, Population_Data@bbox[1,1], Population_Data@bbox[2,1], Population_Data@grid@cellsize[2], na = 0)
    write.table(matrix, filename,col.names = FALSE,row.names = FALSE,sep=' ')
    print(paste('Completed Fault: ',as.character(i)))
    print(length(rownames(matrix)))
    print(length(colnames(matrix)))
    setwd(Olddir)
    
  }
}
#ggplot(aes(group = group,x = lon, y = lat), data = FL_Data3[,1:6]) + geom_path()
