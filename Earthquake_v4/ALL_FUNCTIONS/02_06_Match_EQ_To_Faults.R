library(rgdal)
library(ggplot2)
library(png)
library(maptools)
library(broom)



Match_Eq_To_FL <- function(EQ_Data,FL_Data){
  
  #FIND THE FAULT LINE COORDINATES
  cds <-coordinates(FL_Data)
  #CREATE LIST CONTAINING THEM
  NewDataFrame <- do.call("rbind", cds)
  #TURN THE LIST INTO A DATAFRAME
  NewDataFrame <- tidy(NewDataFrame)
  #CREATE A MATRIX TO STORE THE MATCHES
  Matched_Data = matrix(,nrow = dim(EQ_Data)[1], ncol = 3)
  #ASSIGN THE EQ NUMBER, FAULT LINE NUMBER, DISTANCE TO THE FL
  colnames(Matched_Data) = c('i', 'FLNo.','Distance')
  
  #ITERATE THROUGH EVERY EQ EVENT
  for (i in 1:dim(EQ_Data)[1]){
    #ASSIGN A HIGH STARTING DISTANCE FOR FIRST VALUE
    Smallest_dist= 10^10
    #RESET FlNo
    FLNo = NULL
    
    #ITERATE THROUGH ALL FAULT LINES TO FIND THE CLOSEST
    for (i2 in 1:dim(NewDataFrame)[1]){
      
      #GET LAT AND LONG OF FL's
      Fault_DataLong = data.frame(NewDataFrame[i2,])[,1]
      Fault_DataLat = data.frame(NewDataFrame[i2,])[,2]
      #GET EQ_EVENT LAT AND LONG
      Earth_DataLong = EQ_Data$lon[i]
      Earth_DataLat = EQ_Data$lat[i]
      
      #CHECK IF THE CURRENT FAULT LINE IS THE CLOSEST
      Dist = min(sqrt((Fault_DataLong-Earth_DataLong)^2 + (Fault_DataLat-Earth_DataLat)^2))
      #ASSIGN VALUES IF SO
      if (Dist<Smallest_dist){
        Smallest_dist = Dist
        FLNo = i2
        #ID = EQ_Data$Event.ID[i]
      }
    }
    #AFTER ITERATING ALL FAULTS ASSIGN THE CLOSEST TO THAT EQ EVENT
    Matched_Data[i,1] = i
    Matched_Data[i,2] = FLNo
    Matched_Data[i,3] = Smallest_dist
    print(i)
  }
  return(Matched_Data)
}


