#Get the Historical EQ Data
source('02_01_Get_Data_Ready.R')
EQ_Data <- get_EQ_data()
FL_Data <- get_FL_data()


FL_Data2 <- FL_Data
FL_Data2$id <- seq(from = 0,to = (length(FL_Data2$IDBG)-1),by=1)

#Convert spatial dataframe to dataframe for ggplot2
FL_Data3 <- fortify(FL_Data2)
colnames(FL_Data3) <- c("lon","lat","order","piece","id","group")
#Add Mw data to the dataframe
FL_Data3 <- join(FL_Data3,data.frame(FL_Data2@data),by='id')

unique(FL_Data3$group)
count = 0

for(i in 1:length(unique(FL_Data3$group))){
  tempdata <- FL_Data3[FL_Data3$group == unique(FL_Data3$group)[i],]
  deltalong <- tempdata$lon[length(tempdata$lon)] - tempdata$lon[1]
  deltalat <- tempdata$lat[length(tempdata$lat)] - tempdata$lat[1]
  deltadeg <- sqrt(deltalong ^2 + deltalat^2)*110
  if(deltadeg > 250){
    #FL_Data3 <- FL_Data3[FL_Data3$group != unique(FL_Data3$group)[i],]
    tempdata$group[length(tempdata$group)/2:length(tempdata$group)] <- max(as.numeric(FL_Data3$group))+1
    class(FL_Data3$group[1])
    print(as.factor(as.numeric(FL_Data3$group[1])))
    max(as.numeric(FL_Data3$group))+1
  }
 
}
listdf <- vector('list',length(FL_Data3$group))


for (i in 1:length(unique(FL_Data3$group))){

  lon = FL_Data3[FL_Data3$group == unique(FL_Data3$group)[i],]$lon
  lat = FL_Data3[FL_Data3$group == unique(FL_Data3$group)[i],]$lat
  templist2 <- cbind(lon,lat)
  templist <- Line(templist2)
  listdf[[i]] <- cbind(listdf,Lines(list(templist), ID = as.character(unique(FL_Data3$group)[i])))
}
listdf <- Lines(listdf)
SLDF = SpatialLinesDataFrame(listdf,data = FL_Data3,match.ID = TRUE )

names(listdf) <- as.character(unique(FL_Data3$group))

proj4string(FL_Data)

SpatialLines(listdf, proj4string = proj4string(FL_Data))

