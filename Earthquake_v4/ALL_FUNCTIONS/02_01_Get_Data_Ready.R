get_EQ_data <- function(){
  
  file1 <- paste(workingdir1,'/Data/SHAREver3.3.csv',sep = '')
  Hist_data1 <- read.csv(file1, header=TRUE, sep=",")
  
  #Just get the data for Turkey
  Hist_data1$Long <- as.numeric(Hist_data1$Lon)
  Hist_data1 <- Hist_data1[Hist_data1$Lon>23,]
  Hist_data1 <- Hist_data1[Hist_data1$Lon<50,]
  Hist_data1$Lat <- as.numeric(Hist_data1$Lat)
  Hist_data1 <- Hist_data1[Hist_data1$Lat>30,]
  Hist_data1 <- Hist_data1[Hist_data1$Lat<45,]
  Hist_data1$Year = as.numeric(Hist_data1$Year)
  Hist_data1 <- Hist_data1[Hist_data1$Year>1899,]
  Hist_data1$Mw = as.numeric(Hist_data1$Mw)
  Hist_data1 = data.frame(Hist_data1$Year,Hist_data1$Lon,Hist_data1$Lat,Hist_data1$Mw)
  colnames(Hist_data1) <- c('Year','lon','lat','Mw')
  return(Hist_data1)
}

get_FL_data <- function(){
  dir = paste(workingdir1,'/ALL_FUNCTIONS/DATA/FSBGmodelv6.1',sep = '')
  file <- "FSBGModelV6.1_FaultSources"
  FL_Data <- readOGR(dsn = dir, layer = file)
  return(FL_Data)
}

get_Map <- function(){
  Map <- ggmap(get_map(maptype='terrain',location = 'Turkey',crop=FALSE,zoom = 5))
  return(Map)
}

get_pop_Data <- function(){
  population_file <- paste(workingdir1,'/tur_gpwv3_pcount_ascii_25/turp00ag.asc',sep = '')
  Population_Data <- read.csv(population_file, header = FALSE,skip = 6,sep=" ")
  Pop <- raster(population_file)
  Pop33 <- asc.from.raster(Pop)
  Pop.sp1 <- sp.from.asc(Pop33, projs = CRS(as.character("+init=epsg:4326")))
  class(Pop.sp1)
  Pop.sp2 <- Pop.sp1[Pop.sp1@data$z > 0,]
  return(Pop.sp2)
}





