libraries <- c('rgdal','ggplot2','maptools','broom','ggmap','plyr')
for (i in libraries){
  require(i)
}


gpclibPermit()
options(expressions=100000)

dir = "C:/Users/Peter/Documents/Earthquake Turkey_160211/FSBGmodelv6.1"
dir2 = 'C:/Users/Peter/Documents/Earthquake Turkey_160211/Historical Data.txt'
dir4 = 'C:/Users/Peter/Documents/Earthquake Turkey_160211/EARTH DATA/clc00_c111'
file1 = 'clc00_c111'
file = "FSBGModelV6.1_FaultSources"

lnd <- ggmap(get_map(maptype='terrain',location = 'Turkey',crop=FALSE,zoom = 5))  # download map data for the lnd data and plot
load('C:/Users/Peter/Documents/Earthquake Turkey_160211/Matched_EQtoFaultLines.rda')

shape <- readOGR(dsn = dir, layer = file)

EQ_Data <- read.csv(dir2, header=TRUE, sep="\t")
quiet <- list(xquiet, yquiet)
cds <-coordinates(shape)
cds = tidy(cds)

NewDataFrame <- do.call("rbind", cds)
NewDataFrame <- tidy(NewDataFrame)
typeof(NewDataFrame)

typeof(NewDataFrame[2,1][[1]])


typeof(NewDataFrame[2,1][1])
typeof(NewDataFrame[2,1][1][1][1])
NewDataFrame[2,1][1][[1]]

x = NewDataFrame[2,1][[1]][,1,drop=FALSE]
x = NewDataFrame[2,1][[1]][,1,drop=FALSE]

Matched_Data2 = data.frame(DATA =rep(1, dim(NewDataFrame)[1]))
x = c(3:5)

for (i2 in Matched_Data[,3]){
  Matched_Data2$DATA[i2] = Matched_Data2$DATA[i2]+1
}

graph <-lnd
dim(NewDataFrame)[1]
for (i2 in 1:dim(NewDataFrame)[1]){
  if (Matched_Data2$DATA[i2]>1){
    lat = NewDataFrame[i2,1][[1]][,2,drop=FALSE]
    long = NewDataFrame[i2,1][[1]][,1,drop=FALSE]
    No.EQ = rep(Matched_Data2$DATA[i2],length(long))
    Fault_EQ_Data <- data.frame(long,lat,No.EQ)
    r = 0
    g = 0
    b = 1
    shape.f <- list(geom_path(aes(x = long, y = lat),colour = rgb(r,g,b), size = 0.1,data=Fault_EQ_Data))
    graph = graph + shape.f
  }
}

graph

#dir2 = 'C:/Users/Peter/Documents/Earthquake Turkey_160211/Historical Data.txt'

#EQ_Data <- read.csv(dir2, header=TRUE, sep="\t")
#EQEvents <- c(geom_point(aes(x = Longitude, y = Latitude,colour = Mw),size = 0.1,data=EQ_Data))
#graph + EQEvents
