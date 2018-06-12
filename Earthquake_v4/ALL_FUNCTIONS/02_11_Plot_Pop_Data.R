
plotPop <- function(Pop.sp2, Map,Pop.sp3.points){
  Pop.sp2 <- Population_Data
  Pop.sp3 <- as(Pop.sp2, "SpatialPolygonsDataFrame")
  Pop.sp3@polygons[[1]]@Polygons[[1]]@coords
  Pop.sp3@data$id = seq(1,length(Pop.sp3@polygons),by = 1)
  #Pop.sp3.points = fortify(Pop.sp3, region="id")
  #save(Pop.sp3.points,file='FORTIFIED_v2.R')
  load('FORTIFIED_v2.R')
  Pop.df = join(Pop.sp3.points, Pop.sp3@data, by="id")
  colnames(Pop.df) <- c('lon','lat','order','hole','piece','id','group','population')
  graph <- Map + geom_polygon(aes(group=Pop.df$id),data=Pop.df) + scale_color_gradient(low = 'green', high = 'red')+scale_fill_gradient(low = "green", high = "red")
  pdf('Population_Data.pdf')
  print(graph)
  dev.off()
}









