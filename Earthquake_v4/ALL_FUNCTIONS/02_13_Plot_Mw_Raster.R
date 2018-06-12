
plotMw_Raster2 <- function(Population_Data, Map,Dissipation_coeff,Distribution){
  setwd(workingdir2)
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/',Distribution,'/FL_DISS_RASTER/',Dissipation_coeff,sep = '')
  dir.create(Newdir)
  Newdir2 <- paste(getwd(),'/',Distribution,sep = '')
  Mw_Raster_file <- paste(getwd(),'/',Distribution,'/FL_DISS_RASTER/',Dissipation_coeff,'/COMBINED_FAULTS.asc',sep = '')
  Mw_Data <- read.asc(Mw_Raster_file)

  Mw <- raster.from.asc(Mw_Data)

  Mw.sp1 <- sp.from.asc(Mw_Data, projs = CRS(as.character("+init=epsg:4326")))

  class(Mw.sp1)
  Mw.sp2 <- Mw.sp1[Mw.sp1@data$z > 0,]
  Mw.sp3 <- as(Mw.sp2, "SpatialPolygonsDataFrame")
  setwd(paste(workingdir2,'/DATA/TUR_adm_shp/',sep = ''))
  TUR <- readOGR(".", "TUR_adm0")
  #TUR <- sp.from.asc(Mw33, projs = CRS(as.character("+init=epsg:4326")))
  TUR <- spTransform(TUR, CRSobj = CRS(proj4string(Mw.sp2)))
  summary(TUR)
  summary(Mw.sp2)
  Mw.sp2 <- Mw.sp2[TUR,]
  TRy50 <- data.frame(Mw.sp2)
  
  setwd(workingdir2)
  Newdir <- paste(getwd(),'/',Distribution,'/FL_DISS_RASTER/',Dissipation_coeff,sep = '')
  setwd(Newdir)
  Tur.f = data.frame(TUR)
  #save.image('Plot.RData')
  Tur.f <- tidy(TUR)
  bTry50 <- TRy50
  TRy50$z <- round(TRy50$z)
  TRy50$z <- as.character(TRy50$z)
  
  save.image('ReadytoPlot.RData')
  cols <- c("9" = "firebrick4", "8" = 'firebrick3', "7" = "orangered3","6" = "orangered","5" = "darkorange", "4" = "yellow2","3" = "whitesmoke")
  graph2 <- ggplot(aes(x = s1, y = s2),data = TRy50) + geom_raster(aes(fill = z),size = 0.01) + scale_fill_manual(name = 'Mw', values = cols,breaks = c(4,5,6,7,8))
  graph2 <- graph2 +  geom_path(aes(group = group, x = long, y = lat),data = Tur.f, color = 'black',size = 1) + theme_void()
  
  pdf('Mw_Raster_Data.pdf')
  print(graph2)
  dev.off()
  setwd(Olddir)

}

