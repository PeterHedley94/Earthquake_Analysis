

Create_Risk_CSV <- function(Population_Data,Dissipation_coeff,Distribution,Risk_Coeff){
  
  population_file <- 'C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/tur_gpwv3_pcount_ascii_25/turp00ag.asc'
  Pop_Data <- read.csv(population_file, header = FALSE,skip = 6,sep=" ")
  Pop_Data <- Pop_Data[,1:528]
  
  #Newdir <- paste(getwd(),'/FL_DISS_RASTER','/',Distribution,as.character(Dissipation_coeff),sep = '')
  
  Mw_Raster_file <- paste('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS/',Distribution,'/FL_DISS_RASTER/',as.character(Dissipation_coeff),'/COMBINED_FAULTS.csv',sep = '')
  
  #Mw_Raster_file <- 'C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS/FL_DISS_RASTER/COMBINED_FAULTS.csv'
  #Mw_Data <- read.asc(Mw_Raster_file)
  Mw_Data <- read.csv(Mw_Raster_file, header = FALSE,sep=" ")
  #head(Mw_Data)

  Risk <- 10^Mw_Data*(Pop_Data^Risk_Coeff)
  Risk <- as.matrix(Risk)
  Risk <- log10(Risk)
  max(Risk)
  
  
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/',Distribution,'/FL_DISS_RASTER','/',as.character(Dissipation_coeff),sep = '')
  
  setwd(Newdir)
  matrix(rep(c(1,1,1), 3), nrow = 3)
  combfilename  = paste('Risk_Data', as.character(Risk_Coeff),'.asc')
  matrix1 <- raster(Risk,xmn=Population_Data@bbox[1,1], xmx=Population_Data@bbox[1,2], ymn=Population_Data@bbox[2,1], ymx = Population_Data@bbox[2,2])
  matrix1 <- focal(matrix1,fun = mean, w = matrix(rep(c(1,1,1), 3), nrow = 3))
  raterbkup <- matrix
  plot(matrix1)
  matrix <- asc.from.raster(matrix1)
  
  #matrix
  write.asc(matrix, file = combfilename)
  
  setwd(Olddir)
}



plot_Risk <- function(Population_Data, Map,Dissipation_coeff,Distribution,Risk_Coeff){
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/',Distribution,'/FL_DISS_RASTER','/',as.character(Dissipation_coeff),sep = '')
  Newdir2 <- paste(getwd(),'/PLOTS/',Distribution,'/',as.character(Dissipation_coeff),sep='')
  setwd(Newdir)
  Risk_Raster_file <- paste('Risk_Data', as.character(Risk_Coeff),'.asc')
  Risk_Data <- read.asc(Risk_Raster_file)
  Risk <- raster.from.asc(Risk_Data)
  #plot(Risk)
  
  summary(Risk@data@values)
  Risk@data@values[which(Risk@data@values<13)] <- 7
  Risk@data@values[which(Risk@data@values<15 & Risk@data@values>=13)] <- 8
  Risk@data@values[which(Risk@data@values<17 & Risk@data@values>=15)] <- 9
  Risk@data@values[which(Risk@data@values<19 & Risk@data@values>=17)] <- 10
  Risk@data@values[which(Risk@data@values<21 & Risk@data@values>=19)] <- 11
  Risk@data@values[which(Risk@data@values<23 & Risk@data@values>=21)] <- 12
  Risk@data@values[Risk@data@values>=23] <- 13

  Risk_Data <-asc.from.raster(Risk)
  
  Mw.sp1 <- sp.from.asc(Risk_Data, projs = CRS(as.character("+init=epsg:4326")))
  plot(Mw.sp1)
  
  Mw.sp2 <- Mw.sp1[Mw.sp1@data$z > 0,]
  
  Mw.sp3 <- as(Mw.sp2, "SpatialPolygonsDataFrame")
  #plot(Mw.sp3)
  
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS/DATA/TUR_adm_shp/')
  TUR <- readOGR(".", "TUR_adm0")
  TUR <- spTransform(TUR, CRSobj = CRS(proj4string(Mw.sp2)))

  #Mw.sp2 <- Mw.sp2[TUR,]
  TRy50 <- data.frame(Mw.sp2)
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  
  Tur.f = data.frame(TUR)
  Tur.f <- tidy(TUR)
  
  
  
  TRy50$z <- round(TRy50$z)
  summary(TRy50$z)


  #TRy50$z[TRy50$z<230] <- 1
  TRy50$z <- as.character(TRy50$z)
  
  #min(TRy50$z)
  Newdir <- paste(getwd(),'/',Distribution,'/FL_DISS_RASTER','/',as.character(Dissipation_coeff),sep = '')
  setwd(Newdir)
  
  
  cols <- c("13" = "firebrick4", "12" = 'firebrick3', "11" = "orangered3","10" = "orangered","9" = "darkorange", "8" = "yellow2","7" = "lightgoldenrod")#
  graph2 <- ggplot(aes(x = s1, y = s2),data = TRy50) + geom_raster(aes(fill = z),size = 0.01) + scale_fill_manual(name = 'Risk', values = cols,breaks = c(7:13))
  graph2 <- graph2 +  geom_path(aes(group = group, x = long, y = lat),data = Tur.f, color = 'black', size = 1) + theme_void()
  
  graph2
  
  pdf(paste('Risk_Map', as.character(Risk_Coeff),'.pdf'))
  print(graph2)
  dev.off()
  setwd(Olddir)
  #return(Mw.df)
  
}

