

plot_Risk <- function(Population_Data, Map,Dissipation_coeff,Distribution,Risk_Coeff){
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  
  coords2 = EQ_Data[,2:3]
  Mw.sp2 <- SpatialPoints(coords2,proj4string = CRS(as.character("+init=epsg:4326")))
  
  #df = data.frame(EQ_Data['Mw'])
  Mw.sp3 <- SpatialPointsDataFrame(Mw.sp2, data=EQ_Data[,2:4], proj4string = CRS(as.character("+init=epsg:4326")))
  
  #plot(Mw.sp3)
  
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS/DATA/TUR_adm_shp/')
  TUR <- readOGR(".", "TUR_adm0")
  TUR <- spTransform(TUR, CRSobj = CRS(proj4string(Mw.sp3)))

  Mw.sp3 <- Mw.sp3[TUR,]
  TRy50 <- data.frame(Mw.sp3)
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  
  Tur.f = data.frame(TUR)
  Tur.f <- tidy(TUR)
  
  TRy50 <- TRy50[c('lon','lat','Mw')]


  
  pdf('EQ_Event_Mw_Plot.pdf')
  graph <- ggplot(aes(x = lon, y = lat),data = TRy50) + geom_point(aes(colour = Mw),size=0.3,alpha = 0.5,data=subset(TRy50,TRy50$Mw <6))
  graph2 <- graph + geom_point(aes(x=lon,y=lat,colour = Mw,size=Mw),data=subset(TRy50,TRy50$Mw>6)) + scale_color_continuous(low = muted('green'), high = muted('red')) + scale_size_continuous(limits = c(6,9),range = c(3,5))
  graph3 <- graph2 + geom_path(aes(group = group, x = long, y = lat),data = Tur.f, color = 'black', size = 1) + theme_void() + coord_fixed(ratio = 1.0)
  graph3
  plot(graph3)
  # + scale_alpha_continuous(limits = c(6,9),range = c(0.6,1))
  dev.off()
  
  
  
  

  graph2 <- ggplot(aes(x = s1, y = s2),data = TRy50) + 
  graph2 <- graph2 +  
  
  graph2
  
  pdf(paste('Mw_Poster_Plot.pdf'))
  print(graph2)
  dev.off()
  setwd(Olddir)
  #return(Mw.df)
  
}

