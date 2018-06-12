require(SDMTools)


plotMw_Raster <- function(Map){
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/FL_DISS_RASTER',sep = '')
  setwd(Newdir)
  Newdir2 <- paste(getwd(),'/PLOTS',sep='')
  combfilename  = 'COMBINED_FAULTS.csv'
  
  for (file in list.files(path = Newdir,pattern = '.csv')){
    print(file)
    Mw <- raster(file)
    Mw2 <- read.csv(file,skip = 6,sep = ' ')
    print(max(Mw2))
    if(max(Mw2) >0){
      setwd(Newdir2)
      filename <- paste(file,'PLOT.pdf')
      #pdf(filename)
      #plot(Mw)
      #dev.off()
      Mw33 <- asc.from.raster(Mw)
      Mw.sp1 <- sp.from.asc(Mw33, projs = CRS(as.character("+init=epsg:4326")))
      class(Mw.sp1)
      Mw.sp2 <- Mw.sp1[Mw.sp1@data$z > 0,]
      print(getwd())
      pdf(filename)
      plot(Mw.sp2)
      dev.off()
      
      setwd(Newdir)
    }

  }
}


  

  