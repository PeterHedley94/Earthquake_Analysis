

Combine_FL_Diss <- function(){
  setwd(workingdir2)
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/FL_DISS_RASTER',sep = '')
  setwd(Newdir)
  combfilename  = 'COMBINED_FAULTS.csv'
  
  for (file in list.files(path = Newdir)){
    
    FL2 <- read.csv(file, header = FALSE,skip = 6,sep=" ")
    
    if (!is.element(filename,list.files(path=Newdir))){
      matrix <- as.matrix(FL2)
      epi.asc(matrix, filename, Population_Data@bbox[1,1], Population_Data@bbox[2,1], Population_Data@grid@cellsize[2], na = 0)
    }else{
      FLCOMB <- read.csv(combfilename, header = FALSE,skip = 6,sep=" ")
      FL3 <- log10(10**FLCOMB + 10**FL2)
      matrix <- as.matrix(FL3)
      epi.asc(matrix, filename, Population_Data@bbox[1,1], Population_Data@bbox[2,1], Population_Data@grid@cellsize[2], na = 0)
    }
  }
}


