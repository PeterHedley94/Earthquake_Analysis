require(epiR)
require(plyr)
require(raster)
require(broom)
require(SDMTools)
require(ggplot2)

rotate_clockwise <- function(x) { t(apply(x, 2, rev))}

Combine_FL_Diss <- function(Population_Data,Dissipation_coeff,Distribution){
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/',Distribution,'/FL_DISS_RASTER/',Dissipation_coeff,sep = '')
  setwd(Newdir)
  combfilename  = 'COMBINED_FAULTS.csv'
  count = 0
  degpersqr = Population_Data@grid@cellsize[2]
  for (file in list.files(path = Newdir,pattern = '.csv')){
    
    FL2 <- read.csv(file, header = FALSE,sep=" ")
    #FL2 <- asc.from.raster(FL2)
    rownames(FL2) <- seq(Population_Data@bbox[2,1],Population_Data@bbox[2,2]-degpersqr, by = degpersqr)
    colnames(FL2) <- seq(Population_Data@bbox[1,1],Population_Data@bbox[1,2]-degpersqr, by = degpersqr)
    
    #FL2 <- Mw_Data
    if (!is.element(combfilename,list.files(path=Newdir))){
      matrix <- as.matrix(FL2)
      #epi.asc(matrix, file = combfilename, Population_Data@bbox[1,1], Population_Data@bbox[2,1], Population_Data@grid@cellsize[2], na = 0)
      write.table(matrix, file = combfilename,col.names = FALSE,row.names = FALSE,sep = ' ')
    }else{
      FLCOMB <- read.csv(combfilename, header = FALSE,sep=" ")
      rownames(FLCOMB) <- seq(Population_Data@bbox[2,1],Population_Data@bbox[2,2]-degpersqr, by = degpersqr)
      colnames(FLCOMB) <- seq(Population_Data@bbox[1,1],Population_Data@bbox[1,2]-degpersqr, by = degpersqr)
      
      
      if(!dim(FL2)[2] == Population_Data@grid@cells.dim[1]){
        print(file)
        #colnames(FLCOMB) <- seq(1,dim(FLCOMB)[2],by = 1)
        #FLCOMB <- matrix(FLCOMB)
        print('ALTERED FL')
        FL2 <- rotate_clockwise(FL2)

        #FL2 <- t(FL2)
      }
      #print(dim(FL2))
      #Population_Data@grid@cells.dim[1]
      #dim(FL2)[2]
      if(!dim(FLCOMB)[2] == Population_Data@grid@cells.dim[1]){
        print(file)
        print('ALTERED COMBINED')
        FLCOMB <- rotate_clockwise(FLCOMB)
      }
      FL3 <- log10(10**FLCOMB + 10**FL2)
      matrix <- as.matrix(FL3)
      count  = count + 1
      if (count == length(list.files(path = Newdir,pattern = '.csv'))){
        matrix <- round_any(matrix,1)
        matrix[matrix <4] <- 0
        head(matrix)
      }
      #epi.asc(matrix, file = combfilename, Population_Data@bbox[1,1], Population_Data@bbox[2,1], Population_Data@grid@cellsize[2], na = 0)
      write.table(matrix, file = combfilename,col.names = FALSE,row.names = FALSE,sep = ' ')
    }
  }
  combfilename  = 'COMBINED_FAULTS.asc'
  matrix <- raster(matrix,xmn=Population_Data@bbox[1,1], xmx=Population_Data@bbox[1,2], ymn=Population_Data@bbox[2,1], Population_Data@bbox[2,2])
  raterbkup <- matrix
  matrix <- asc.from.raster(matrix)
  #matrix
  write.asc(matrix, file = combfilename)
  #epi.asc(matrix, file = combfilename, Population_Data@bbox[1,1], Population_Data@bbox[2,1], Population_Data@grid@cellsize[2], na = 0)
}



# foo = rotate_clockwise(foo)
# #mback <- matrix
# rstr <- raster(foo)
# plot(rstr)
