Fl_Spec_Log_Norm <- function(){
  count = 1
  #Get the Directories in variables
  setwd(workingdir2)
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/FL_AMAX',sep = '')
  list.files(path=Newdir)
  #Create a dataframe to store the variables
  vec <- vector(mode = 'numeric',length = length(list.files(path=Newdir)))
  Fault_Spec_Log_Norm <- data.frame(vec,vec)
  colnames(Fault_Spec_Log_Norm) <- c('meanlog','sdlog')
  count <- 1
  no_FL = 0
  no_FL2 = 0
  no_FL3 = 0
  
  #List through all Fault line .csv files
  for (file in list.files(path=Newdir)){
    Fault_Data_temp <- read.csv(file = paste(Newdir,'/',file,sep=''), header = TRUE, sep=',')
    #print(Fault_Data_temp)
    if(length(Fault_Data_temp[,1]) > 0){
      no_FL3 = no_FL3 + 1
    }

    #Create the AMAx series
    Year <- c(1900:2006)
    AMAX <- vector(mode='numeric',length = length(Year))
    for (i in 1:length(Year)){
      if (Year[i] %in% Fault_Data_temp$Year){
        AMAX[i] = max(Fault_Data_temp[Fault_Data_temp$Year==Year[i],]$Mw)
      }else{
        AMAX[i] <- 0
      }
    }
    if (length(AMAX[AMAX>0])>0){
      no_FL2 = no_FL2 + 1
      AMAX <- data.frame(Year,AMAX)
      if (count == 1){
        df.AMAX <- AMAX
        count = count + 1
      }
      df.AMAX <- rbind(df.AMAX,AMAX)
      
    }
  }
  #colnames(df.AMAX) <- c('left','right')
  df.AMAX2 <- df.AMAX
  df.AMAX$Year <- df.AMAX$AMAX
  df.AMAX$AMAX[df.AMAX$AMAX == 0] <- 4
  colnames(df.AMAX) <- c('left','right')
  return(df.AMAX)
}


