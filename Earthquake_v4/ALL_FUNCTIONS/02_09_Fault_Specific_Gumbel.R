Fl_Spec_Gumbel <- function(){
  #Get the Directories in variables
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/FL_AMAX',sep = '')
  list.files(path=Newdir)
  #Create a dataframe to store the variables
  vec <- vector(mode = 'numeric',length = length(list.files(path=Newdir)))
  Fault_Spec_Gumbel <- data.frame(vec,vec,vec,vec)
  colnames(Fault_Spec_Gumbel) <- c('mu','alpha','stdevmu','stdevalpha')
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
    }
    AMAX <- data.frame(Year,AMAX)
    
    #Perform Max_Likelihood Gumbel Estimation on Values
    source('02_03_Maximum_Likelihood_Gumbel1.R')
    MLE_values <- get_Gumbel(AMAX)
    
    #Assign values of MLE to dataframe
    Fault_Spec_Gumbel$mu[count] <- MLE_values['mu','Estimate']
    Fault_Spec_Gumbel$alpha[count] <-MLE_values['alpha','Estimate']
    Fault_Spec_Gumbel$stdevmu[count] <-MLE_values['mu','Std..Error']
    Fault_Spec_Gumbel$stdevalpha[count] <-MLE_values['alpha','Std..Error']
    count = count+ 1
  }
  
  print(length(Fault_Spec_Gumbel$mu[Fault_Spec_Gumbel$mu==Fault_Spec_Gumbel$mu[1]]))
  #reset directory and write data to a csv
  setwd(Olddir)
  write.csv(Fault_Spec_Gumbel,file = 'Fault_Spec_Gumbel.csv')
  return(Fault_Spec_Gumbel)
}







get_rp_Mw <- function(Fault_Spec_Gumbel,ret_p){
  
  #Set up dataframe to hold values
  Fault_Spec_Gumbel <- Fl_Spec_Gumbel()
  Gumbel_Mw <- vector(mode = 'numeric', length = length(Fault_Spec_Gumbel$mu))
  Gumbel_Mw <- data.frame(rep(list(Gumbel_Mw),length(ret_p)))
  colnames(Gumbel_Mw) <- as.character(ret_p)
  
  
  #Find the correct Mw for a given return period CHECK THIS!!
  for (i in 1:length(Fault_Spec_Gumbel$mu)){
    FLGumbel_Mw <- qGumbel((1-1/ret_p),mu = Fault_Spec_Gumbel$mu[i],sigma = Fault_Spec_Gumbel$alpha[i])
    Gumbel_Mw[i,] <- FLGumbel_Mw
  }
  
  #Gumbel_Mw <- Gumbel_Mw[Gumbel_Mw['10'] > 0,]
  Fault_Spec_Gumbel <- data.frame(Fault_Spec_Gumbel,Gumbel_Mw)
  return(Fault_Spec_Gumbel)
}


