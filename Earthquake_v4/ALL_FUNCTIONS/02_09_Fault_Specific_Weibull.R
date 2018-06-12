Fl_Spec_Weibull_Mw <- function(ret_p){
  #Get the Directories in variables
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/FL_AMAX',sep = '')
  list.files(path=Newdir)
  #Create a dataframe to store the variables
  vec <- vector(mode = 'numeric',length = length(list.files(path=Newdir)))
  Fault_Spec_Weibull_Mw <- data.frame(vec,vec)
  colnames(Fault_Spec_Weibull_Mw) <- c('scale','shape')
  count <- 1
  no_FL = 0
  no_FL2 = 0
  no_FL3 = 0
  rm(AMAX)
  rm(AMAX_Data_check)
  #List through all Fault line .csv files
  for (file in list.files(path=Newdir)){
    #print(file)
    Fault_Data_temp <- read.csv(file = paste(Newdir,'/',file,sep=''), header = TRUE, sep=',')
    #print(Fault_Data_temp)

    no_FL3 = no_FL3 + 1


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

    if (length(AMAX[AMAX>4])>0){
      no_FL2 = no_FL2 + 1
      AMAX <- data.frame(Year,AMAX)

      #Perform Max_Likelihood weibull_Mw Estimation on Values
      source('02_03_Get_Distribution_Weibull_V3.R')
      MLE_values <- fit_distribution(AMAX)
      if (no_FL3 == 54){
        AMAX_Data_check2 <- AMAX
        AMAX_Data_check <- data.frame(rep(AMAX$AMAX,20000))
        colnames(AMAX_Data_check) <- c('AMAX')
        MLE_values[2,1]
        print(file)
        x <- data.frame(rweibull(length(AMAX_Data_check$AMAX),MLE_values[1,1],MLE_values[2,1]))
        AMAX_Data_check[AMAX_Data_check$AMAX<4,] <- runif(dim(AMAX[AMAX_Data_check$AMAX<4,])[1],0,4)
        colnames(x) <- c('values')
        graph <- ggplot(data=AMAX_Data_check, aes(AMAX_Data_check$AMAX)) + geom_histogram(aes(y =..density..),bins = 20,fill = NA,color = 'black')# + geom_density(fill = 'red',col=2,alpha = 0.4)
        graph <- graph +  geom_density(aes(values),alpha = 0.4,fill = 'blue',data = x) + theme_bw() + scale_x_continuous(name = 'AMAX (Mw)') + scale_y_continuous(name = 'Density')
        pdf('Worst_FL_Example.pdf')
        plot(graph)
        dev.off()
        
        MLE_values
        values <- MLE_values
        weibull_Mw = qweibull((1-1/ret_p),scale =values['scale','Estimate'],shape = values['shape','Estimate'])
        print(weibull_Mw)
        toplot <- data.frame(ret_p,weibull_Mw)
        pdf('EQ_weibull_FL_Plot.pdf')
        graph <- ggplot(aes(x=toplot$ret_p,y=toplot$weibull_Mw,color=toplot$weibull_Mw),data=toplot,size=0.5 ) + geom_point()
        graph <- graph + scale_color_gradient(low='dark green',high='dark red') + guides(fill=FALSE)+ scale_x_continuous(name = 'Return Period (Years)')+
          scale_y_continuous(name = 'Magnitude (Mw)') + theme_bw()
        print(graph)
        dev.off()
      }
      
      #Assign values of MLE to dataframe
      Fault_Spec_Weibull_Mw$scale[count] <- MLE_values['scale','Estimate']
      Fault_Spec_Weibull_Mw$shape[count] <-MLE_values['shape','Estimate']
      count = count+ 1
    }else{
      #Assign values of MLE to dataframe
      Fault_Spec_Weibull_Mw$scale[count] <- 0
      Fault_Spec_Weibull_Mw$shape[count] <- 0
      count = count+ 1
    }
    

  }

  print(length(Fault_Spec_Weibull_Mw$scale[Fault_Spec_Weibull_Mw$scale==Fault_Spec_Weibull_Mw$scale[1]]))
  #reset directory and write data to a csv
  setwd(Olddir)
  write.csv(Fault_Spec_Weibull_Mw,file = 'Fault_Spec_Weibull.csv')
  return(Fault_Spec_Weibull_Mw)
}







get_rp_Mw <- function(Fault_Spec_Weibull,ret_p){
  
  #Set up dataframe to hold values
  #Fault_Spec_Weibull_Mw <- Fl_Spec_Weibull_Mw(ret_p)
  weibull_Mw <- vector(mode = 'numeric', length = length(Fault_Spec_Weibull$shape))
  weibull_Mw <- data.frame(rep(list(weibull_Mw),length(ret_p)))
  colnames(weibull_Mw) <- as.character(ret_p)
  max(Fault_Spec_Weibull$scale)
  fl2 <- Fault_Spec_Weibull[Fault_Spec_Weibull$shape==max(Fault_Spec_Weibull$shape),]
  qweibull((1-1/ret_p),scale = fl2[1,1],shape = fl2[1,2])
  #Find the correct Mw for a given return period CHECK THIS!!
  for (i in 1:length(Fault_Spec_Weibull$scale)){
    FLweibull_Mw <- qweibull((1-1/ret_p),scale = Fault_Spec_Weibull$scale[i],shape = Fault_Spec_Weibull$shape[i])
    weibull_Mw[i,] <- FLweibull_Mw
  }
  
  #weibull_Mw_Mw <- weibull_Mw_Mw[weibull_Mw_Mw['10'] > 0,]
  Fault_Spec_Weibull <- data.frame(Fault_Spec_Weibull,weibull_Mw)
  return(Fault_Spec_Weibull)
}


