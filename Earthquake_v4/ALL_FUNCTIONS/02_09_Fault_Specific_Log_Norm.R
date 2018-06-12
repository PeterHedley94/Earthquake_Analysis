Fl_Spec_Log_Norm_Mw <- function(ret_p){
  #Get the Directories in variables
  setwd('C:/Users/Peter/Documents/Earthquake_v4/Earthquake_v4/ALL_FUNCTIONS')
  Olddir <- getwd()
  Newdir <- paste(getwd(),'/FL_AMAX',sep = '')
  list.files(path=Newdir)
  #Create a dataframe to store the variables
  vec <- vector(mode = 'numeric',length = length(list.files(path=Newdir)))
  Fault_Spec_Log_Norm_Mw <- data.frame(vec,vec)
  colnames(Fault_Spec_Log_Norm_Mw) <- c('meanlog','sdlog')
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


      #Perform Max_Likelihood lnorm_Mw Estimation on Values
      source('02_03_Get_Distribution_Log_Norm_V3.R')
      MLE_values <- fit_distribution(AMAX)
      if (no_FL3 == 54){
        AMAX <- AMAX.df
        MLE_values2 <- MLE_values
        colnames(AMAX) <- c('AMAX','R')
        AMAX_Data_check2 <- AMAX
        AMAX_Data_check <- data.frame(rep(AMAX$AMAX,1))
        colnames(AMAX_Data_check) <- c('AMAX')
        MLE_values[2,1]
        print(file)
        #length(AMAX_Data_check$AMAX)
        x <- data.frame(rlnorm(length(AMAX_Data_check$AMAX),MLE_values[1,1],MLE_values[2,1]))
        AMAX_Data_check[AMAX_Data_check$AMAX<4,] <- runif(dim(AMAX[AMAX_Data_check$AMAX<4,])[1],0,4)
        AMAX_Data_check = (AMAX_Data_check[AMAX_Data_check$AMAX>4,])
        colnames(x) <- c('values')
        graph <- ggplot(data=AMAX_Data_check, aes(AMAX_Data_check$AMAX)) + geom_histogram(aes(y =..density..),bins = 20,fill = NA,color = 'black')# + geom_density(fill = 'red',col=2,alpha = 0.4)
        #graph <- graph +  #geom_density(aes(values),alpha = 0.4,fill = 'blue',data = x) + theme_bw() + scale_x_continuous(name = 'AMAX (Mw)',limits = c(0,8)) + scale_y_continuous(name = 'Density')
        plotdist(AMAX_Data_check)
        
        
        
        pdf('Worst_FL_Example.pdf')
        plot(graph)
        dev.off()
        
        MLE_values
        values <- MLE_values
        lnorm_Mw = qlnorm((1-1/ret_p),meanlog =values['meanlog','Estimate'],sdlog = values['sdlog','Estimate'])
        print(lnorm_Mw)
        toplot <- data.frame(ret_p,lnorm_Mw)
        pdf('EQ_lnorm_FL_Plot.pdf')
        graph <- ggplot(aes(x=toplot$ret_p,y=toplot$lnorm_Mw,color=toplot$lnorm_Mw),data=toplot,size=0.5 ) + geom_point()
        graph <- graph + scale_color_gradient(low='dark green',high='dark red') + guides(fill=FALSE)+ scale_x_continuous(name = 'Return Period (Years)')+
          scale_y_continuous(name = 'Magnitude (Mw)') + theme_bw()
        print(graph)
        dev.off()
      }
      
      #Assign values of MLE to dataframe
      Fault_Spec_Log_Norm_Mw$meanlog[count] <- MLE_values['meanlog','Estimate']
      Fault_Spec_Log_Norm_Mw$sdlog[count] <-MLE_values['sdlog','Estimate']
      count = count+ 1
    }else{
      #Assign values of MLE to dataframe
      Fault_Spec_Log_Norm_Mw$meanlog[count] <- 0
      Fault_Spec_Log_Norm_Mw$sdlog[count] <- 0
      count = count+ 1
    }
    

  }

  print(length(Fault_Spec_Log_Norm_Mw$meanlog[Fault_Spec_Log_Norm_Mw$meanlog==Fault_Spec_Log_Norm_Mw$meanlog[1]]))
  #reset directory and write data to a csv
  setwd(Olddir)
  write.csv(Fault_Spec_Log_Norm_Mw,file = 'Fault_Spec_Log_Norm.csv')
  return(Fault_Spec_Log_Norm_Mw)
}







get_rp_Mw <- function(Fault_Spec_Log_Norm,ret_p){
  
  #Set up dataframe to hold values
  #Fault_Spec_Log_Norm_Mw <- Fl_Spec_Log_Norm_Mw(ret_p)
  lnorm_Mw <- vector(mode = 'numeric', length = length(Fault_Spec_Log_Norm$sdlog))
  lnorm_Mw <- data.frame(rep(list(lnorm_Mw),length(ret_p)))
  colnames(lnorm_Mw) <- as.character(ret_p)
  max(Fault_Spec_Log_Norm$meanlog)
  fl2 <- Fault_Spec_Log_Norm[Fault_Spec_Log_Norm$sdlog==max(Fault_Spec_Log_Norm$sdlog),]
  qlnorm((1-1/ret_p),meanlog = fl2[1,1],sdlog = fl2[1,2])
  #Find the correct Mw for a given return period CHECK THIS!!
  for (i in 1:length(Fault_Spec_Log_Norm$meanlog)){
    FLlnorm_Mw <- qlnorm((1-1/ret_p),meanlog = Fault_Spec_Log_Norm$meanlog[i],sdlog = Fault_Spec_Log_Norm$sdlog[i])
    lnorm_Mw[i,] <- FLlnorm_Mw
  }
  
  #lnorm_Mw_Mw <- lnorm_Mw_Mw[lnorm_Mw_Mw['10'] > 0,]
  Fault_Spec_Log_Norm <- data.frame(Fault_Spec_Log_Norm,lnorm_Mw)
  return(Fault_Spec_Log_Norm)
}


