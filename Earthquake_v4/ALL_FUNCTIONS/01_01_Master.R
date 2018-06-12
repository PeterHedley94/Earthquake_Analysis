#*********************************************************************#
#Master File that calls all other functions


#CHANGE ME!!
#find the correct working directory change to wherever you put the code
#Remember to use a forward slash e.g. 'C:/Users/Peter' not!!!! 'C:\Users\Peter'
setwd('C:/Users/Peter/Desktop/Peter_Hedley_Dissertation/Earthquake_v4')


#Setup
libraries <- c('evd','colorRamps','scales','raster','SDMTools','rgdal','fitdistrplus','epiR','rgeos','ggplot2','maptools','broom','ggmap','plyr','stats4','QRM')
lapply(libraries, require, character.only = TRUE)


workingdir1 = getwd()
workingdir2 = paste(getwd(),'/ALL_FUNCTIONS',sep = '')

#Admin stuff with libraries and allocating a large enough expression size
gpclibPermit()
options(expressions=100000)
setwd(workingdir2)




#**********************************************************************#

#Get the Historical EQ Data, FL Data, and Map from Google Maps API
source('02_01_Get_Data_Ready.R')
EQ_Data <- get_EQ_data()
FL_Data <- get_FL_data()
Map <- get_Map()


#Plot various graphs of events, Fault_Lines and Fl's with Mw assigned
source('02_02_Plot_Events.R')
Plot_EQ_Events(EQ_Data,FL_Data,Map)
Plot_EQ_Events_Mw(EQ_Data,Map)
Plot_EQ_Data(EQ_Data)
Plot_Fault_Lines(FL_Data,Map)
#This function you need to go through manually inside the file
Plot_Fault_Lines_Mw(FL_Data,Map)


#Get the Population Data/ Density
source('02_01_Get_Data_Ready.R')
#This function you also need to go through manually inside the file
Population_Data <- get_pop_Data()

#Plot the Population Data
setwd(workingdir2)
source('02_11_Plot_Pop_Data.R')
#Loading file to save time- processing takes a while
load(file = 'FORTIFIED.R')
plotPop(Population_Data, Map,Pop.sp3.points)

#GET THE AMAX SERIES FOR ALL DATA
source('02_04_GetAMAXSERIES.R')
AMAX <- get_AMAX(EQ_Data)


#Determine the Gamma series for all of Turkey + plot
source('02_03_Get_Distribution_Gamma_V3.R')
MLE_values <- fit_distribution(AMAX)
ret_p <- seq(10,10000,by=10)
plot_Gamma(MLE_values,ret_p)

#Plot all distributions for all of Turkey AMAX series
source('02_15_Experiment_Distr.R')
plot_Experiment1(AMAX,ret_p)

#***********************************************************************#

#Separate Data between Faults

#Already done - see excel files in one of the folders can re-run 
#Match EQ Events to FL's
#source('02_06_Match_EQ_To_Faults.R')
#Matched_Data <- Match_Eq_To_FL(EQ_Data,FL_Data)

#SAVE THE DF AS THERE IS A LOT OF COMPUTING TIME HERE
#save(Matched_Data, file="Matched_EQtoFaultLines.rda")
load("Matched_EQtoFaultLines.rda")

#Plot the Faultline activity  = number of events for each fault
source('02_07_Plot_FL_Activity.R')
plot_FL_Act(Map,FL_Data,Matched_Data)


#**************************************************************#
#Plot EQ events with their Mw
source('02_02_Plot_Events.R')
Plot_EQ_Events_Mw(EQ_Data,Map)



setwd(workingdir2)
#Now individual FL AMAX series
source('02_08_Export_Fault_Specific_Data.R')
Export_Fl_Spec_Data(EQ_Data,FL_Data)

#Get dataframe of combined AMAX series - sum of every AMAX for every fault in Turkey
#not the AMAX for all of Turkey - many more values!!
source('02_09_Fault_Specific_Log_Norm_COMBFL_v1.R')
AMAX.df <- Fl_Spec_Log_Norm()

#Determine the Log-Normal series combined data
source('02_03_Get_Distribution_V3_FL_COMB.R')
source('03_03_Distributions.R')
MLE_values2 <- fit_distribution(AMAX.df)



#************************************************************************
#Now experiment with the AMAx data one value at 7.1 every other at <4Mw
ret_p <- c(10,100,500,1000,10000)
source('02_16_Experiment_Distr2.R')
plot_Experiment2(ret_p)



#Plot / Calculate for both Gamma and Log_Norm Distributions
for (Distribution in c('Gamma','Log_Norm')){
  
  
  #Calculate Return Periods for each Distribution from AMAX fl data (censored)
  if (Distribution == 'Gamma'){
    print('Gamma Distribution')
    source('02_09_Fault_Specific_Gamma.R')
    Fault_Spec_Gamma <- Fl_Spec_Gamma_Mw(ret_p)
    fl2 <- Fault_Spec_Gamma
    #Obtain return period Mw for certain values
    Fault_Spec_Gamma <- get_rp_Mw(Fault_Spec_Gamma,ret_p)
    Return_Period_Mw<- Fault_Spec_Gamma
  }
  
  if (Distribution == 'Log_Norm'){
    print('Log_Norm Distribution')
    source('02_09_Fault_Specific_Log_Norm.R')
    Fault_Spec_Log_Norm <- Fl_Spec_Log_Norm_Mw(ret_p)
    fl2 <- Fault_Spec_Log_Norm
    #Obtain return period Mw for certain values
    Fault_Spec_Log_Norm <- get_rp_Mw(Fault_Spec_Log_Norm,ret_p)
    Return_Period_Mw<- Fault_Spec_Log_Norm
  }
  
  #Sort out admin stuff - directories etc.
  setwd(workingdir2)
  Newdir <- paste(getwd(),'/',Distribution,sep = '')
  dir.create(Newdir)
  Newdir <- paste(getwd(),'/',Distribution,'/FL_DISS_RASTER',sep = '')
  dir.create(Newdir)
  Newdir <- paste(getwd(),'/',Distribution,'/PLOTS',sep = '')
  dir.create(Newdir)
  
  #Change the Values that are uncertain and over 8.8 back to the 8.8 value
  Return_Period_Mw$X500[Return_Period_Mw$X500>8.8] <- 8.8
  Return_Period_Mw$X1000[Return_Period_Mw$X1000>8.8] <- 8.8
  Return_Period_Mw$X10000[Return_Period_Mw$X10000>8.8] <- 8.8
  
  #Plot the Faultline Mw
  source('02_10_Plot_FL_Mw.R')
  setwd(workingdir2)
  plot_FL_Mw(Map,FL_Data,Return_Period_Mw,Return_Period_Mw,Distribution)
  
  #Save data if need to
  #save.image(file="workspace2.RData")
  #load("workspace2.RData")
  
  setwd(workingdir2)
  
  #Dissipate the fault lines with different power coefficients - sensitivity of hazard map
  for (Dissipation_coeff in c(1.2)){
    source('02_12_Setup2Diss.R')
    Dissipate_Fault_Lines(Return_Period_Mw,FL_Data,Population_Data,Dissipation_coeff,Distribution)
  }
  
  #Dissipation creates raster data as excel .csv files - combine all of these into one
  for (Dissipation_coeff in c(1.2)){
    setwd(workingdir2)
    source('02_13_Combine_Raster.R')
    Combine_FL_Diss(Population_Data,Dissipation_coeff,Distribution)
  }
  
  #Plot the Hazard Map.
  for (Dissipation_coeff in c(1.2)){
    setwd(workingdir2)
    source('02_13_Plot_Mw_Raster.R')
    Combined_FL_Data <- plotMw_Raster2(Population_Data, Map,Dissipation_coeff,Distribution)
  }
  
  #Using a dissipation coeff of 1.2 conduct risk sensitivity
  for (Dissipation_coeff in c(1.2)){
    Dissipation_coeff = 1.2
    
    # Vary population to the power of...
    for (Risk_Coeff in c(1,2,5,10)){
      Risk_Coeff = 5
      setwd(workingdir2)
      source('02_14_Risk_Map.R')
      Create_Risk_CSV(Population_Data,Dissipation_coeff,Distribution,Risk_Coeff)
      plot_Risk(Population_Data, Map,Dissipation_coeff,Distribution,Risk_Coeff)
    }
  }
}





