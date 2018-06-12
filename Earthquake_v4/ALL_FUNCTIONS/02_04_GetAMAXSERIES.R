
get_AMAX <- function(EQ_Data){
  AMAX <- vector(mode = 'numeric',length = length(c(1900:2006)))
  count <- 1
  for (year in c(1900:2006)){
    if(year %in% EQ_Data$Year){
      AMAX[count] <- max(EQ_Data[EQ_Data$Year == year,]['Mw'])
    }else{
      AMAX[count] <- 0
    }
    count = count + 1
  }
  AMAX <- data.frame(c(1900:2006),AMAX)
  colnames(AMAX) <- c('Year','AMAX')
  return(AMAX)
}
