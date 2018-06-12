#AIM of this script is to dissipate a Mw from a line
#basic process is to define a circle of decreasing radius
#Move the line with respect to the circle around the original line
#This allows the formation of a line dissipating


create_matrix <- function(longdim,latdim,stepdist){
  #create a matrix with the given data
  lat_array = seq(latdim[1],latdim[2], by = stepdist)
  long_array = seq(longdim[1],longdim[2], by = stepdist)
  totalnumber = length(long_array)*length(lat_array)
  matrix1 = matrix(data = c(rep(0,totalnumber)),ncol = length(long_array), byrow = FALSE)
  colnames(matrix1) <- long_array
  rownames(matrix1) <- lat_array
  return(matrix1)
}

create_line <- function(matrix1,Mw,templong,templat,coordsx,coordsy){
  #for the line situation use all points to offset

  for (i2 in 1:length(coordsx)){
    
    #calculate the coordinates of the matrix
    long2 <- round(as.numeric(coordsx[i2])+as.numeric(templong))
    lat2 <- round(as.numeric(coordsy[i2])+as.numeric(templat))
  
    if(is.element(long2,as.numeric(colnames(matrix1))) & is.element(lat2,as.numeric(rownames(matrix1)))){
      matrix1[toString(lat2),toString(long2)] = Mw

    }else{
      #print('out of bounds')
      
    }
  }
  return(matrix1)
}


create_ends <- function(matrix1,Mw,templong,templat,coordsx,coordsy){

  
  #for ends only need first and last point of line
  for (i in 1:length(templong)){
    
    for (i2 in c(1,length(coordsx))){

      long2 <- round(as.numeric(coordsx[i2])+as.numeric(templong[i]))
      lat2 <- round(as.numeric(coordsy[i2])+as.numeric(templat[i]))

      #Assign Mw to the calculated coordinates in the matrix
      if(is.element(long2,as.numeric(colnames(matrix1))) & is.element(lat2,as.numeric(rownames(matrix1)))){
        matrix1[toString(lat2),toString(long2)] = Mw

      }else{
        #print('out of bounds')
        
      }
    }
  }
  plot(raster(matrix1))
  return(matrix1)
}


create.circle4 <- function(matrix1,Mw,distance,coordsx,coordsy){
  #R <- distance
  #treating like polar coordinates we need an angle and a distance
  R <- distance
  #to speed up the algorithm find the angle of the line
  deltax <- coordsx[length(coordsx)] - coordsx[1]
  deltay <- coordsy[length(coordsy)]- coordsy[1]
  angle_line <- atan(deltay/deltax)
  
  #find the perpendicular angles to the line
  angle_lineperp1 <- (angle_line + pi/2) %% (2*pi)
  angle_lineperp2 <- (angle_line-pi/2) %% (2*pi)

  precision = 32
  #Using an excel file to understand a central cell is surrounded by 8 then 16,24 etc. as you go out a number of layers
  templat <- vector(mode='numeric',length=precision*R)
  templong <- vector(mode='numeric',length=precision*R)
  for (i in 1:(precision*distance)){
    
    #split the angle so it matches the cells of the matrix
    angle_templong <- i*(2*pi-2*pi/(precision*R))/(precision*R)
    #determine the cell locations
    templong[i] <- round_any(R*cos(angle_templong),1)
    templat[i] <- round_any(R*sin(angle_templong),1)
    matrix1 <- create_line(matrix1=matrix1,Mw = Mw,templong = templong[i],templat = templat[i],coordsx = coordsx,coordsy=coordsy)
    #Proper way of doing it here - ignored for this run
    #if the angle is close to the perp angle of line offset the line by the given distance
    # if(abs((angle_templong - angle_lineperp1 + pi + 2*pi) %% (2*pi) - pi)< pi/(2*R)){
    #   matrix1 <- create_line(matrix1=matrix1,Mw = Mw,templong = templong[i],templat = templat[i],coordsx = coordsx,coordsy=coordsy)
    # 
    #   }else if(abs((angle_templong - angle_lineperp2 + pi + 2*pi) %% (2*pi) - pi) < pi/(2*R)){
    #   matrix1 <- create_line(matrix1=matrix1,Mw = Mw,templong = templong[i],templat = templat[i],coordsx = coordsx,coordsy=coordsy)
    # }

  }
  #now use the two end points of the line to create the end dissipation
  matrix1 <- create_ends(matrix1=matrix1,Mw = Mw,templong = templong,templat = templat,coordsx = coordsx,coordsy=coordsy)
  return(matrix1)
}
dissipate <- function(matrix1,Mw,coordsx,coordsy,longdim,latdim,degpersqr,Dissipation_coeff){
  #As a 4 on magnitude is not very significant dissipate until 4 is reached
  Mw_dis <- Mw
  
  #We know the number of degrees per sqr of matrix so modify the distance to take account of dissipation
   #1 degree equals c.110km
   #******************************************
  dist = ( (10^( 1.5*(Mw_dis - 4) ) )/(2*pi))^(1/(2*Dissipation_coeff))

  distance <- round_any(dist/degpersqr/110,1)
  print('distance')
  print(distance)
  print(Mw_dis)

  #*****************************************
  diss_per_sqr <- degpersqr

  if(distance >0 ){
    for (i in seq(1,distance,by=1)){
      #********************************
      kmdist = (distance - i)*degpersqr * 110
      
      
      #Changed here as well!!!
      #tempMw = Mw - log10(2 * pi * kmdist^2.4)/1.5
      tempMw = Mw - log10(2 * pi * kmdist^(2*Dissipation_coeff))/1.5

      
      if(!is.finite(tempMw)){
        tempMw = Mw
      }
      #********************************
      tempdist = 1+distance - i
      #print(tempMw)
      #write to the matrix using this Mw and distance
      matrix1 <- create.circle4(matrix1,tempMw,tempdist,coordsx,coordsy)
    }
  }
  print(Mw)
  matrix1 <- create_line(matrix1=matrix1,Mw = Mw,templong = 0,templat = 0,coordsx = coordsx,coordsy=coordsy)
  return(matrix1)
}


dissipate_FL_Mw <- function(Population_Data,coordsx,coordsy,Mw,Dissipation_coeff){
  
  #Mws here are replicated but come from a raster
  longdim = c(Population_Data@bbox[1,1],Population_Data@bbox[1,2])
  latdim = c(Population_Data@bbox[2,1],Population_Data@bbox[2,2])
  #Population_Data@bbox
  print(longdim)
  print(latdim[1])
  
  degpersqr = Population_Data@grid@cellsize[2]
  #Transfer the original coordinates into easier to use integers
  norows <- (latdim[2] - latdim[1])/degpersqr
  nocols <- (longdim[2] - longdim[1])/degpersqr
  
  #ADJUST COORDS TO BE MORE SENSITIVE AND COVER EVERY BIT OF MATRIX THE LINE IS ON
  coordsxtemp <- c()
  coordsytemp <- c()
  
  for (i in 1:(length(coordsx)-1)){
    deltax1 <- abs(coordsx[i+1]-coordsx[i])
    deltay1 <- abs(coordsy[i+1]-coordsy[i])
    if (deltax1 > deltay1){
      if(degpersqr >abs(coordsy[i]-coordsy[i+1])){
        len <- floor(abs(coordsx[i]-coordsx[i+1])/degpersqr) + 1
        tempy <- rep(coordsy[i],length = len)
      }else if(coordsy[i]>coordsy[i+1]){
        tempy <- seq(coordsy[i],coordsy[i+1],by = -degpersqr*deltay1/deltax1)
      }else{
        tempy <- seq(coordsy[i],coordsy[i+1],by = degpersqr*deltay1/deltax1)
      }
      if(coordsx[i]>coordsx[i+1]){
        tempx <- seq(coordsx[i],coordsx[i+1],-degpersqr)
      }else{
        tempx <- seq(coordsx[i],coordsx[i+1],degpersqr)
      }

    }else{
      #print(coordsx[i])
      #print(coordsx[i+1])
      
      if(coordsy[i]>coordsy[i+1]){
        tempy <- seq(coordsy[i],coordsy[i+1],by = -degpersqr)
      }else{
        tempy <- seq(coordsy[i],coordsy[i+1],by = degpersqr)
      }
      
      if(degpersqr >abs(coordsx[i]-coordsx[i+1])){
        len <- floor(abs(coordsy[i]-coordsy[i+1])/degpersqr) + 1
        tempx <- rep(coordsx[i],length = len)
      }else if(coordsx[i]>coordsx[i+1]){
        tempx <- seq(coordsx[i],coordsx[i+1],-degpersqr*deltax1/deltay1)
      }else{
        tempx <- seq(coordsx[i],coordsx[i+1],degpersqr*deltax1/deltay1)
      }

    }
    coordsxtemp <- c(coordsxtemp, tempx)
    coordsytemp <- c(coordsytemp, tempy)
  }
  coordsytemp = coordsytemp[is.finite(coordsxtemp)]
  coordsxtemp = coordsxtemp[is.finite(coordsxtemp)]
  coordsxtemp = coordsxtemp[is.finite(coordsytemp)]
  coordsytemp = coordsytemp[is.finite(coordsytemp)]
  
  #coordsx <- coordsxtemp
  #coordsy <- coordsytemp
  
  coordsx <- round_any((coordsx - longdim[1])/degpersqr,1)
  coordsy <- round_any((latdim[2] - coordsy)/degpersqr,1)
  
  print(max(coordsx))
  print(max(coordsy))

  latdim <- c(1,norows)
  longdim <- c(1,nocols)
  #create a matrix using these Mws
  #Same dimensions as raster but from 1:... row and col names
  matrix1 <- create_matrix(longdim,latdim,1)
  as.matrix(Population_Data)
  length(colnames(matrix1))
  length(rownames(matrix1))

  #Initiate the function to dissipate from the line
  matrix1 <- dissipate(matrix1,Mw,coordsx,coordsy,longdim,latdim,degpersqr,Dissipation_coeff)


  rownames(matrix1) <- seq(Population_Data@bbox[2,1],Population_Data@bbox[2,2]-degpersqr, by = degpersqr)
  colnames(matrix1) <- seq(Population_Data@bbox[1,1],Population_Data@bbox[1,2]-degpersqr, by = degpersqr)
  print(max(matrix1))
  return(matrix1)
}

