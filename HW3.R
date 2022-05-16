

  library(ggplot2)
  library(data.table)
  library(plotly)
  library(TSstudio)
  
  
 #read data as a table 
  x_train <- read.table("/Users/okansayar/Desktop/R/RStudio Directory/UWave/uWaveGestureLibrary_X_TRAIN")
  y_train <- read.table("/Users/okansayar/Desktop/R/RStudio Directory/UWave/uWaveGestureLibrary_Y_TRAIN")
  z_train <- read.table("/Users/okansayar/Desktop/R/RStudio Directory/UWave/uWaveGestureLibrary_Z_TRAIN")
  ecg <- read.table("/Users/okansayar/Desktop/R/RStudio Directory/UWave/ecgTrain")
  
  #combine these tables in a three different col. 
  mdata <- data.frame(c1 = x_train,  
                      c2 = y_train,
                      c3=  z_train)
  #see the time series
  ##mdata
  ts.plot(mdata)
  
  #in a three different dimension vector
  x=x_train$V2
  y=y_train$V2
  z=z_train$V2
  
  #plot result
  p=plot_ly(ecg,x=~x,y=~y,z=~z)
  p
   
  #Part b
  
  library(readxl)

    
  #read & check data
  ildist = read_xlsx("/Users/okansayar/Desktop/R/RStudio Directory/ilmesafe.xlsx")
  ildist
  str(ildist)
  
  # remove first row storing the names
  ildist1=ildist[, -1]
  ildist1

  
  ildist2=ildist1[, -1]
  str(ildist2)
  ildist2
  
  
  # apply multidimensional scaling
  mds_fit=cmdscale(ildist2, k = 2, )
  
  # getting coordinates
  x=mds_fit[, 1]
  y=mds_fit[, 2]
  
  # plotting
  plot(x, y, pch = 19)
  ildist
  text(x, y, pos = 4, labels =colnames(ildist2))

    
    