library(envPred)
library(Hmisc)
library(tidyr)
library(gplots)
library(ggplot2)
library(RColorBrewer)
library(cmocean)
# for a readmat function that works with HDF5
library(raveio)

filepath <- ("./data/")
filename <- (gsub(" ","",paste(filepath,"timeAllCNAPS.mat")))
timeAux  <- read_mat(filename)

# Format the date-time object to "yyyy-mm-dd" format
datetime <- as.POSIXct("1858-11-17", tz = "UTC") + timeAux$timeAll*86400
datetime <-  as.Date(datetime)

# Extract the month and quarter using the months() and quarters() functions
monthT <- months(datetime)
quarterT <- quarters(datetime)

# get lat and lon
filename <- (gsub(" ","",paste(filepath,"latlon_CNAPS.mat")))
latlon   <- read_mat(filename)
latlon   <- latlon$latlon_CNAPS

latrange <- seq(29,34)
# Initialize an empty data frame to store data for plotting
plot_data <- data.frame()

# load matfile
for (ivar in 1:5) {
    varname <- switch(ivar,
                    'ssh',
                    'bottomT',
                    'sst',
                    'Salinity',
                    'mixedlayer')
  # work the variable
  # loop all sites and do 1) envPred, 2) spectral analyses for entire time (breakdown in frequencies), 3) trend
  filename <- (gsub(" ","",paste(filepath,varname,"_CNAPS.mat")))
  VarMaux  <- read_mat(filename)
  VarM     <- VarMaux$varM
  rm(VarMaux)
  
  #datePos <-datetime[datetime <= as.Date("2021-01-01")]
  
  for (ilat in latrange) {
    # Find the closest latitude in your latlon data
    closest_lat <- latlon[which.min(abs(latlon[,1] - ilat)),1]
    LatIndex <- which(latlon[,1]==closest_lat)
    
    # Filter VarM data for the closest latitude
    VarMProfile <- VarM[, LatIndex]
    
    minP <- min(VarMProfile)
    maxP <- max(VarMProfile)
    sizeP <- (maxP - minP)/8
  
    # Create a contour plot with adjusted range and color scale
    fig <- plot_ly(x = latlon[LatIndex,2],
                   y = datetime,
                   z = ~VarMProfile, type = "contour",
                   contours = list(start = minP, end = maxP, size = sizeP),
                   colorscale = "Jet", width = 450, height = 1200)
    reticulate::py_run_string("import sys")
    save_image(fig, file = gsub(" ","",paste("./images/hovmoller_",varname,"_",ilat,".png")))
    
    # PLOT SEASONS ONLY 
    for (iQ in 1:4) {
      qnumber <- switch(iQ,
                        'Q1',
                        'Q2',
                        'Q3',
                        'Q4')
      qname <- switch(iQ,
                      'Winter',
                      'Spring',
                      'Summer',
                      'Fall')
      QuartIndex <- which(quarterT == qnumber)
      datePos <-datetime[QuartIndex]
      # Filter VarM data for the closest latitude
      VarMProfile <- VarM[QuartIndex, LatIndex]
      
      # Create a contour plot with adjusted range and color scale
      minP <- min(VarMProfile)
      maxP <- max(VarMProfile)
      sizeP <- (maxP - minP)/8
      
      fig <- plot_ly(x = latlon[LatIndex,2],
                     y = datePos,
                     z = ~VarMProfile, type = "contour",
                     contours = list(start = minP, end = maxP, size = sizeP),
                     colorscale = "Jet", width = 450, height = 1200)
      reticulate::py_run_string("import sys")
      save_image(fig, file = gsub(" ","",paste("./images/hovmoller_",varname,"_",ilat,"_",qname,".png")))
    }
  }
}

# make PLOTS for month and date for month 
start_date <- as.Date(min(datetime))
end_date   <- as.Date(max(datetime))
dateMonth <- seq.Date(from = start_date, to = end_date, by = "month")

# load matfile
for (ivar in 1:5) {
  varname <- switch(ivar,
                    'ssh',
                    'bottomT',
                    'sst',
                    'Salinity',
                    'mixedlayer')
  # work the variable
  # loop all sites and do 1) envPred, 2) spectral analyses for entire time (breakdown in frequencies), 3) trend
  filename <- (gsub(" ","",paste(filepath,varname,"_CNAPS_Monthly.mat")))
  VarMaux  <- read_mat(filename)
  VarM     <- VarMaux$varM
  rm(VarMaux)
  
  for (ilat in latrange) {
    # Find the closest latitude in your latlon data
    closest_lat <- latlon[which.min(abs(latlon[,1] - ilat)),1]
    LatIndex <- which(latlon[,1]==closest_lat)
    
    # Filter VarM data for the closest latitude
    VarMProfile <- VarM[, LatIndex]
    
    minP <- min(VarMProfile)
    maxP <- max(VarMProfile)
    sizeP <- (maxP - minP)/8
    
    # Create a contour plot with adjusted range and color scale
    fig <- plot_ly(x = latlon[LatIndex,2],
                   y = dateMonth,
                   z = ~VarMProfile, type = "contour",
                   contours = list(start = minP, end = maxP, size = sizeP),
                   colorscale = "Jet", width = 450, height = 1200)
    reticulate::py_run_string("import sys")
    save_image(fig, file = gsub(" ","",paste("./images/hovmoller_monthly_",varname,"_",ilat,".png")))
  } 
}