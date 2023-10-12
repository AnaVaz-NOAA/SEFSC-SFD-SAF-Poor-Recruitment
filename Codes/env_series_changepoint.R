library(envPred)
library(Hmisc)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(cmocean)
# for a readmat function that works with HDF5
library(raveio)
library(changepoint)
library(strucchange)

#filepath <- ("/Users/anavaz/Stuff/Current/SAtlantic/CNAPS_mat/")
filepath <- ("/Users/anacarolvaz/Stuff/current_work/SAtlantic/CNAPS_mat/")
filename <- (gsub(" ","",paste(filepath,"timeAllCNAPS.mat")))
timeAux  <- read_mat(filename)

# Format the date-time object to "yyyy-mm-dd" format
datetime <- as.POSIXct("1858-11-17", tz = "UTC") + timeAux$timeAll*86400
datetime <- as.Date(datetime)
datetimeF <- format(datetime, "%Y-%m-%d")
# to use in the plot
datetime_numeric <- as.numeric(datetime)

# Extract the month and quarter using the months() and quarters() functions
monthT   <- months(datetime)
quarterT <- quarters(datetime)

# make for month 
start_date <- as.Date("1993-01-01")
end_date   <- as.Date("2021-12-01")
dateMonth  <- seq.Date(from = start_date, to = end_date, by = "month")
dateMonth_numeric <- as.numeric(dateMonth)

# get lat and lon
filename <- (gsub(" ","",paste(filepath,"latlon_CNAPS.mat")))
latlon   <- read_mat(filename)
latlon   <- latlon$latlon_CNAPS

for (ivar in 1:3) {
  # create new matrices
  breaks_matrixMV <- matrix(nrow = 0, ncol = 4)
  colnames(breaks_matrixMV) <- c("Lat","Lon", "Date","Breaks")
  breaks_matrixM <- matrix(nrow = 0, ncol = 4)
  colnames(breaks_matrixM) <- c("Lat","Lon", "Date", "Breaks")
  breaks_matrixV <- matrix(nrow = 0, ncol = 4)
  colnames(breaks_matrixV) <- c("Lat","Lon", "Date", "Breaks")
  
  # load the mat file
  varname <- switch(ivar,
                    'bottomT',
                    'sst',
                    'mixedlayer')
  filename <- (gsub(" ","",paste(filepath,varname,"_CNAPS.mat")))
  VarMaux  <- read_mat(filename)
  VarM     <- VarMaux$varM
  rm(VarMaux)
  
  for (i in seq_len(ncol(VarM))) {
    varPos <- VarM[,i]
    cpaux  <- cpt.meanvar(varPos)
    for (cpt in cpaux@cpts) {
      if (cpt < 10500){      
        breaks_matrixMV <- rbind(breaks_matrixMV, c(latlon[i,1], latlon[i,2], datetimeF[cpt], cpt))
      }
    }
    cpaux  <- cpt.mean(varPos)
    for (cpt in cpaux@cpts) {
      if (cpt < 10500){      
        breaks_matrixM <- rbind(breaks_matrixM, c(latlon[i,1], latlon[i,2] , datetimeF[cpt], cpt))
      }
    }
    cpaux  <- cpt.var(varPos)
    for (cpt in cpaux@cpts) {
      if (cpt < 10500){      
        breaks_matrixV <- rbind(breaks_matrixV, c(latlon[i,1], latlon[i,2] , datetimeF[cpt], cpt))
      }
    } 
  }  # end of loop positions
  write.csv(breaks_matrixMV, file = gsub(" ","",paste("./csv_files/",varname,"_breaks_space_VarMean.csv")), row.names = FALSE)
  write.csv(breaks_matrixM,  file = gsub(" ","",paste("./csv_files/",varname,"_breaks_space_Mean.csv")), row.names = FALSE)
  write.csv(breaks_matrixV,  file = gsub(" ","",paste("./csv_files/",varname,"_breaks_space_Var.csv")), row.names = FALSE)
}
# this matrix is outside because it is looping though it
breaks_matrix <- matrix(nrow = 0, ncol = 4)
colnames(breaks_matrix) <- c("Variable", "Method", "Dates", "Breaks")

# For mean series all regions
# load matfile
for (ivar in 1:3) {
  varname <- switch(ivar,
                    'bottomT',
                    'sst',
                    'mixedlayer')
  varprint <- switch(ivar,
                    'Bottom Temperature',
                    'SST',
                    'Mixed Layer Depth')
  filename <- (gsub(" ","",paste(filepath,varname,"_CNAPS.mat")))
  VarMaux  <- read_mat(filename)
  VarM     <- VarMaux$varM
  VarAvg  <- rowMeans(VarM)
  limitU = max(abs(VarAvg),na.rm = TRUE)
  limitL = min(abs(VarAvg),na.rm = TRUE)
  cpaux  <- cpt.meanvar(VarAvg)
  
  for (cpt in cpaux@cpts) {
    if (cpt < 10500){      
      breaks_matrix <- rbind(breaks_matrix, c(varname ,'MeanVar' , datetimeF[cpt], cpt))
    }
  }
  
  png(gsub(" ","",paste("./images/ChangePoint/",varname,"_changepoint_MeanVar.png")), width = 2000, height = 1000, res = 300)
  par(mfrow = c(1,1), mar=c(0.5, 0.5, 0.5, 0.5), oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(datetime, rep(0,length(datetime)), ylim=c(limitL, limitU), type="n", ylab="", xlab="", main = c("Change Point (VarMean)",varprint))
  axis(1, at = seq(min(datetime), max(datetime), by = 365), labels = NA, tck = -0.02)
  lines(datetime, VarAvg,  lty=1, col=4, lwd=2)
  abline(v = datetime[cpaux@cpts], col = "lightgray", lwd = 5)
  
  for (i in 1:(length(cpaux@cpts)+1)) {
    print(i)
    if (i == 1) {
      start <- 1
      end   <- cpaux@cpts[i]
    } else if (i == length(cpaux@cpts)+1) {
      start <- cpaux@cpts[i-1] + 1 #cpaux@cpts[length(cpaux@cpts)] + 1
      end   <- length(VarAvg)
    } else {
      start <- cpaux@cpts[i-1] + 1
      end <- cpaux@cpts[i]
    }
    if (start > length(VarAvg)){
      start <- length(VarAvg)
    }
    print(start)
    print(end)
    
    # Calculate the linear regression coefficients for the segment
    model <- lm(VarAvg[start:end] ~ datetime_numeric[start:end])
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    segments(datetime_numeric[start], intercept + slope*datetime_numeric[start], 
             datetime_numeric[end], intercept + slope*datetime_numeric[end], col = "red", lwd = 2, lty = "dashed")
  }
  dev.off()
    
  cpaux  <- cpt.mean(VarAvg)
  for (cpt in cpaux@cpts) {
    if (cpt < 10500){      
      breaks_matrix <- rbind(breaks_matrix, c(varname ,'Mean' , datetimeF[cpt], cpt))
    }
  }
  png(gsub(" ","",paste("./images/ChangePoint/",varname,"_changepoint_Mean.png")), width = 2000, height = 1000, res = 300)
  par(mfrow = c(1,1), mar=c(0.5, 0.5, 0.5, 0.5), oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(datetime, rep(0,length(datetime)), ylim=c(limitL, limitU), type="n", ylab="", xlab="", main = c("Change Point (Mean)",varprint))
  axis(1, at = seq(min(datetime), max(datetime), by = 365), labels = NA, tck = -0.02)
  lines(datetime, VarAvg,  lty=1, col=4, lwd=2)
  abline(v = datetime[cpaux@cpts], col = "lightgray", lwd = 5)
  
  for (i in 1:(length(cpaux@cpts)+1)) {
    print(i)
    if (i == 1) {
      start <- 1
      end   <- cpaux@cpts[i]
    } else if (i == length(cpaux@cpts)+1) {
      start <- cpaux@cpts[i-1] + 1 #cpaux@cpts[length(cpaux@cpts)] + 1
      end   <- length(VarAvg)
    } else {
      start <- cpaux@cpts[i-1] + 1
      end <- cpaux@cpts[i]
    }
    if (start > length(VarAvg)){
      start <- length(VarAvg)
    }
    print(start)
    print(end)
    
    # Calculate the linear regression coefficients for the segment
    model <- lm(VarAvg[start:end] ~ datetime_numeric[start:end])
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    segments(datetime_numeric[start], intercept + slope*datetime_numeric[start], 
             datetime_numeric[end], intercept + slope*datetime_numeric[end], col = "red", lwd = 2, lty = "dashed")
  }
  dev.off()
  
  cpaux  <- cpt.var(VarAvg)
  for (cpt in cpaux@cpts) {
    if (cpt < 10500){      
      breaks_matrix <- rbind(breaks_matrix, c(varname ,'Var' , datetimeF[cpt], cpt))
    }
  }
  png(gsub(" ","",paste("./images/ChangePoint/",varname,"_changepoint_Var.png")), width = 2000, height = 1000, res = 300)
  par(mfrow = c(1,1), mar=c(0.5, 0.5, 0.5, 0.5), oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(datetime, rep(0,length(datetime)), ylim=c(limitL, limitU), type="n", ylab="", xlab="", main = c("Change Point (Var)",varprint))
  axis(1, at = seq(min(datetime), max(datetime), by = 365), labels = NA, tck = -0.02)
  lines(datetime, VarAvg,  lty=1, col=4, lwd=2)
  abline(v = datetime[cpaux@cpts], col = "lightgray", lwd = 5)
  
  for (i in 1:(length(cpaux@cpts)+1)) {
    print(i)
    if (i == 1) {
      start <- 1
      end   <- cpaux@cpts[i]
    } else if (i == length(cpaux@cpts)+1) {
      start <- cpaux@cpts[i-1] + 1 #cpaux@cpts[length(cpaux@cpts)] + 1
      end   <- length(VarAvg)
    } else {
      start <- cpaux@cpts[i-1] + 1
      end <- cpaux@cpts[i]
    }
    if (start > length(VarAvg)){
      start <- length(VarAvg)
    }
    print(start)
    print(end)
    
    # Calculate the linear regression coefficients for the segment
    model <- lm(VarAvg[start:end] ~ datetime_numeric[start:end])
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    segments(datetime_numeric[start], intercept + slope*datetime_numeric[start], 
             datetime_numeric[end], intercept + slope*datetime_numeric[end], col = "red", lwd = 2, lty = "dashed")
  }
  dev.off()
}
write.csv(breaks_matrix, file = gsub(" ","",paste("./csv_files/breaks_daily_avg.csv")), row.names = FALSE)


# this matrix is outside because it is looping though it

breaks_matrix <- matrix(nrow = 0, ncol = 4)
colnames(breaks_matrix) <- c("Variable", "Method", "Dates", "Breaks")

# For mean series all regionscd 
# load matfile
for (ivar in 1:3) {
  varname <- switch(ivar,
                    'bottomT',
                    'sst',
                    'mixedlayer')
  varprint <- switch(ivar,
                     'Bottom Temperature',
                     'SST',
                     'Mixed Layer Depth')
  filename <- (gsub(" ","",paste(filepath,varname,"_CNAPS_Monthly.mat")))
  VarMaux  <- read_mat(filename)
  VarM     <- VarMaux$varM
  VarAvg  <- rowMeans(VarM)
  limitU = max(abs(VarAvg),na.rm = TRUE)
  limitL = min(abs(VarAvg),na.rm = TRUE)
  cpaux  <- cpt.meanvar(VarAvg)
  for (cpt in cpaux@cpts) {
    if (cpt < 10500){      
      breaks_matrix <- rbind(breaks_matrix, c(varname ,'MeanVar' , dateMonth[cpt], cpt))
    }
  }
  
  png(gsub(" ","",paste("./images/ChangePoint/",varname,"_changepoint_MeanVar_Monthly.png")), width = 2000, height = 1000, res = 300)
  par(mfrow = c(1,1), mar=c(0.5, 0.5, 0.5, 0.5), oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(dateMonth, rep(0,length(dateMonth)), ylim=c(limitL, limitU), type="n", ylab="", xlab="", main = c("Change Point (VarMean)",varprint))
  axis(1, at = seq(min(dateMonth), max(dateMonth), by = 1), labels = NA, tck = -0.02)
  lines(dateMonth, VarAvg,  lty=1, col=4, lwd=2)
  abline(v = dateMonth[cpaux@cpts], col = "lightgray", lwd = 5)
  
  for (i in 1:(length(cpaux@cpts)+1)) {
    print(i)
    if (i == 1) {
      start <- 1
      end   <- cpaux@cpts[i]
    } else if (i == length(cpaux@cpts)+1) {
      start <- cpaux@cpts[i-1] + 1 #cpaux@cpts[length(cpaux@cpts)] + 1
      end   <- length(VarAvg)
    } else {
      start <- cpaux@cpts[i-1] + 1
      end <- cpaux@cpts[i]
    }
    if (start > length(VarAvg)){
      start <- length(VarAvg)
    }
    print(start)
    print(end)
    
    # Calculate the linear regression coefficients for the segment
    model <- lm(VarAvg[start:end] ~ dateMonth_numeric[start:end])
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    segments(dateMonth_numeric[start], intercept + slope*dateMonth_numeric[start], 
             dateMonth_numeric[end], intercept + slope*dateMonth_numeric[end], col = "red", lwd = 2, lty = "dashed")
  }
  dev.off()
  
  cpaux  <- cpt.mean(VarAvg)
  for (cpt in cpaux@cpts) {
    if (cpt < 10500){      
      breaks_matrix <- rbind(breaks_matrix, c(varname ,'Mean' , dateMonth[cpt], cpt))
    }
  }
  png(gsub(" ","",paste("./images/ChangePoint/",varname,"_changepoint_Mean_Monthly.png")), width = 2000, height = 1000, res = 300)
  par(mfrow = c(1,1), mar=c(0.5, 0.5, 0.5, 0.5), oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(dateMonth, rep(0,length(dateMonth)), ylim=c(limitL, limitU), type="n", ylab="", xlab="", main = c("Change Point (Mean)",varprint))
  axis(1, at = seq(min(datetime), max(datetime), by = 365), labels = NA, tck = -0.02)
  lines(dateMonth, VarAvg,  lty=1, col=4, lwd=2)
  abline(v = dateMonth[cpaux@cpts], col = "lightgray", lwd = 5)
  
  for (i in 1:(length(cpaux@cpts)+1)) {
    print(i)
    if (i == 1) {
      start <- 1
      end   <- cpaux@cpts[i]
    } else if (i == length(cpaux@cpts)+1) {
      start <- cpaux@cpts[i-1] + 1 #cpaux@cpts[length(cpaux@cpts)] + 1
      end   <- length(VarAvg)
    } else {
      start <- cpaux@cpts[i-1] + 1
      end <- cpaux@cpts[i]
    }
    if (start > length(VarAvg)){
      start <- length(VarAvg)
    }
    print(start)
    print(end)
    
    # Calculate the linear regression coefficients for the segment
    model <- lm(VarAvg[start:end] ~ dateMonth_numeric[start:end])
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    segments(dateMonth_numeric[start], intercept + slope*dateMonth_numeric[start], 
             dateMonth_numeric[end], intercept + slope*dateMonth_numeric[end], col = "red", lwd = 2, lty = "dashed")
  }
  dev.off()
  
  cpaux  <- cpt.var(VarAvg)
  for (cpt in cpaux@cpts) {
    if (cpt < 10500){      
      breaks_matrix <- rbind(breaks_matrix, c(varname ,'Var' , dateMonth[cpt], cpt))
    }
  }
  png(gsub(" ","",paste("./images/ChangePoint/",varname,"_changepoint_Var_Month.png")), width = 2000, height = 1000, res = 300)
  par(mfrow = c(1,1), mar=c(0.5, 0.5, 0.5, 0.5), oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(dateMonth, rep(0,length(dateMonth)), ylim=c(limitL, limitU), type="n", ylab="", xlab="", main = c("Change Point (Var)",varprint))
  axis(1, at = seq(min(dateMonth), max(dateMonth), by = 1), labels = NA, tck = -0.02)
  lines(dateMonth, VarAvg,  lty=1, col=4, lwd=2)
  abline(v = dateMonth[cpaux@cpts], col = "lightgray", lwd = 5)
  
  for (i in 1:(length(cpaux@cpts)+1)) {
    print(i)
    if (i == 1) {
      start <- 1
      end   <- cpaux@cpts[i]
    } else if (i == length(cpaux@cpts)+1) {
      start <- cpaux@cpts[i-1] + 1 #cpaux@cpts[length(cpaux@cpts)] + 1
      end   <- length(VarAvg)
    } else {
      start <- cpaux@cpts[i-1] + 1
      end <- cpaux@cpts[i]
    }
    if (start > length(VarAvg)){
      start <- length(VarAvg)
    }
    print(start)
    print(end)
    
    # Calculate the linear regression coefficients for the segment
    model <- lm(VarAvg[start:end] ~ dateMonth_numeric[start:end])
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    segments(dateMonth_numeric[start], intercept + slope*dateMonth_numeric[start], 
             dateMonth_numeric[end], intercept + slope*dateMonth_numeric[end], col = "red", lwd = 2, lty = "dashed")
  }
  dev.off()
}
write.csv(breaks_matrix, file = gsub(" ","",paste("./csv_files/breaks_month_avg.csv")), row.names = FALSE)