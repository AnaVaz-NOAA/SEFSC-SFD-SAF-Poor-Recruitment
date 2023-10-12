library(envPred)
library(Hmisc)
library(tidyr)
library(gplots)
library(ggplot2)
library(RColorBrewer)
library(cmocean)
# for a readmat function that works with HDF5
library(raveio)

#filepath <- ("/Users/anavaz/Stuff/Current/SAtlantic/CNAPS_mat/")
filepath <- ("/Users/anacarolvaz/Stuff/current_work/SAtlantic/CNAPS_mat/")
filename <- (gsub(" ","",paste(filepath,"timeAllCNAPS.mat")))
timeAux  <- read_mat(filename)

# Format the date-time object to "yyyy-mm-dd" format
datetime <- as.POSIXct("1858-11-17", tz = "UTC") + timeAux$timeAll*86400
datetime <-  as.Date(datetime)

# Extract the month and quarter using the months() and quarters() functions
monthT <- months(datetime)
quarterT <- quarters(datetime)

# make for month 
start_date <- as.Date("1993-01-01")
end_date   <- as.Date("2021-12-01")
dateMonth <- seq.Date(from = start_date, to = end_date, by = "month")

# get lat and lon
filename <- (gsub(" ","",paste(filepath,"latlon_CNAPS.mat")))
latlon   <- read_mat(filename)
latlon   <- latlon$latlon_CNAPS

# Constancy (C) measures the extent to which the environment is the same for all months in all years. 
# Contingency (M) measures the extent to which the environmental differences between months are the same in all years. 
# Predictability (P) is the sum of Constancy (C) and Contingency (M). 

col_names <- c("Lat","Lon","Constancy", "Contigency", "Predictability")

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
    datePos <-datetime[datetime <= as.Date("2021-01-01") & quarterT == qnumber]
    
    row_names <- 1:ncol(VarM)
    my_df <- data.frame(matrix(NA, nrow = ncol(VarM), ncol = length(col_names)))
    colnames(my_df) <- col_names
    rownames(my_df) <- row_names
    my_df[, "Lat"] <- latlon[,1]
    my_df[, "Lon"] <- latlon[,2]
    
    for (i in seq_len(ncol(VarM))) {
      varPos  <- VarM[,i]
      varPos  <-  varPos[datetime <= as.Date("2021-01-01") & quarterT == qnumber]
      
      # will try to do colwell directly by seasons - or quarters
      #colwell_stats <- function(varPos, datePos, n_states = 11) {
      n_states <- 11
      dat <- data.frame(dates = datePos)
      dat$month <- factor(strftime(dat$dates, format = "%m"))
      dat$year <- factor(strftime(dat$dates, format = "%Y"))
      dat$y_raw  <- varPos
      month_av_yrs <- aggregate(y_raw ~ month + year, dat, mean, na.rm = TRUE)
      month_av_yrs$breaks <- cut(month_av_yrs$y_raw,
                                 n_states, right = FALSE,
                                 include.lowest = TRUE)
      colwell_mat <- with(month_av_yrs, table(breaks, month))
      
      x <- colSums(colwell_mat, na.rm = TRUE)
      y <- rowSums(colwell_mat, na.rm = TRUE)
      z <- sum(colwell_mat, na.rm = TRUE)
      hx  <- -sum((x / z) * log2(x / z), na.rm = TRUE)
      hy  <- -sum((y / z) * log2(y / z), na.rm = TRUE)
      hxy <- -sum((colwell_mat / z) * log2(colwell_mat / z), na.rm = TRUE)
      colwell_c <- 1 - (hy / log2(n_states))
      colwell_m <- (hx + hy - hxy) / log2(n_states)
      colwell_p <- colwell_c + colwell_m
      
      my_df[i, "Constancy"]  <- colwell_c
      my_df[i, "Contigency"] <- colwell_m
      my_df[i, "Predictability"] <- colwell_p
    }  
    write.csv(my_df, file = gsub(" ","",paste("./csv_files/",varname,"_analyses_",qname,".csv")), row.names = FALSE)
  }
}

# Do for each 10 years interval
as.Date("2021-01-01")
# load matfile
for (ivar in 1:5) {
  varname <- switch(ivar,
                    'ssh',
                    'bottomT',
                    'sst',
                    'Salinity',
                    'mixedlayer')
  
  # work the variable, need to reload below
  filename <- (gsub(" ","",paste(filepath,varname,"_CNAPS.mat")))
  VarMaux  <- read_mat(filename)
  VarM    <- VarMaux$varM
  # Format the date-time object to "yyyy-mm-dd" format
  datetime <- as.POSIXct("1858-11-17", tz = "UTC") + timeAux$timeAll*86400
  datetime <- as.Date(datetime)
  
  # Extract the month and quarter using the months() and quarters() functions
  monthT <- months(datetime)
  quarterT <- quarters(datetime)
  
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
    datePos1 <- datetime[datetime <= as.Date("2009-01-01") & quarterT == qnumber]
    datePos2 <- datetime[datetime >= as.Date("2010-01-01") & datetime <= as.Date("2021-01-01") & quarterT == qnumber]
    
    # I am creating 2 matrices for this analyses
    VarM1  <-  VarM[(datetime <= as.Date("2009-01-01") & quarterT == qnumber), ]
    VarM2  <-  VarM[datetime >= as.Date("2010-01-01") & datetime <= as.Date("2021-01-01") & quarterT == qnumber, ]
    
    col_names <- c("Lat","Lon","Constancy1", "Contigency1", "Predictability1",
                   "Constancy2", "Contigency2", "Predictability2")
    
    row_names <- 1:ncol(VarM)
    my_df <- data.frame(matrix(NA, nrow = ncol(VarM), ncol = length(col_names)))
    colnames(my_df) <- col_names
    rownames(my_df) <- row_names
    my_df[, "Lat"] <- latlon[,1]
    my_df[, "Lon"] <- latlon[,2]
    
    for (isegment in 1:2) {
      if (isegment == 1) {
        VarMP    <- VarM1
        datePos <- datePos1
      } else {
        if (isegment == 2) {
          VarMP    <- VarM2
          datePos <- datePos2
        }
      }
      for (i in seq_len(ncol(VarM))) {
        varPos  <- VarMP[,i]
        # will try to do colwell directly by seasons - or quarters
        #colwell_stats <- function(varPos, datePos, n_states = 11) {
        n_states <- 11
        dat <- data.frame(dates = datePos)
        dat$month  <- factor(strftime(dat$dates, format = "%m"))
        dat$year   <- factor(strftime(dat$dates, format = "%Y"))
        dat$y_raw  <- varPos
        month_av_yrs <- aggregate(y_raw ~ month + year, dat, mean, na.rm = TRUE)
        month_av_yrs$breaks <- cut(month_av_yrs$y_raw,
                                   n_states, right = FALSE,
                                   include.lowest = TRUE)
        colwell_mat <- with(month_av_yrs, table(breaks, month))
        
        x   <- colSums(colwell_mat, na.rm = TRUE)
        y   <- rowSums(colwell_mat, na.rm = TRUE)
        z   <- sum(colwell_mat, na.rm = TRUE)
        hx  <- -sum((x / z) * log2(x / z), na.rm = TRUE)
        hy  <- -sum((y / z) * log2(y / z), na.rm = TRUE)
        hxy <- -sum((colwell_mat / z) * log2(colwell_mat / z), na.rm = TRUE)
        colwell_c <- 1 - (hy / log2(n_states))
        colwell_m <- (hx + hy - hxy) / log2(n_states)
        colwell_p <- colwell_c + colwell_m
        if (isegment == 1) {
          my_df[i, "Constancy1"]  <- colwell_c
          my_df[i, "Contigency1"] <- colwell_m
          my_df[i, "Predictability1"] <- colwell_p
        } else {
          my_df[i, "Constancy2"]  <- colwell_c
          my_df[i, "Contigency2"] <- colwell_m
          my_df[i, "Predictability2"] <- colwell_p
        }
      }
    }
    # save the first part of the analyses for segments
    write.csv(my_df, file = gsub(" ","",paste("./csv_files/",varname,"_envPred_",qname,".csv")), row.names = FALSE)
  }
 }