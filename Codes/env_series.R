library(envPred)
library(Hmisc)
library(tidyr)
library(gplots)
library(ggplot2)
library(RColorBrewer)
library(cmocean)
# for a readmat function that works with HDF5
library(raveio)

filepath <- ("~/Stuff/Current/SAtlantic/data/")
filename <- (gsub(" ","",paste(filepath,"timeAllCNAPS.mat")))
timeAux  <- read_mat(filename)

# Format the date-time object to "yyyy-mm-dd" format
datetime <- as.POSIXct("1858-11-17", tz = "UTC") + timeAux$timeAll*86400
datetime <-  as.Date(datetime)

# make for month 
start_date <- as.Date("1993-01-01")
end_date   <- as.Date("2021-12-01")
dateMonth <- seq.Date(from = start_date, to = end_date, by = "month")

# get lat and lon
filename <- (gsub(" ","",paste(filepath,"latlon_CNAPS.mat")))
latlon   <- read_mat(filename)
latlon   <- latlon$latlon_CNAPS

# "unbounded" fraction of the total variance that is due to predictable seasonal periodicity
# "bounded" fraction of the total variance that is due to predictable seasonal periodicity
# color - White noise occurs when there is no correlation between one measurement and the next 
# while for reddened noise, there is some correlation between measurements separated by a finite time-scale 
# Constancy (C) measures the extent to which the environment is the same for all months in all years. 
# Contingency (M) measures the extent to which the environmental differences between months are the same in all years. 
# Predictability (P) is the sum of Constancy (C) and Contingency (M). 

col_names <- c("Lat","Lon","USeasonal", "BSeasonal", "Colour", "Constancy", "Contigency", "Predictability",
               "InterAnnual","Annual","IntraAnnual","Synoptic","TrendLinear","Trend","Avg","Std")

# Define frequency band cutoffs based on Muñiz et al., 2021
#
# (1) inter annual frequencies 384–1152 days (>1-3 years)
# (2) annual frequencies 101–383 days 
# (3) intra-seasonal frequencies 20–100 days
# (4) synoptic frequencies 16–19 days
bands <- list(
  inter_annual = c(1/1152, 1/384),
  annual = c(1/383, 1/101),
  intra_seasonal = c(1/100, 1/20),
  synoptic = c(1/19, 1/16)
)
maxband <- 1/3650

filepath <- ("~/Stuff/Current/SAtlantic/data/CNAPS/")

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
  filepath <- ("/Users/anavaz/Stuff/Current/SAtlantic/data/CNAPS/")
  filename <- (gsub(" ","",paste(filepath,varname,"_CNAPS.mat")))
  VarMaux  <- read_mat(filename)
  VarM     <- VarMaux$varM
  rm(VarMaux)
  
  datePos <-datetime[datetime <= as.Date("2021-01-01")]

  row_names <- 1:ncol(VarM)
  my_df <- data.frame(matrix(NA, nrow = ncol(VarM), ncol = length(col_names)))
  colnames(my_df) <- col_names
  rownames(my_df) <- row_names
  my_df[, "Lat"] <- latlon[,1]
  my_df[, "Lon"] <- latlon[,2]
  
  for (i in seq_len(ncol(VarM))) {
    varPos  <- VarM[,i]
    varPos  <-  varPos[datetime <= as.Date("2021-01-01")]

    # do the envPred
    VarPred <- env_stats(time_series = varPos, dates = datePos,
                          n_states = 11, delta = 1, is_uneven = FALSE,
                          interpolate = FALSE, show_warns = TRUE,
                          noise_method = "spectrum")
    
    my_df[i, "USeasonal"]  <- VarPred$unbounded_seasonality
    my_df[i, "BSeasonal"]  <- VarPred$bounded_seasonality
    my_df[i, "Colour"]     <- VarPred$env_col
    my_df[i, "Constancy"]  <- VarPred$colwell_c
    my_df[i, "Contigency"] <- VarPred$colwell_m
    my_df[i, "Predictability"] <- VarPred$colwell_p
    
    # do trend analyses two ways
    # ------------------------------------------------------------
    varPos <- VarM[,i]
    
    # do avg and std
    my_df[i, "Avg"] <- mean(varPos)
    my_df[i, "Std"] <- sd(varPos)
    
    model <- lm(varPos ~ seq(1,length(varPos)))
    model_summary <- summary(model)
    
    # Extract the p-value for the slope coefficient
    p_value <- model_summary$coefficients["seq(1, length(varPos))", "Pr(>|t|)"]
    # Check if the p-value is less than your chosen significance level (e.g., 0.05)
    if (p_value < 0.05) {
      # slope of the line is given by coef(model)[2]
      # only save if significant 
      my_df[i, "TrendLinear"] <- coef(model)[2]
    } else {
      my_df[i, "TrendLinear"] <- NaN
    }
 
    # try with the Seasonal Decomposition of Time Series (stl) from stats
    varPos_ts <- ts(varPos, start = c(1993, 1), frequency = 365.25)
    decomp <- stl(varPos_ts, s.window = "periodic")
    
    # extract the trend component
    trend <- decomp$time.series[, "trend"]
    model <- lm(trend ~ seq(1,length(trend)))
    model_summary <- summary(model)
    # Extract the p-value for the slope coefficient
    p_value <- model_summary$coefficients["seq(1, length(trend))", "Pr(>|t|)"]
    # Check if the p-value is less than your chosen significance level (e.g., 0.05)
    if (p_value < 0.05) {
       my_df[i, "Trend"] <- coef(model)[2]
    } else {
      my_df[i, "Trend"] <- NaN
    }
    
    # do spectral analyses, sum different bands
    #--------------------------------------------------------------
    spec <- spectrum(varPos, spans = c(2, 2), plot = FALSE)
    freq_range <- which(spec$freq >= maxband)
    TSD   <- sum(spec$spec[freq_range])
    # TSD  <- sum(spec$spec)
    # Calculate energy in each frequency band
    band_energy <- numeric(length(bands))
    for (iBand in seq_along(bands)) {
      freq_range <- which(spec$freq >= bands[[iBand]][1] & spec$freq < bands[[iBand]][2])
      band_energy[iBand] <- sum(spec$spec[freq_range])/TSD
    }
    my_df[i, "InterAnnual"] <- band_energy[1]
    my_df[i, "Annual"]      <- band_energy[2]
    my_df[i, "IntraAnnual"] <- band_energy[3]
    my_df[i, "Synoptic"]    <- band_energy[4]
    
  }
  write.csv(my_df, file = gsub(" ","",paste("./csv_files/",varname,"_analyses.csv")), row.names = FALSE)
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
  filepath <- ("/Users/anavaz/Stuff/Current/SAtlantic/data/CNAPS/")
  filename <- (gsub(" ","",paste(filepath,varname,"_CNAPS.mat")))
  VarMaux  <- read_mat(filename)
  VarM    <- VarMaux$varM
  # Format the date-time object to "yyyy-mm-dd" format
  datetime <- as.POSIXct("1858-11-17", tz = "UTC") + timeAux$timeAll*86400
  datetime <- as.Date(datetime)
   
  datePos1 <- datetime[datetime <= as.Date("2009-01-01")]
  datePos2 <- datetime[datetime >= as.Date("2010-01-01") & datetime <= as.Date("2021-01-01")]
  
  # I am creating 2 matrices for this analyses
  VarM1  <-  VarM[datetime <= as.Date("2009-01-01"), ]
  VarM2  <-  VarM[datetime >= as.Date("2010-01-01") & datetime <= as.Date("2021-01-01"), ]
  
  col_names <- c("Lat","Lon","USeasonal1", "BSeasonal1", "Colour1", "Constancy1", "Contigency1", "Predictability1",
                             "USeasonal2", "BSeasonal2", "Colour2", "Constancy2", "Contigency2", "Predictability2")
  
  row_names <- 1:ncol(VarM)
  my_df <- data.frame(matrix(NA, nrow = ncol(VarM), ncol = length(col_names)))
  colnames(my_df) <- col_names
  rownames(my_df) <- row_names
  my_df[, "Lat"] <- latlon[,1]
  my_df[, "Lon"] <- latlon[,2]
  
  for (isegment in 1:2) {
    if (isegment == 1) {
      VarM    <- VarM1
      datePos <- datePos1
    } else {
      if (isegment == 2) {
        VarM    <- VarM2
        datePos <- datePos2
      }
    }
    for (i in seq_len(ncol(VarM))) {
      varPos  <- VarM[,i]
      any(is.na(varPos))
      # do the envPred
      VarPred <- env_stats(time_series = varPos, dates = datePos,
                           n_states = 11, delta = 1, is_uneven = FALSE,
                           interpolate = FALSE, show_warns = TRUE,
                           noise_method = "spectrum")
      
      my_df[i, gsub(" ","",paste("USeasonal",isegment))]  <- VarPred$unbounded_seasonality
      my_df[i, gsub(" ","",paste("BSeasonal",isegment))]  <- VarPred$bounded_seasonality
      my_df[i, gsub(" ","",paste("Colour",isegment))]     <- VarPred$env_col
      my_df[i, gsub(" ","",paste("Constancy",isegment))]  <- VarPred$colwell_c
      my_df[i, gsub(" ","",paste("Contigency",isegment))] <- VarPred$colwell_m
      my_df[i, gsub(" ","",paste("Predictability",isegment))] <- VarPred$colwell_p
    }
  }
  # save the first part of the analyses for segments
  write.csv(my_df, file = gsub(" ","",paste("./csv_files/",varname,"_envPred.csv")), row.names = FALSE)
 
   # load all variables again since we change VarM
  VarM    <- VarMaux$varM
  # Format the date-time object to "yyyy-mm-dd" format
  datetime <-  as.POSIXct("1858-11-17", tz = "UTC") + timeAux$timeAll*86400
  datetime <-  as.Date(datetime)
  # I am creating 2 matrices for this analyses
  datePos1 <- datetime[datetime <= as.Date("2009-01-01")]
  datePos2 <- datetime[datetime >= as.Date("2010-01-01")]
  VarM1  <-  VarM[datetime <= as.Date("2009-01-01"), ]
  VarM2  <-  VarM[datetime >= as.Date("2010-01-01"), ]
  col_names <- c("Lat","Lon","InterAnnual1","Annual1","IntraAnnual1","Synoptic1",
                             "InterAnnual2","Annual2","IntraAnnual2","Synoptic2",
                             "TrendLinear1","Trend1","Avg1","Std1",
                             "TrendLinear2","Trend2","Avg2","Std2")
  row_names <- 1:ncol(VarM)
  my_df <- data.frame(matrix(NA, nrow = ncol(VarM), ncol = length(col_names)))
  colnames(my_df) <- col_names
  rownames(my_df) <- row_names
  my_df[, "Lat"] <- latlon[,1]
  my_df[, "Lon"] <- latlon[,2]
  
  for (isegment in 1:2) {
    if (isegment == 1) {
      VarM    <- VarM1
      datePos <- datePos1
    } else {
       if (isegment == 2) {
         VarM    <- VarM2
         datePos <- datePos2
       }
    }
    for (i in seq_len(ncol(VarM))) {
      varPos  <- VarM[,i]
      # do avg and std
      my_df[i, gsub(" ","",paste("Avg",isegment))] <- mean(varPos)
      my_df[i, gsub(" ","",paste("Std",isegment))] <- sd(varPos)
      
      # do trend analyses two ways
      # ------------------------------------------------------------
      model <- lm(varPos ~ seq(1,length(varPos)))
      model_summary <- summary(model)
      # Extract the p-value for the slope coefficient
      p_value <- model_summary$coefficients["seq(1, length(varPos))", "Pr(>|t|)"]
      # Check if the p-value is less than your chosen significance level (e.g., 0.05)
      if (p_value < 0.05) {
        # slope of the line is given by coef(model)[2]
        # only save if significant 
        my_df[i, gsub(" ","",paste("TrendLinear",isegment))] <- coef(model)[2]
      } else {
        my_df[i, gsub(" ","",paste("TrendLinear",isegment))] <- NaN
      }
      
      # try with the TS package
      if (isegment == 2) {
        varPos_ts <- ts(varPos, start = c(2010, 1), frequency = 365.25)
      } else {
        varPos_ts <- ts(varPos, start = c(1993, 1), frequency = 365.25)
      }
      decomp    <- stl(varPos_ts, s.window = "periodic")
      # extract the trend component
      trend <- decomp$time.series[, "trend"]
      model <- lm(trend ~ seq(1,length(trend)))
      model_summary <- summary(model)
      # Extract the p-value for the slope coefficient
      p_value <- model_summary$coefficients["seq(1, length(trend))", "Pr(>|t|)"]
      # Check if the p-value is less than your chosen significance level (e.g., 0.05)
      if (p_value < 0.05) {
        my_df[i, gsub(" ","",paste("Trend",isegment))] <- coef(model)[2]
      } else {
        my_df[i, gsub(" ","",paste("Trend",isegment))] <- NaN
      }
      
      # do spectral analyses, sum different bands
      #--------------------------------------------------------------
      spec <- spectrum(varPos, spans = c(2, 2), plot = FALSE)
      freq_range <- which(spec$freq >= maxband)
      # TSD  <- sum(spec$spec)
      TSD   <- sum(spec$spec[freq_range])
      # Calculate energy in each frequency band
      band_energy <- numeric(length(bands))
      for (iBand in seq_along(bands)) {
        freq_range <- which(spec$freq >= bands[[iBand]][1] & spec$freq < bands[[iBand]][2])
        # normalize the band energy by TSD to find the relative contributions 
        # to the total spectral density 
        band_energy[iBand] <- sum(spec$spec[freq_range])/TSD
      }
      my_df[i, gsub(" ","",paste("InterAnnual",isegment))] <- band_energy[1]
      my_df[i, gsub(" ","",paste("Annual",isegment))]      <- band_energy[2]
      my_df[i, gsub(" ","",paste("IntraAnnual",isegment))] <- band_energy[3]
      my_df[i, gsub(" ","",paste("Synoptic",isegment))]    <- band_energy[4]
    }
  }
  write.csv(my_df, file = gsub(" ","",paste("./csv_files/",varname,"_spectral.csv")), row.names = FALSE)
}