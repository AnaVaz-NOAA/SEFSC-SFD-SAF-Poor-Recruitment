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
library(WaveletComp)
library(gridGraphics)
library(ggpubr)

filepath <- ("/Users/anavaz/Stuff/Current/SAtlantic/CNAPS_mat/")
#filepath <- ("/Users/anacarolvaz/Stuff/current_work/SAtlantic/CNAPS_mat/")
filename <- (gsub(" ","",paste(filepath,"timeAllCNAPS.mat")))
timeAux  <- read_mat(filename)

# Format the date-time object to "yyyy-mm-dd" format
datetime  <- as.POSIXct("1858-11-17", tz = "UTC") + timeAux$timeAll*86400
datetime  <- as.Date(datetime)
datetimeF <- format(datetime, "%Y-%m-%d")
# Convert datetime to numeric
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

# For mean series all regions
# load matfile
for (ivar in 1:5) {
  varname <- switch(ivar,
                    'bottomT',
                    'sst',
                    'mixedlayer',
                    'ssh',
                    'salinity')
  varprint <- switch(ivar,
                     'Bottom Temperature',
                     'SST',
                     'Mixed Layer Depth',
                     'SSH',
                     'Salinity')
  filename <- (gsub(" ","",paste(filepath,varname,"_CNAPS.mat")))
  VarMaux  <- read_mat(filename)
  VarM     <- VarMaux$varM
  VarAvg   <- rowMeans(VarM)
  x  <- 1:length(VarAvg)
  df <- data.frame(x=datetime_numeric, y=VarAvg)
  
  # testing loess aside from analyses
  # Fit a LOESS model
  loess_model1 <- loess(y ~ x, data = df, span = 0.01)
  loess_model2 <- loess(y ~ x, data = df, span = 0.025)
  loess_model5 <- loess(y ~ x, data = df, span = 0.05)
  
  # Predict smoothed values
  df$smoothed1 <- predict(loess_model1)
  df$smoothed2 <- predict(loess_model2)
  df$smoothed5 <- predict(loess_model5)
  
  # plot the filtered data with LOESS
  png(gsub(" ","",paste("./images/smoothed_",varname,".png")), 
      width = 2000, height = 1200, res = 300)
  ggplot(df, aes(x, y)) + 
    geom_point(aes(x,y), df, alpha=0.25, color = "lightgray") +
    geom_line(data = df, aes(x, smoothed1), colour = "deeppink") +
    geom_line(data = df, aes(x, smoothed2), colour = "deepskyblue2") +
    geom_line(data = df, aes(x, smoothed5), colour = "seagreen3") +
    scale_color_manual(values = c("smoothed1" = "deeppink", "smoothed2" = "deepskyblue2", "smoothed5" = "seagreen3")) +
    labs(x = "Days", y = "Magnitude", color = "Smooth") +
    theme_light()
  dev.off()
  
  my.data <- data.frame(x = df$smoothed1, date = as.POSIXct(datetime))
  my.w    <- analyze.wavelet(my.data, "x", make.pval = TRUE, n.sim = 10)
  maximum.level = 1.001*max(my.w$Power.avg, my.w$Power.avg)
  png(gsub(" ","",paste("./images/wavelet_days_",varname,"_smoothed1.png")), 
      width = 2000, height = 1200, res = 300)
  wt.image(my.w, color.key = "interval", n.levels = 250, 
           legend.params = list(lab = "wavelet power levels"),
           periodlab = "Period (Days)",
           exponent = 0.5,
           timetcl = -0.5,
           show.date = TRUE, date.format = "%F", timelab = "")
  dev.off()
  png(gsub(" ","",paste("./images/wavelet_period_",varname,"_smoothed1.png")), 
      width = 600, height = 1200, res = 150)
  wt.avg(my.w, maximum.level = maximum.level)
  dev.off()
  
  my.data <- data.frame(x = df$y, date = as.POSIXct(datetime))
  my.w    <- analyze.wavelet(my.data, "x", make.pval = TRUE, n.sim = 10)
  maximum.level = 1.001*max(my.w$Power.avg, my.w$Power.avg)
  
  # need to figure how to extract the ridge 
  #ridge <- my.w$Period * my.w$Ridge
  #ridge <- colSums(ridge)
  #ridge[ridge == 0] <- NA
  
  png(gsub(" ","",paste("./images/wavelet_days_",varname,".png")), 
      width = 2000, height = 1200, res = 300)
  # This was my first graph settings (page 50)
  # color.key default is quantile, which overestimates the low power and under the high
  # interval gives a more realistic view 
  plot1 <- wt.image(my.w, color.key = "interval", n.levels = 250, 
           legend.params = list(lab = "wavelet power levels",label.digits = 2),
           periodlab = "Period (Days)", #maximum.level = 1.35,
  # this is to plot the sqrt of power so lower periods are more evident
           exponent = 0.5, 
           timetcl = -0.5, # draws outward ticks
  # include dates on x axis
           show.date = TRUE, date.format = "%F", timelab = "")
  dev.off()
  
  png(gsub(" ","",paste("./images/wavelet_period_",varname,".png")), 
      width = 600, height = 600, res = 150)
  plot2 <- wt.avg(my.w, maximum.level = maximum.level)
  dev.off()
  
  my.wSeason <- analyze.wavelet(my.data, "x", 
                             lowerPeriod = 64, upperPeriod = 128,
                             dt = 1,
                             make.pval = TRUE, n.sim = 10)
 
  png(gsub(" ","",paste("./images/wavelet_days_",varname,"_64_128.png")), 
      width = 2000, height = 1200, res = 300) 
  wt.image(my.wSeason, color.key = "interval", n.levels = 250, 
           legend.params = list(lab = "wavelet power levels",label.digits = 2),
           periodlab = "Period (Days)", ##maximum.level = 0.5,
           # this is to plot the sqrt of power so lower periods are more evident
           exponent = 0.5, 
           timetcl = -0.5, # draws outward ticks
           show.date = TRUE, date.format = "%F", timelab = "") # have date
  dev.off()
  png(gsub(" ","",paste("./images/power_",varname,"_64_128.png")), 
      width = 2000, height = 1200, res = 300)
  plot(as.POSIXct(datetime),colSums(my.wSeason$Power),type="l")
  title(xlab="Date",ylab="Sum(Power)")
  dev.off()

  my.wIntraAnnual <- analyze.wavelet(my.data, "x", 
                                lowerPeriod = 128, upperPeriod = 256,
                                dt = 1,
                                make.pval = TRUE, n.sim = 10)
  
  png(gsub(" ","",paste("./images/wavelet_days_",varname,"_128_256.png")), 
      width = 2000, height = 1200, res = 300) 
  wt.image(my.wIntraAnnual, color.key = "interval", n.levels = 250, 
           legend.params = list(lab = "wavelet power levels",label.digits = 2),
           periodlab = "Period (Days)", #maximum.level = 0.8,
           # this is to plot the sqrt of power so lower periods are more evident
           exponent = 0.5, 
           timetcl = -0.5, # draws outward ticks
           show.date = TRUE, date.format = "%F", timelab = "") # have date
  dev.off()
  png(gsub(" ","",paste("./images/power_",varname,"_128_256.png")), 
      width = 2000, height = 1200, res = 300)
  plot(as.POSIXct(datetime),colSums(my.wIntraAnnual$Power),type="l")
  title(xlab="Date",ylab="Sum(Power)")
  dev.off()
  
  my.wAnnual <- analyze.wavelet(my.data, "x", 
                                     lowerPeriod = 256, upperPeriod = 512,
                                     dt = 1,
                                     make.pval = TRUE, n.sim = 10)
  
  png(gsub(" ","",paste("./images/wavelet_days_",varname,"_256_512.png")), 
      width = 2000, height = 1200, res = 300) 
  wt.image(my.wAnnual, color.key = "interval", n.levels = 250, 
           legend.params = list(lab = "wavelet power levels",label.digits = 2),
           periodlab = "Period (Days)", #maximum.level = 1.7,
           # this is to plot the sqrt of power so lower periods are more evident
           exponent = 0.5, 
           timetcl = -0.5, # draws outward ticks
           show.date = TRUE, date.format = "%F", timelab = "") # have date
  dev.off()
  png(gsub(" ","",paste("./images/power_",varname,"_256_512.png")), 
      width = 2000, height = 1200, res = 300)
  plot(as.POSIXct(datetime),colSums(my.wAnnual$Power),type="l")
  title(xlab="Date",ylab="Sum(Power)")
  dev.off()
  
  my.wLT <- analyze.wavelet(my.data, "x", 
                                lowerPeriod = 512, upperPeriod = 1024,
                                dt = 1,
                                make.pval = TRUE, n.sim = 10)
  
  png(gsub(" ","",paste("./images/wavelet_days_",varname,"_512_1024.png")), 
      width = 2000, height = 1200, res = 300) 
  wt.image(my.wLT, color.key = "interval", n.levels = 250, 
           legend.params = list(lab = "wavelet power levels",label.digits = 2),
           periodlab = "Period (Days)", #maximum.level = 0.1,
           # this is to plot the sqrt of power so lower periods are more evident
           exponent = 0.5, 
           timetcl = -0.5, # draws outward ticks
           show.date = TRUE, date.format = "%F", timelab = "") # have date
  dev.off()
  png(gsub(" ","",paste("./images/power_",varname,"_512_1024.png")), 
      width = 2000, height = 1200, res = 300)
  plot(as.POSIXct(datetime),colSums(my.wLT$Power),type="l")
  title(xlab="Date",ylab="Sum(Power)")
  dev.off()
}

# # my.w    <- analyze.wavelet(my.data, "x", make.pval = TRUE, n.sim = 10,
# #                           lowerPeriod = 64, upperPeriod = 2048,
# #                           loess.span = 0.05,
# #                           dt = 1)
# 
# # Cutting table 
# 
# model <- lm(VarAvg ~ datetime_numeric)
# trendline <- predict(model)
# # remove the slope of the line
# aux <- VarAvg - trendline
# # corrected by anomaly 
# VarFilter <- (aux)/sd(VarAvg)
# 
# # testing loess with plot
# df <- data.frame(x=datetime_numeric, y=VarFilter)
# 
# #spans <- c(0.75, 0.5, 0.25, 0.1, 0.01)
# spans <- 0.01
# k <- length(spans)
# i <- 1
# g <- ggplot(df, aes(x, y)) + geom_line(aes(x,y), df, alpha=0.25)
# #for (i in 1:k) {
# png(gsub(" ","",paste("./images/timeseries_",varname,"_",spans[i],".png")), width = 2000, height = 1000, res = 300)
# g + geom_smooth(method="loess", span=spans[i])  +
#   labs(title=paste("Span =", spans[i]))
# dev.off()
# #}
# 
# # View the filtered data
# png(gsub(" ","",paste("./images/timeseries_",varname,".png")), width = 2000, height = 1000, res = 300)
# par(mfrow = c(2, 1), mar = c(2, 2, 2, 2))
# plot(datetime, VarAvg, col = "gray30", type = 'l', main = paste(varprint), xlab = "Time", ylab = "Variable")
# lines(datetime, trendline, col = "red", lwd = 2)
# plot(datetime, VarFilter, col = "gray30", type = 'l', 
#      main = paste(varprint," Anomaly"), xlab = "Time", ylab = "Variable")
# abline(h = 0, col = "gray", lty = 2)
# dev.off()
