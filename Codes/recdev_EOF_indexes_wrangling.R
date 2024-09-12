# Read in .rdat files from SEDAR assessments and extract recruitment deviations
# BAsed on K. Wade file Jun/2022, modified by A. Vaz Feb/2023
library(Hmisc)
library(tidyr)
library(R.matlab)
library(metR)

# set the directory with the assessment files
#setwd("/Users/anavaz/Stuff/github/Poor-Recruitment-South-Atlantic/")
filedir <- "./data/recruitment/updates/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
sppName <- c("Gag","GAm","GTr","RPo","RGr","BSB","RSn","Sca","Sno","Ver")
NamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish",
              " Red Porgy","Red Grouper","Black Sea Bass",
              "Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")
sppSeasonSp <- c(1,2,2,1,1,1,2,1,3,2)
index <- 1

# first year of assessments accross species is 1974, most recent is 2021
yearsAssessments <- 1973:2021

# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsAssessments), ncol=10, dimnames=list(yearsAssessments, sppName))

# FOR AFS TALK

# First plot all the blue lines together
png("./images/TimeSeries/RecDevBlue.png", res = 300, width = 2500, height = 3000)
par(mfrow = c(4, 3), mar=c(1.5, 1.5, 1.5, 1.5), oma=c(1, 1, 1, 1), mai=c(0.3, 0.3, 0.3, 0.3))

index <- 1
for (inFile in file_list) {
  filename <- file.path(filedir, inFile)
  fileaux <- dget(filename)
  devaux  <- fileaux$t.series$logR.dev
  
  if (index == 6) { 
    yraux  <- fileaux$t.series$year 
  } else {
    yraux  <- fileaux$t.series$year - 1
  }
  
  yr  <- yraux[devaux != 0 & !is.na(devaux)]
  dev <- devaux[devaux != 0 & !is.na(devaux)]
  
  File.out[yearsAssessments %in% yr, index] = dev
  
  if (sppSeasonSp[index] == 1) {  # Plot only blue
    plot(yearsAssessments, rep(0,length(yearsAssessments)), ylim=c(-1.5, 1.5), type="n", ylab="log residuals", xlab="", main = NamePlot[index])
    lines(yr, dev, lty=1, col="dodgerblue4", lwd = 2)
    abline(h=0, col="gray70")
  }
  
  index <- index + 1
}
dev.off()

# Now plot all the red lines together
png("./images/TimeSeries/RecDevRed.png", res = 300, width = 2500, height = 3000)
par(mfrow = c(4, 3), mar=c(1.5, 1.5, 1.5, 1.5), oma=c(1, 1, 1, 1), mai=c(0.3, 0.3, 0.3, 0.3))

index <- 1
for (inFile in file_list) {
  filename <- file.path(filedir, inFile)
  fileaux <- dget(filename)
  devaux  <- fileaux$t.series$logR.dev
  
  if (index == 6) { 
    yraux  <- fileaux$t.series$year 
  } else {
    yraux  <- fileaux$t.series$year - 1
  }
  
  yr  <- yraux[devaux != 0 & !is.na(devaux)]
  dev <- devaux[devaux != 0 & !is.na(devaux)]
  
  File.out[yearsAssessments %in% yr, index] = dev
  
  if (sppSeasonSp[index] == 2) {  # Plot only red
    plot(yearsAssessments, rep(0,length(yearsAssessments)), ylim=c(-1.5, 1.5), type="n", ylab="log residuals", xlab="", main = NamePlot[index])
    lines(yr, dev, lty=1, col="tomato4", lwd = 2)
    abline(h=0, col="gray70")
  }
  
  index <- index + 1
}
dev.off()



#png("./images/TimeSeries/RecDev.png",res = 300,width = 2000, height = 2000)
png("./images/TimeSeries/RecDevSeason.png",res = 300, width = 2500, height = 3000)
#Graph the time series of recruitment deviations

par(mfrow = c(4, 3), mar=c(1.5, 1.5, 1.5, 1.5), oma=c(1, 1, 1, 1), mai=c(0.3, 0.3, 0.3, 0.3))

# Loop through assessment files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  
  # Read the rdat file for the current species
  fileaux <- dget(filename)
  devaux  <- fileaux$t.series$logR.dev; 
  
  # black sea bass (6th file) start at age 0, all others at age 1
  if (index == 6) { 
    yraux  <- fileaux$t.series$year 
  } else {
    # start at age 1, needs to remove 1 from their year
    yraux  <- fileaux$t.series$year - 1
  }
  # Limit each time series to years where recruitment deviations are estimated
  yr  <- yraux[devaux!=0  & !is.na(devaux)]
  dev <- devaux[devaux!=0 & !is.na(devaux)]

  # File.out[yearsAssessments%in%yr,index] = (dev - mean(dev))/sd(dev);
  File.out[yearsAssessments%in%yr,index] = dev
  
  # plot the graph
  if (sppSeasonSp[index] == 1) {
    plot(yearsAssessments, rep(0,length(yearsAssessments)), ylim=c(-1.5, 1.5), type="n", ylab="log residuals", xlab="", main = NamePlot[index])
    lines(yr,dev,lty=1, col="dodgerblue4", lwd = 2);
  }else if (sppSeasonSp[index] == 2) {
    plot(yearsAssessments, rep(0,length(yearsAssessments)), ylim=c(-1.5, 1.5), type="n", ylab="log residuals", xlab="", main = NamePlot[index])
    lines(yr,dev,lty=1, col="tomato4", lwd = 2);
  } else {
    plot(yearsAssessments, rep(0,length(yearsAssessments)), ylim=c(-1.5, 1.5), type="n", ylab="log residuals", xlab="", main = NamePlot[index])
  lines(yr,dev,lty=1, col="black", lwd = 2);
  }
  abline(h=0, col="gray70")
  # to know what is the species
  index <- index +1
}
dev.off()

write.csv(File.out, file="./csv_files/RecruitmentResiduals_Feb23.csv", quote=F)

################################################################################
# now read all the indexes of oscillation

# set the diretory with the assessment files
filedir <- "./data/indexes/use/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("AMO","MEI","NAO")

index <- 1
#Graph the time series of recruitment deviations
png("./images/TimeSeries/Indexes.png",res = 300, width = 2000, height = 2000)
par(mfrow = c(3, 1), mar=c(0.5, 0.5, 0.5, 0.5), oma=c(0, 0, 0, 0), mai=c(0.3, 0.3, 0.3, 0.3))

yearsIndex <- seq(1973,2021,1/12)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsIndex), ncol=3, dimnames=list(yearsIndex, NamePlot))

# Loop through Indexes files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  
  if (index < 3 ) {
    # Read the first and last year of file
    header <- readLines(filename, n = 1)
    
    # Extract the first and last year from the header
    yearB <- substring(header, 3, 6)
    yearE <- substring(header, nchar(header) - 3, nchar(header))
    nlines <- as.numeric(yearE) - as.numeric(yearB) + 1
    
    # Print the first and last year
    cat("First year:", yearB, "\n")
    cat("Last year:", yearE, "\n")
    
    # read the file
    indaux <- read.table(filename, header = FALSE, skip = 1, nrows = nlines)
    
    # Add column names for the months
    colnames(indaux)[-1] <- paste("month", 1:12, sep = "_")
    colnames(indaux)[1] <- "year"
    # Convert from wide to long format
    indlong <- pivot_longer(indaux, cols = -1, names_prefix = "month_")
    indlong$value[indlong$value==-99.99] <- NaN
    
  } else{
    # read NAO which is already in long format
    indlong <- read.table(filename)
    colnames(indlong) <- c("year", "month", "value")
    yearB <- min(indlong$year)
    yearE <- max(indlong$year)
  }
  aux <- seq(as.numeric(yearB),as.numeric(yearE)+(1-1/12),1/12)
  indlong <- cbind(aux, indlong)
  colnames(indlong)[1] <- "yearPlot"
  # only use for the years we have assessment
  File.out[yearsIndex%in%indlong$yearPlot,index] = indlong$value[indlong$yearPlot >= 1973 & indlong$yearPlot <= 2021]
  
  # plot the graph
  switch (index,
            plot(yearsIndex, rep(0,length(yearsIndex)), ylim=c(-.75, .75), type="n", ylab="", xlab="", main = NamePlot[index]),
            plot(yearsIndex, rep(0,length(yearsIndex)), ylim=c(-2.5, 2.5), type="n", ylab="", xlab="", main = NamePlot[index]),
            plot(yearsIndex, rep(0,length(yearsIndex)), ylim=c(-3, 3), type="n", ylab="", xlab="", main = NamePlot[index])
          )
    lines(indlong$yearPlot, indlong$value, lty=1, col=4)
    abline(h=0, col="gray70")
  # to know what is the index
  index <- index +1
}
dev.off()
write.csv(File.out, file="./csv_files/indexes_feb23.csv", quote=F)

################################################################################
# now read all EOF (PCA for time)

# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/EOF_CNAPS/All/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Bottom T","Mixed Layer","Salinity","SSH","SST")
NameModes <- c("Mode 1","Mode 2")

# create combinations of names and combine the names
aux <- expand.grid(NamePlot, NameModes)
NamePlotModes <- paste(aux$Var1, aux$Var2)

index <- 1
#Graph the time series of recruitment deviations
png("./images/TimeSeries/PCAs_monthly.png",res = 300,width = 2000, height = 2000)
par(mfrow = c(4, 2), mar=c(0.15, 0.15, 0.15, 0.15), oma=c(0, 0, 0, 0), mai=c(0.2, 0.2, 0.2, 0.2))

yearsEOF <- seq(1993,2022-(1/12),1/12)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=10, dimnames=list(yearsEOF, NamePlotModes))

# Loop through EOF files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  EOFtime  <- EOFaux$EOFtime
  # each EOF has 2 modes
  for (i in 1:2) {
    # save each mode
    File.out[,index] = EOFtime[,i]
    # plot each mode
    limit <- max(abs(EOFtime[,i]))
    plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlot[index])
    lines(yearsEOF, EOFtime[,i], lty=1, col=4)
    abline(h=0, col="gray70")
    # increase index
    index <- index +1
  }
}
write.csv(File.out, file="./csv_files/EOF_CNAPS.csv", quote=F)

################################################################################
# now read all EOF Seasonal (PCA for time)

# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/EOF_CNAPS/SeasonConst/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Bottom T", "Mixed Layer","Salinity","SSH","SST")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")
NameModes <- c("Mode 1","Mode 2")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var3, aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(1993,2021)
# create matrix for saving recdevs

File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=40, dimnames=list(yearsEOF, NamePlotModes))

# Loop through EOF files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  EOFtime  <- EOFaux$EOFtime
  # each EOF has 4 modes (saving and plotting only 2 first ones)
  for (i in 1:2) {
    # save each mode
    File.out[,index] = EOFtime[,i]
    # plot each mode
    limit <- max(abs(EOFtime[,i]))
    # check if newplot  if index == 1, or index divided by 5 is 0
    if (index == 1 || (index-1)%%8 == 0) {
      if (index > 1) {
        dev.off()
      }
      png(gsub(" ","",(paste("./images/TimeSeries/EOF_Seasonal_",NamePlot[((index-1)/8)+1],".png"))),res = 300,width = 2000, height = 2000)
      par(mfrow = c(4, 2), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
    }
    plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
    lines(yearsEOF, EOFtime[,i], lty=1, col=4)
    abline(h=0, col="gray70")
    # increase index
    index <- index +1
  }
}
dev.off()
write.csv(File.out, file="./csv_files/EOF_Seasonal_CNAPS.csv", quote=F)

################################################################################
# now read all EOF Spawning Season (Winter - Feb to April, Summer - Jun to Aug

# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/EOF_CNAPS/SpSeasonConst/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Bottom T","Mixed Layer","Salinity","SSH","SST")
NameSeason <- c("Summer", "Winter")
NameModes <- c("Mode 1","Mode 2")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var3, aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(1993,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=20, dimnames=list(yearsEOF, NamePlotModes))

# Loop through EOF files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  EOFtime  <- EOFaux$EOFtime
  # each EOF has 2 modes
  for (i in 1:2) {
    # save each mode
    File.out[,index] = EOFtime[,i]
    # plot each mode
    limit <- max(abs(EOFtime[,i]))
    # check if newplot  if index == 1, or index divided by 5 is 0
    if (index == 1 || (index-1)%%4 == 0) {
      if (index > 1) {
        dev.off()
      }
      png(gsub(" ","",(paste("./images/TimeSeries/EOF_Spawning_Seasonal_",NamePlot[((index-1)/4)+1],".png"))),res = 300, width = 2000, height = 2000)
      par(mfrow = c(4, 1), mar=c(.1, .1, .1, .1), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
    }
    plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
    lines(yearsEOF, EOFtime[,i], lty=1, col=4)
    abline(h=0, col="gray70")
    # increase index
    index <- index +1
  }
}
write.csv(File.out, file="./csv_files/EOF_Spawning_Seasonal_CNAPS.csv", quote=F)

################################################################################
# now read anomaly Seasonal (avg over time)
# 
# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/Anomaly/Seasonal"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Bottom Temp" ,"Mixed Layer", "Salinity", "SSH", "SST")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(1993,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=20, dimnames=list(yearsEOF, NamePlotModes))

# Loop through EOF files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  File.out[,index]  <- EOFaux$blaux
  
  # plot each season
  limit <- max(abs(File.out[,index]))
  # check if newplot  if index == 1, or index divided by 5 is 0
  if (index == 1 || (index-1)%%20 == 0) {
    if (index > 1) {
      dev.off()
    }
    png(gsub(" ","",(paste("Anomaly_Seasonal_",NamePlot[((index-1)/16)+1],".png"))),res = 300,width = 2000, height = 1500)
    par(mfrow = c(4, 5), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
  }
  plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
  lines(yearsEOF, File.out[,index], lty=1, col=4)
  abline(h=0, col="gray70")
  # increase index
  index <- index +1
}
dev.off()
write.csv(File.out, file="Anomaly_Seasonal.csv", quote=F)

# ################################################################################
# # now read ALL Spawning Season (Winter - Feb to April, Summer - Jun to Aug
# # set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/Anomaly/SeasonalSp/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Bottom Temp" ,"Mixed Layer", "Salinity", "SSH", "SST")
NameSeason <- c("Summer", "Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(1993,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=10, dimnames=list(yearsEOF, NamePlotModes))

# Loop through EOF files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  File.out[,index]  <- EOFaux$blaux
  
  # plot each season
  limit <- max(abs(File.out[,index]))
  # check if newplot  if index == 1, or index divided by 5 is 0
  if (index == 1 || (index-1)%%10 == 0) {
    if (index > 1) {
      dev.off()
    }
    png(gsub(" ","",(paste("Anomaly_SpSeasonal_",NamePlot[((index-1)/16)+1],".png"))),res = 300,width = 2000, height = 1500)
    par(mfrow = c(4, 3), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
  }
  plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
  lines(yearsEOF, File.out[,index], lty=1, col=4)
  abline(h=0, col="gray70")
  # increase index
  index <- index +1
}
dev.off()
write.csv(File.out, file="Anomaly_Spawning_Seasonal.csv", quote=F)

################################################################################
# now read anomaly Seasonal (avg over time)
# 
# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/CNAPS_Shelf_15/anomaly/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Bottom Temp" ,"Mixed Layer", "Salinity", "SSH", "SST")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(1993,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=20, dimnames=list(yearsEOF, NamePlotModes))

# Loop through EOF files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  File.out[,index]  <- EOFaux$blaux
  
  # plot each season
  limit <- max(abs(File.out[,index]))
  # check if newplot  if index == 1, or index divided by 5 is 0
  if (index == 1 || (index-1)%%20 == 0) {
    if (index > 1) {
      dev.off()
    }
    png(gsub(" ","",(paste("Anomaly_Seasonal_Shelf15_",NamePlot[((index-1)/16)+1],".png"))),res = 300,width = 2000, height = 1500)
    par(mfrow = c(4, 5), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
  }
  plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
  lines(yearsEOF, File.out[,index], lty=1, col=4)
  abline(h=0, col="gray70")
  # increase index
  index <- index +1
}
dev.off()
write.csv(File.out, file="Anomaly_Seasonal_Shelf15.csv", quote=F)

filedir <- "~/Stuff/Current/SAtlantic/data/CNAPS_Shelf_50/anomaly/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Bottom Temp" ,"Mixed Layer", "Salinity", "SSH", "SST")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(1993,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=20, dimnames=list(yearsEOF, NamePlotModes))

# Loop through EOF files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  File.out[,index]  <- EOFaux$blaux
  
  # plot each season
  limit <- max(abs(File.out[,index]))
  # check if newplot  if index == 1, or index divided by 5 is 0
  if (index == 1 || (index-1)%%20 == 0) {
    if (index > 1) {
      dev.off()
    }
    png(gsub(" ","",(paste("Anomaly_Seasonal_Shelf50_",NamePlot[((index-1)/16)+1],".png"))),res = 300,width = 2000, height = 1500)
    par(mfrow = c(4, 5), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
  }
  plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
  lines(yearsEOF, File.out[,index], lty=1, col=4)
  abline(h=0, col="gray70")
  # increase index
  index <- index +1
}
dev.off()
write.csv(File.out, file="Anomaly_Seasonal_Shelf50.csv", quote=F)

# ################################################################################
# # now read ALL Differential (4 cases)
# 
# # set the directory with the assessment files
# filedir <- "/Users/anacarolvaz/Stuff/Current/SAtlantic/data/Averages/ALLDiff"
# 
# # Get a list of all the files in the directory
# file_list <- list.files(filedir)
# 
# # it is in alphabetical order, define species define species names
# NamePlot <- c("Bottom Temp" ,"Mixed Layer", "Salinity", "SST")
# NameSeason <- c("JanApr", "JanMay", "FebApr", "FebMay")
# 
# # create combinations of names and combine the names
# aux <- expand.grid(NameSeason,NamePlot)
# NamePlotModes <- paste(aux$Var2, aux$Var1)
# 
# index <- 1
# #Graph the EOF series
# 
# yearsEOF <- seq(1998,2020)
# # create matrix for saving recdevs
# File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=16, dimnames=list(yearsEOF, NamePlotModes))
# 
# # Loop through EOF files
# for (inFile in file_list) {
#   # Construct the full file path
#   filename <- file.path(filedir, inFile)
#   EOFaux   <- readMat(filename)
#   EOFtime  <- EOFaux$ALLdifferential
#   # each file has 4 differencials
#   for (i in 1:4) {
#     # calculate the anomalies for each season
#     File.out[,index] <- EOFtime[,i]
#     # plot each season
#     limit <- max(abs(File.out[,index]))
#     # check if newplot  if index == 1, or index divided by 5 is 0
#     if (index == 1 || (index-1)%%16 == 0) {
#       if (index > 1) {
#         dev.off()
#       }
#       png(gsub(" ","",(paste("ALL_Diff_",NamePlot[((index-1)/16)+1],".png"))),res = 300, width = 2000, height = 2000)
#       par(mfrow = c(4, 4), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
#     }
#     plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
#     lines(yearsEOF, File.out[,index], lty=1, col=4)
#     abline(h=0, col="gray70")
#     # increase index
#     index <- index +1
#   }
# }
# dev.off()
# write.csv(File.out, file="ALL_Diff_feb23.csv", quote=F)