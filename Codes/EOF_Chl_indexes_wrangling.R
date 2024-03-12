library(Hmisc)
library(tidyr)
library(R.matlab)

################################################################################
# now read all EOF Seasonal (PCA for time)

# set the diretory 
filedir <- "~/Stuff/Current/SAtlantic/data/ChlShelf15/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

NamePlot <- c("Chl")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")
NameModes <- c("Mode 1","Mode 2")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var3, aux$Var2, aux$Var1)

yearsEOF <- seq(1998,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=8, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("../images/TimeSeries/EOF_Seasonal_Chl_Shelf15.png"))),res = 300,width = 2000, height = 2000)
par(mfrow = c(2, 4), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))

index <- 1
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
    plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
    lines(yearsEOF, EOFtime[,i], lty=1, col=4)
    abline(h=0, col="gray70")
    index <- index + 1
  }
}
dev.off()
write.csv(File.out, file="Chl_EOF_Seasonal_Shelf15.csv", quote=F)

################################################################################
# now read all EOF Spawning Season (Winter - Feb to April, Summer - Jun to Aug

# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/ChlShelfSp15/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Chl")
NameSeason <- c("Summer", "Winter")
NameModes <- c("Mode 1","Mode 2")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var3, aux$Var2, aux$Var1)

yearsEOF <- seq(1998,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=4, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("../images/TimeSeries/EOF_Spawning_Seasonal_Chl_Shelf15.png"))),res = 300, width = 2000, height = 2000)
par(mfrow = c(2, 2), mar=c(.1, .1, .1, .1), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
index <- 1
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
    plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
    lines(yearsEOF, EOFtime[,i], lty=1, col=4)
    abline(h=0, col="gray70")
    index <- index + 1
  }
 
}
dev.off()

write.csv(File.out, file="Chl_EOF_SeasonalSp_Shelf15.csv", quote=F)

################################################################################
# now read all EOF Seasonal (PCA for time)

# set the diretory 
filedir <- "~/Stuff/Current/SAtlantic/data/ChlShelf50/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

NamePlot <- c("Chl")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")
NameModes <- c("Mode 1","Mode 2")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var3, aux$Var2, aux$Var1)

yearsEOF <- seq(1998,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=8, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("../images/TimeSeries/EOF_Seasonal_Chl_Shelf50.png"))),res = 300,width = 2000, height = 2000)
par(mfrow = c(2, 4), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))

index <- 1
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
    plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
    lines(yearsEOF, EOFtime[,i], lty=1, col=4)
    abline(h=0, col="gray70")
    index <- index + 1
  }
}
dev.off()
write.csv(File.out, file="Chl_EOF_Seasonal_Shelf50.csv", quote=F)

################################################################################
# now read all EOF Spawning Season (Winter - Feb to April, Summer - Jun to Aug

# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/ChlShelfSp50/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Chl")
NameSeason <- c("Summer", "Winter")
NameModes <- c("Mode 1","Mode 2")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var3, aux$Var2, aux$Var1)

yearsEOF <- seq(1998,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=4, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("../images/TimeSeries/EOF_Spawning_Seasonal_Chl_Shelf50.png"))),res = 300, width = 2000, height = 2000)
par(mfrow = c(2, 2), mar=c(.1, .1, .1, .1), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
index <- 1
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
    plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
    lines(yearsEOF, EOFtime[,i], lty=1, col=4)
    abline(h=0, col="gray70")
    index <- index + 1
  }
  
}
dev.off()

write.csv(File.out, file="Chl_EOF_SeasonalSp_Shelf50.csv", quote=F)

################################################################################
# now read all EOF Seasonal (PCA for time)

# set the diretory 
filedir <- "~/Stuff/Current/SAtlantic/data/ChlModis15/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

NamePlot <- c("Chl")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")
NameModes <- c("Mode 1","Mode 2")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var3, aux$Var2, aux$Var1)

yearsEOF <- seq(2003,2022)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=8, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("../images/TimeSeries/EOF_Seasonal_Chl_Modis_Shelf15.png"))),res = 300,width = 2000, height = 2000)
par(mfrow = c(2, 4), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))

index <- 1
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
    plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
    lines(yearsEOF, EOFtime[,i], lty=1, col=4)
    abline(h=0, col="gray70")
    index <- index + 1
  }
}
dev.off()
write.csv(File.out, file="Chl_EOF_Seasonal_Modis_Shelf15.csv", quote=F)

################################################################################
# now read all EOF Seasonal (PCA for time)

# set the diretory 
filedir <- "~/Stuff/Current/SAtlantic/data/ChlModis50/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

NamePlot <- c("Chl")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")
NameModes <- c("Mode 1","Mode 2")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var3, aux$Var2, aux$Var1)

yearsEOF <- seq(2003,2022)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=8, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("../images/TimeSeries/EOF_Seasonal_Chl_Modis_Shelf50.png"))),res = 300,width = 2000, height = 2000)
par(mfrow = c(2, 4), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))

index <- 1
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
    plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
    lines(yearsEOF, EOFtime[,i], lty=1, col=4)
    abline(h=0, col="gray70")
    index <- index + 1
  }
}
dev.off()
write.csv(File.out, file="Chl_EOF_Seasonal_Modis_Shelf50.csv", quote=F)

################################################################################
# now read anomaly Seasonal (avg over time)
# 
# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/Anomaly/ChlModis15/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Chl")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(2003,2022)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=4, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("Anomaly_Seasonal_ChlModis15.png"))),res = 300,width = 2000, height = 1500)
par(mfrow = c(2, 2), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
# Loop through anomaly files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  File.out[,index]  <- EOFaux$blaux
  
  # plot each season
  limit <- max(abs(File.out[,index]))
  plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
  lines(yearsEOF, File.out[,index], lty=1, col=4)
  abline(h=0, col="gray70")
  # increase index
  index <- index +1
}
dev.off()

write.csv(File.out, file="Anomaly_Seasonal_ChlModis15.csv", quote=F)

# ################################################################################
filedir <- "~/Stuff/Current/SAtlantic/data/Anomaly/ChlModis50/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Chl")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(2003,2022)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=4, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("Anomaly_Seasonal_ChlModis50.png"))),res = 300,width = 2000, height = 1500)
par(mfrow = c(2, 2), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
# Loop through anomaly files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  File.out[,index]  <- EOFaux$blaux
  
  # plot each season
  limit <- max(abs(File.out[,index]))
  plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
  lines(yearsEOF, File.out[,index], lty=1, col=4)
  abline(h=0, col="gray70")
  # increase index
  index <- index +1
}
dev.off()

write.csv(File.out, file="Anomaly_Seasonal_ChlModis50.csv", quote=F)

################################################################################
# now read anomaly Seasonal (avg over time)
# for GlbColour
# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/Anomaly/ChlShelf15/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Chl")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(1998,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=4, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("Anomaly_Seasonal_ChlShelf15.png"))),res = 300,width = 2000, height = 1500)
par(mfrow = c(2, 2), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
# Loop through anomaly files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  File.out[,index]  <- EOFaux$blaux
  
  # plot each season
  limit <- max(abs(File.out[,index]))
  plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
  lines(yearsEOF, File.out[,index], lty=1, col=4)
  abline(h=0, col="gray70")
  # increase index
  index <- index +1
}
dev.off()

write.csv(File.out, file="Anomaly_Seasonal_ChlShelf15.csv", quote=F)

# ################################################################################
filedir <- "~/Stuff/Current/SAtlantic/data/Anomaly/ChlShelf50/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Chl")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(1998,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=4, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("Anomaly_Seasonal_ChlShelf50.png"))),res = 300,width = 2000, height = 1500)
par(mfrow = c(2, 2), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
# Loop through anomaly files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  File.out[,index]  <- EOFaux$blaux
  
  # plot each season
  limit <- max(abs(File.out[,index]))
  plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
  lines(yearsEOF, File.out[,index], lty=1, col=4)
  abline(h=0, col="gray70")
  # increase index
  index <- index +1
}
dev.off()
write.csv(File.out, file="Anomaly_Seasonal_ChlShelf50.csv", quote=F)

################################################################################
# now read anomaly Seasonal (avg over time)
#  for GlbColour
# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/Anomaly/ChlSpShelf15/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Chl")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(1998,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=4, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("Anomaly_Seasonal_ChlSpShelf15.png"))),res = 300,width = 2000, height = 1500)
par(mfrow = c(2, 2), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
# Loop through anomaly files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  File.out[,index]  <- EOFaux$blaux
  
  # plot each season
  limit <- max(abs(File.out[,index]))
  plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
  lines(yearsEOF, File.out[,index], lty=1, col=4)
  abline(h=0, col="gray70")
  # increase index
  index <- index +1
}
dev.off()

write.csv(File.out, file="Anomaly_Seasonal_ChlSpShelf15.csv", quote=F)

# ################################################################################
filedir <- "~/Stuff/Current/SAtlantic/data/Anomaly/ChlSpShelf50/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

# it is in alphabetical order, define species define species names
NamePlot <- c("Chl")
NameSeason <- c("Fall", "Spring", "Summer", "Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModes <- paste(aux$Var2, aux$Var1)

index <- 1
#Graph the EOF series

yearsEOF <- seq(1998,2021)
# create matrix for saving recdevs
File.out <- matrix(data=NA, nrow=length(yearsEOF), ncol=4, dimnames=list(yearsEOF, NamePlotModes))

png(gsub(" ","",(paste("Anomaly_Seasonal_ChlSpShelf50.png"))),res = 300,width = 2000, height = 1500)
par(mfrow = c(2, 2), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))
# Loop through anomaly files
for (inFile in file_list) {
  # Construct the full file path
  filename <- file.path(filedir, inFile)
  EOFaux   <- readMat(filename)
  File.out[,index]  <- EOFaux$blaux
  
  # plot each season
  limit <- max(abs(File.out[,index]))
  plot(yearsEOF, rep(0,length(yearsEOF)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = NamePlotModes[index])
  lines(yearsEOF, File.out[,index], lty=1, col=4)
  abline(h=0, col="gray70")
  # increase index
  index <- index +1
}
dev.off()
write.csv(File.out, file="Anomaly_Seasonal_ChlSpShelf50.csv", quote=F)