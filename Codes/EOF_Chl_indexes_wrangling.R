library(Hmisc)
library(tidyr)
library(R.matlab)

################################################################################
# now read all EOF Seasonal (PCA for time)

# set the diretory 
filedir <- "~/Stuff/Current/SAtlantic/data/EOF_CNAPS/ChSeason/"

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

png(gsub(" ","",(paste("./images/TimeSeries/EOF_Seasonal_Chl.png"))),res = 300,width = 2000, height = 2000)
par(mfrow = c(2, 4), mar=c(.2, .2, .2, .2), oma=c(0, 0, 0, 0), mai=c(.25, .25, .25, .25))

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
  }
}
dev.off()
write.csv(File.out, file="./csv_files/Chl_EOF_Seasonal.csv", quote=F)

################################################################################
# now read all EOF Spawning Season (Winter - Feb to April, Summer - Jun to Aug

# set the diretory with the assessment files
filedir <- "~/Stuff/Current/SAtlantic/data/EOF_CNAPS/ChSpSeason/"

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

png(gsub(" ","",(paste("../images/TimeSeries/EOF_Spawning_Seasonal_Chl.png"))),res = 300, width = 2000, height = 2000)
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

write.csv(File.out, file="Chl_EOF_SeasonalSp.csv", quote=F)