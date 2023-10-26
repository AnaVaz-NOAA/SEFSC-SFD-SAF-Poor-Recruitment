library(envPred)
library(Hmisc)
library(tidyr)
library(gplots)
library(ggplot2)
library(RColorBrewer)
library(cmocean)
# for a readmat function that works with HDF5
library(raveio)
library(R.matlab)
library(mapdata)
library(marmap)

load("/Users/anavaz/Stuff/Current/SAtlantic/data/world_map.RData") 
# the object is called "maps" 
head(maps)

refaux <- read.csv("./csv_files/ssh_analyses.csv")
#load bathymetry
bathy <- getNOAA.bathy(min(refaux$Lon)-1,max(refaux$Lon)+1,min(refaux$Lat)-1,max(refaux$Lat)+1,resolution=1)
rm(refaux)
bathy_df <- fortify(bathy)
rm(bathy)

filepath <- ("/Users/anavaz/Stuff/Current/SAtlantic/data/")
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

col_names <- c("Lat","Lon","Avg1",  "Avg2",  "Avg3",  "Avg4",
                           "Std1",  "Std2",  "Std3",  "Std4",
                           "Trend1","Trend2","Trend3","Trend4")

for (ivar in 1:4) {
  varname <- switch(ivar,
                    'ssh',
                    'bottomT',
                    'sst',
                    'mixedlayer')
  filename <- (gsub(" ","",paste(filepath,varname,"_CNAPS.mat")))
  VarMaux  <- read_mat(filename)
  VarM     <- VarMaux$varM
  rm(VarMaux)
  
  row_names <- 1:ncol(VarM)
  my_df <- data.frame(matrix(NA, nrow = ncol(VarM), ncol = length(col_names)))
  colnames(my_df) <- col_names
  rownames(my_df) <- row_names
  my_df[, "Lat"] <- latlon[,1]
  my_df[, "Lon"] <- latlon[,2]
  
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
    VarMProfile <- VarM[QuartIndex, ]
    
    for (i in seq_len(ncol(VarMProfile))) {
      varPos  <- VarMProfile[,i]
      # do avg and std
      my_df[i, gsub(" ","",paste("Avg",iQ))] <- mean(varPos)
      my_df[i, gsub(" ","",paste("Std",iQ))] <- stdM <- sd(varPos)
      #
      model <- lm(varPos ~ seq(1,length(varPos)))
      model_summary <- summary(model)
      
      # Extract the p-value for the slope coefficient
      p_value <- model_summary$coefficients["seq(1, length(varPos))", "Pr(>|t|)"]
      # Check if the p-value is less than your chosen significance level (e.g., 0.05)
      if (p_value < 0.05) {
        # slope of the line is given by coef(model)[2]
        # only save if significant 
        my_df[i, gsub(" ","",paste("Trend",iQ))] <- coef(model)[2]
      } else {
        my_df[i, gsub(" ","",paste("Trend",iQ))] <- NaN
      }
    }  
  }
  # save the first part of the analyses for segments
  write.csv(my_df, file = gsub(" ","",paste("./csv_files/",varname,"_seasonal.csv")), row.names = FALSE)
}

col_names <- c("Lat", "Lon", "Avg", "Std", "Trend")

# plot now in space each one
# load the file for each variable, then rotate to plot each column (analyes results)
for (ivar in 1:4) {
  varname <- switch(ivar,
                    'bottomT',
                    'ssh',
                    'sst',
                    'mixedlayer')
  varnamePretty <- switch(ivar,
                          'Bottom Temperature',
                          'SSH',
                          'SST',
                          'Mixed Layer')
  
  filename <- (gsub(" ","",paste("./csv_files/",varname,"_seasonal.csv")))
  ref  <- read.csv(filename)
  
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
    
    # we have 3 different analyses  
    for (i in 3:5) {
      PlotName <- gsub(" ","",paste(col_names[i],iQ))
      # create a plot
      pl <- ggplot() + # Declaration of local variables for the plot
        geom_polygon(data=maps, aes(x=lon, y=lat),fill = "grey70") +
        coord_quickmap(expand=FALSE,xlim = c(min(ref$Lon)-1, max(ref$Lon)), ylim = c(min(ref$Lat),max(ref$Lat))) +
        geom_point(data=ref, aes(x = Lon, y=Lat, color=!!sym(PlotName))) +
        geom_contour(data = bathy_df, aes(x = x, y = y, z = z), 
                     breaks = c(-50, -400, -700), colour = "grey70", linewidth = 0.5) +
        labs(x = "Longitude",y = "Latitude", title = varnamePretty) +
        theme(text = element_text(size = 20)) +
        theme_light()
        if (i != 5){
          pl + scale_colour_viridis_c()
        }
        else{
          pl + scale_colour_gradient2(
          low = "dodgerblue4", mid = "white", high = "firebrick4",
          midpoint = 0)}
      ggsave(gsub(" ","",paste("./images/analyses/",varname,"_",qname,"_",col_names[i],"_seasonal.png")),device = "png")
    }
  }
}