make_markdown_correlation
================

# Create markdown for CNAPS_no trend

``` r
# Names of Variables to plot
NamePlot   <- c("Bottom Temperature","Salinity","SSH","SST","Mixed Layer")
NameSeason <- c("Winter","Spring","Summer","Fall")
NameSeasonSp <- c("Winter","Summer")
NameModes  <- c("Mode 1","Mode 2")

myname <- c("bottomT","Salinity","ssh","sst","mixedlayer")
mynameUp <- c("BottomT","Salinity","SSH","SST","MixedLayer")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModesSeason <- paste(aux$Var3, aux$Var2, aux$Var1)

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeasonSp,NamePlot)
NamePlotModesSpawnSeason <- paste(aux$Var3, aux$Var2, aux$Var1)

# create combinations of names and combine the names
aux <- expand.grid(NamePlot, NameModes)
NamePlotModes <- paste(aux$Var1, aux$Var2)

spNamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish"," Red Porgy","Red Grouper","Black Sea Bass","Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")

NameIndexes <- c("AMO","MEI","NAO")

cat("# Correlation Heatmap \n\n")
```

    # Correlation Heatmap 

``` r
filedir <- "../images/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)
file_plot <- file_list[grep("^Correlation.*\\.png$",file_list)]
 
for (i in seq_along(file_plot)) {
  cat(paste0('![](', filedir,file_plot[i], ')\n\n'),sep="")
}
```

    ![](../images/Correlation_EOF_Season_Species_heatmap.png)

    ![](../images/Correlation_EOF_SpSeason_Species_heatmap.png)

    ![](../images/Correlation_index_EOF_heatmap.png)

``` r
cat("# Correlations EOF Season \n\n")
```

    # Correlations EOF Season 

``` r
filedir <- "../images/CorrelationEOFSeason/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

for (iSp in seq_along(spNamePlot)) {
  cat(paste0("## ", spNamePlot[iSp], "\n\n"))
  
  for (iVar in seq_along(NamePlot)) {
    cat(paste0("### ", NamePlot[iVar], "\n\n"))
  
    cat("::: {layout-ncol=\"2\"}\n\n")
      
    file_plot <- file_list[grep(gsub(" ","",paste0("^",spNamePlot[iSp], 
                                                      mynameUp[iVar],
                                                      ".*\\.png$")),file_list)]
    if (length(file_plot) > 0) {
      for (iplot in 1:length(file_plot)){
        fig_name <- gsub(" ","",paste0(filedir, file_plot[iplot]))
        cat(paste0('![](', fig_name, ')\n\n'),sep="")
      }
    }
  }
}
```

    ## Gag Grouper

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GagGrouperBottomTFallMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperBottomTFallMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperBottomTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperBottomTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GagGrouperSalinityFallMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSalinityFallMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSalinitySpringMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSalinitySpringMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GagGrouperSSHFallMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSHFallMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSHSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSHSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GagGrouperSSTFallMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSTFallMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GagGrouperMixedLayerFallMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperMixedLayerFallMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperMixedLayerSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperMixedLayerSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GagGrouperMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GagGrouperMixedLayerWinterMode2.png)

    ## Greater Amberjack

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GreaterAmberjackBottomTFallMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackBottomTFallMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackBottomTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackBottomTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSalinityFallMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSalinityFallMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSalinitySpringMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSalinitySpringMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSHFallMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSHFallMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSHSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSHSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSTFallMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSTFallMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GreaterAmberjackMixedLayerFallMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackMixedLayerFallMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackMixedLayerSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackMixedLayerSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GreaterAmberjackMixedLayerWinterMode2.png)

    ## Gray Triggerfish

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GrayTriggerfishBottomTFallMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishBottomTFallMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishBottomTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishBottomTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSalinityFallMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSalinityFallMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSalinitySpringMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSalinitySpringMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSHFallMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSHFallMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSHSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSHSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSTFallMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSTFallMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/GrayTriggerfishMixedLayerFallMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishMixedLayerFallMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishMixedLayerSpringMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishMixedLayerSpringMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeason/GrayTriggerfishMixedLayerWinterMode2.png)

    ##  Red Porgy

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedPorgyBottomTFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgyBottomTFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgyBottomTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgyBottomTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgyBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgyBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgyBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgyBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedPorgySalinityFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySalinityFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgySalinitySpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySalinitySpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgySalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgySalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedPorgySSHFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSHFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSHSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSHSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSHSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSHSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSHWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedPorgySSTFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSTFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgySSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedPorgyMixedLayerFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgyMixedLayerFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgyMixedLayerSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgyMixedLayerSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgyMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgyMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedPorgyMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedPorgyMixedLayerWinterMode2.png)

    ## Red Grouper

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedGrouperBottomTFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperBottomTFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperBottomTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperBottomTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedGrouperSalinityFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSalinityFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSalinitySpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSalinitySpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedGrouperSSHFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSHFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSHSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSHSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedGrouperSSTFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSTFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedGrouperMixedLayerFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperMixedLayerFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperMixedLayerSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperMixedLayerSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedGrouperMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedGrouperMixedLayerWinterMode2.png)

    ## Black Sea Bass

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/BlackSeaBassBottomTFallMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassBottomTFallMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassBottomTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassBottomTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/BlackSeaBassSalinityFallMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSalinityFallMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSalinitySpringMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSalinitySpringMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSHFallMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSHFallMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSHSpringMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSHSpringMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSTFallMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSTFallMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/BlackSeaBassMixedLayerFallMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassMixedLayerFallMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassMixedLayerSpringMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassMixedLayerSpringMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeason/BlackSeaBassMixedLayerWinterMode2.png)

    ## Red Snapper

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedSnapperBottomTFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperBottomTFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperBottomTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperBottomTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedSnapperSalinityFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSalinityFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSalinitySpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSalinitySpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedSnapperSSHFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSHFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSHSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSHSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedSnapperSSTFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSTFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/RedSnapperMixedLayerFallMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperMixedLayerFallMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperMixedLayerSpringMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperMixedLayerSpringMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeason/RedSnapperMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeason/RedSnapperMixedLayerWinterMode2.png)

    ## Scamp

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/ScampBottomTFallMode1.png)

    ![](../images/CorrelationEOFSeason/ScampBottomTFallMode2.png)

    ![](../images/CorrelationEOFSeason/ScampBottomTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/ScampBottomTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/ScampBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/ScampBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/ScampBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/ScampBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/ScampSalinityFallMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSalinityFallMode2.png)

    ![](../images/CorrelationEOFSeason/ScampSalinitySpringMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSalinitySpringMode2.png)

    ![](../images/CorrelationEOFSeason/ScampSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeason/ScampSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/ScampSSHFallMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSSHFallMode2.png)

    ![](../images/CorrelationEOFSeason/ScampSSHSpringMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSSHSpringMode2.png)

    ![](../images/CorrelationEOFSeason/ScampSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeason/ScampSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/ScampSSTFallMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSSTFallMode2.png)

    ![](../images/CorrelationEOFSeason/ScampSSTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSSTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/ScampSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/ScampSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/ScampSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/ScampMixedLayerFallMode1.png)

    ![](../images/CorrelationEOFSeason/ScampMixedLayerFallMode2.png)

    ![](../images/CorrelationEOFSeason/ScampMixedLayerSpringMode1.png)

    ![](../images/CorrelationEOFSeason/ScampMixedLayerSpringMode2.png)

    ![](../images/CorrelationEOFSeason/ScampMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeason/ScampMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeason/ScampMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeason/ScampMixedLayerWinterMode2.png)

    ## Snowy Grouper 

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/SnowyGrouperBottomTFallMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperBottomTFallMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperBottomTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperBottomTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/SnowyGrouperSalinityFallMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSalinityFallMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSalinitySpringMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSalinitySpringMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSHFallMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSHFallMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSHSpringMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSHSpringMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSTFallMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSTFallMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/SnowyGrouperMixedLayerFallMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperMixedLayerFallMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperMixedLayerSpringMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperMixedLayerSpringMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeason/SnowyGrouperMixedLayerWinterMode2.png)

    ## Vermilion Snapper

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/VermilionSnapperBottomTFallMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperBottomTFallMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperBottomTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperBottomTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/VermilionSnapperSalinityFallMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSalinityFallMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSalinitySpringMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSalinitySpringMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSHFallMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSHFallMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSHSpringMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSHSpringMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSTFallMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSTFallMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSTSpringMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSTSpringMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeason/VermilionSnapperMixedLayerFallMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperMixedLayerFallMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperMixedLayerSpringMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperMixedLayerSpringMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeason/VermilionSnapperMixedLayerWinterMode2.png)

``` r
cat("::: \n\n")
```

    ::: 

``` r
cat("# Correlations EOF Season Spawning \n\n")
```

    # Correlations EOF Season Spawning 

``` r
filedir <- "../images/CorrelationEOFSeasonSp/"

# Get a list of all the files in the directory
file_list <- list.files(filedir)

for (iSp in seq_along(spNamePlot)) {
  cat(paste0("## ", spNamePlot[iSp], "\n\n"))
  
  for (iVar in seq_along(NamePlot)) {
    cat(paste0("### ", NamePlot[iVar], "\n\n"))
  
    cat("::: {layout-ncol=\"2\"}\n\n")
      
    file_plot <- file_list[grep(gsub(" ","",paste0("^",spNamePlot[iSp], 
                                                      mynameUp[iVar],
                                                      ".*\\.png$")),file_list)]
    if (length(file_plot) > 0) {
      for (iplot in 1:length(file_plot)){
        fig_name <- gsub(" ","",paste0(filedir, file_plot[iplot]))
        cat(paste0('![](', fig_name, ')\n\n'),sep="")
      }
    }
  }
}
```

    ## Gag Grouper

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GagGrouperBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GagGrouperMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GagGrouperMixedLayerWinterMode2.png)

    ## Greater Amberjack

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GreaterAmberjackMixedLayerWinterMode2.png)

    ## Gray Triggerfish

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/GrayTriggerfishMixedLayerWinterMode2.png)

    ##  Red Porgy

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedPorgyBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgyBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgyBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgyBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedPorgySalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgySalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgySalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgySalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedPorgySSHSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgySSHSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgySSHWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgySSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedPorgySSTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgySSTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgySSTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgySSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedPorgyMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgyMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgyMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedPorgyMixedLayerWinterMode2.png)

    ## Red Grouper

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedGrouperBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedGrouperMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedGrouperMixedLayerWinterMode2.png)

    ## Black Sea Bass

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/BlackSeaBassMixedLayerWinterMode2.png)

    ## Red Snapper

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedSnapperBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/RedSnapperMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/RedSnapperMixedLayerWinterMode2.png)

    ## Scamp

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/ScampBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/ScampBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/ScampBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/ScampBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/ScampSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/ScampSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/ScampSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/ScampSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/ScampSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/ScampSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/ScampSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/ScampSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/ScampSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/ScampSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/ScampSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/ScampSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/ScampMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/ScampMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/ScampMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/ScampMixedLayerWinterMode2.png)

    ## Snowy Grouper 

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/SnowyGrouperMixedLayerWinterMode2.png)

    ## Vermilion Snapper

    ### Bottom Temperature

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperBottomTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperBottomTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperBottomTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperBottomTWinterMode2.png)

    ### Salinity

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSalinitySummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSalinitySummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSalinityWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSalinityWinterMode2.png)

    ### SSH

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSSHSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSSHSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSSHWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSSHWinterMode2.png)

    ### SST

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSSTSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSSTSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSSTWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperSSTWinterMode2.png)

    ### Mixed Layer

    ::: {layout-ncol="2"}

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperMixedLayerSummerMode1.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperMixedLayerSummerMode2.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperMixedLayerWinterMode1.png)

    ![](../images/CorrelationEOFSeasonSp/VermilionSnapperMixedLayerWinterMode2.png)

``` r
cat("::: \n\n")
```

    ::: 
