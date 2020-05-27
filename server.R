source(file = "global.R",
       local = TRUE,
       encoding = "UTF-8")


shinyServer(function(input, output, session){
  # NZM RNAseq data
    ## make boxplot
    source(file = "server-NZM-boxplot.R",
           local = TRUE,
           encoding = "UTF-8")
    ## make corplot
   source(file = "server-NZM-corplot.R",
         local = TRUE,
         encoding = "UTF-8")
  ## make PCAplot
 source(file = "server-NZM-PCA.R",
        local = TRUE,
        encoding = "UTF-8")
  ## make hclust 
  source(file = "server-NZM-hclust.R",
         local = TRUE,
         encoding = "UTF-8")
  
## ----------------  
# PDL1 RNAseq data
 #make boxplot
  source(file = "server-PDL1-boxplot.R",
         local = TRUE,
         encoding = "UTF-8")
# make corplot
  source(file = "server-PDL1-corplot.R",
         local = TRUE,
         encoding = "UTF-8")
# make PCA
  source(file = "server-PDL1-PCA.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-PDL1-hclust.R",
         local = TRUE,
         encoding = "UTF-8")
## ----------------  
# DAC VitC RNAseq data
#make boxplot
  source(file = "server-DACVitC-boxplot.R",
         local = TRUE,
         encoding = "UTF-8")
  
})

