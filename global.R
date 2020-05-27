library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)
library(ggsci)
library(shinycssloaders)
library(shinyHeatmaply)
library(heatmaply)
library(ggplot2)
library(ggpubr)
library(ggrepel)



##-------------------------------------------------------------



load(file = "data/NZM_PDL1_data.RData")

load(file = "data/PDL1_DACVitC_data.RData")


##-------------------------------------------------------------

# parameters for actionBttn
actionBttnParams <- list(
  size = "sm",
  color = "primary",
  style = "fill",
  block = TRUE
)

##------------------------------------------------------------- 

# add colors 
fut_color <- ggsci::pal_futurama()(12)
starT_color <- ggsci::pal_startrek("uniform")(7)

