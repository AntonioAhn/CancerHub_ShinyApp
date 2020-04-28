library(shiny)
library(tidyverse)
library(plotly)
library(ggpubr)
library(DT)
library(shinyWidgets)
library(ggsci)



##-------------------------------------------------------------



load(file = "data/NZM_PDL1_data.RData")




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

