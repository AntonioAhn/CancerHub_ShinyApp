source(file = "global.R",
       local = TRUE,
       encoding = "UTF-8")


shinyServer(function(input, output, session){
    # make boxplot
    source(file = "server-NZM-boxplot.R",
           local = TRUE,
           encoding = "UTF-8")
    # make corplot
   source(file = "server-NZM-corplot.R",
         local = TRUE,
          encoding = "UTF-8")
  # make PCAplot
 source(file = "server-NZM-PCA.R",
        local = TRUE,
        encoding = "UTF-8")
  source(file = "server-NZM-hclust.R",
         local = TRUE,
         encoding = "UTF-8")
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
    #render corplot
    # output$corplot <- plotly::renderPlotly(gg_corplot())
  #  output$corplot <- renderPlot(gg_corplot())
})

