

fluidPage(column(width = 3,
                 box(title = tagList(icon("dna"), "Enter Parameters for the PCA plot"),
                     width = NULL,
                     solidHeader = TRUE,
                     status = "primary",
                     footer = "press make pca plot to generate the new figure",
                     radioButtons(inputId = "NZM_PCAplot_batch", 
                                        label = "select batch", 
                                        choices = list("batch1" = "NZM_batch1_samples", "batch2" = "NZM_batch2_samples", "both batches" = "NZM_batch1and2_samples"), 
                                        selected = "NZM_batch1_samples"),
                     conditionalPanel(condition = "input.NZM_PCAplot_batch=='NZM_batch1_samples'",
                                      selectizeInput(inputId = "NZM_PCAplot_PCx_b1",label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "NZM_PCAplot_PCy_b1",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g1_b1",label = "Select your samples for group1", choices= NZM_batch1_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g2_b1",label = "Select your samples for group2", choices= NZM_batch1_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g3_b1",label = "Select your samples for group3", choices= NZM_batch1_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g4_b1",label = "Select your samples for group4", choices= NZM_batch1_samples, multiple=TRUE),
                                      hr(),
                                      helpText("Top genes are selected according to the genes with the highest variance after filtering out for genes with zero variance"),
                                      numericInput(inputId = "NZM_PCAplot_topgenes_b1",label = "Number of top genes",value = 100, min = 0, step = 1),
                                      materialSwitch(inputId = "NZM_PCAplot_scale_b1",label = "Scale",value = TRUE,right = TRUE,status = "primary"),
                                      materialSwitch(inputId = "NZM_PCAplot_centre_b1",label = "Centre",value = TRUE,right = TRUE,status = "primary"),
                                      do.call(actionBttn, c(list(inputId = "NZM_make_PCAplot_b1", label = "make plot", icon = icon("play")), actionBttnParams))),
                     conditionalPanel(condition = "input.NZM_PCAplot_batch=='NZM_batch2_samples'",
                                      selectizeInput(inputId = "NZM_PCAplot_PCx_b2", label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "NZM_PCAplot_PCy_b2",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g1_b2",label = "Select your samples for group1", choices= NZM_batch2_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g2_b2",label = "Select your samples for group2", choices= NZM_batch2_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g3_b2",label = "Select your samples for group3", choices= NZM_batch2_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g4_b2",label = "Select your samples for group4", choices= NZM_batch2_samples, multiple=TRUE),
                                      hr(),
                                      helpText("Top genes are selected according to the genes with the highest variance after filtering out for genes with zero variance"),
                                      numericInput(inputId = "NZM_PCAplot_topgenes_b2",label = "Number of top genes",value = 100, min = 0, step = 1),
                                      materialSwitch(inputId = "NZM_PCAplot_scale_b2",label = "Scale",value = TRUE,right = TRUE,status = "primary"),
                                      materialSwitch(inputId = "NZM_PCAplot_centre_b2",label = "Centre",value = TRUE,right = TRUE,status = "primary"),
                                      do.call(actionBttn, c(list(inputId = "NZM_make_PCAplot_b2", label = "make plot", icon = icon("play")), actionBttnParams))),
                    conditionalPanel(condition = "input.NZM_PCAplot_batch=='NZM_batch1and2_samples'",
                                      selectizeInput(inputId = "NZM_PCAplot_PCx_b1and2", label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "NZM_PCAplot_PCy_b1and2",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                     selectizeInput(inputId = "NZM_PCAplot_samples_g1_b1and2",label = "Select your samples for group1", choices= NZM_batch1and2_samples, multiple=TRUE),
                                     selectizeInput(inputId = "NZM_PCAplot_samples_g2_b1and2",label = "Select your samples for group2", choices= NZM_batch1and2_samples, multiple=TRUE),
                                     selectizeInput(inputId = "NZM_PCAplot_samples_g3_b1and2",label = "Select your samples for group3", choices= NZM_batch1and2_samples, multiple=TRUE),
                                     selectizeInput(inputId = "NZM_PCAplot_samples_g4_b1and2",label = "Select your samples for group4", choices= NZM_batch1and2_samples, multiple=TRUE),
                                     hr(),
                                     helpText("Top genes are selected according to the genes with the highest variance after filtering out for genes with zero variance"),
                                     numericInput(inputId = "NZM_PCAplot_topgenes_b1and2",label = "Number of top genes",value = 100, min = 0, step = 1),
                                     materialSwitch(inputId = "NZM_PCAplot_scale_b1and2",label = "Scale",value = TRUE,right = TRUE,status = "primary"),
                                     materialSwitch(inputId = "NZM_PCAplot_centre_b1and2",label = "Centre",value = TRUE,right = TRUE,status = "primary"),
                                     do.call(actionBttn, c(list(inputId = "NZM_make_PCAplot_b1and2", label = "make plot", icon = icon("play")), actionBttnParams))),
                 )
                 #downloadButton(outputId = "NZM_download_PCAplot_data",label = "Download data")
                 ),
          column(width = 9,
                 tabBox(title = "", 
                        width = NULL,
                   tabPanel(
                     title = "PCA plot (batch1)",
                     status = "primary",
                     solidHeader = TRUE,
                     width = NULL,
                     plotly::plotlyOutput("NZM_plotly_PCAplot_b1_out") %>% withSpinner()
                     #DT::DTOutput("NZM_DT_PCA_batch1_out")
                     #plotly::plotlyOutput("NZM_plotly_PCAplot_batch1and2_out")
                     ),
                   tabPanel(
                     title = "PCA plot (batch2)",
                     status = "primary",
                     solidHeader = TRUE,
                     width = NULL,
                     plotly::plotlyOutput("NZM_plotly_PCAplot_b2_out") %>% withSpinner()
                     ),
                   tabPanel(
                     title = "PCA plot (batch1 and batch2)",
                     status = "primary",
                     solidHeader = TRUE,
                     width = NULL,
                     plotly::plotlyOutput("NZM_plotly_PCAplot_b1and2_out") %>% withSpinner()
                     #DT::DTOutput("NZM_DT_PCA_batch1_out")
                     #plotly::plotlyOutput("NZM_plotly_PCAplot_batch1and2_out")
                     )
                   )
                 )
          )




                     
                     
                     
                     
                     
                     