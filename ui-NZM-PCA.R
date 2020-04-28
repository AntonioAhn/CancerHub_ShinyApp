

fluidPage(column(width = 3,
                 box(title = tagList(icon("dna"), "Enter Parameters for the PCA plot"),
                     width = NULL,
                     solidHeader = TRUE,
                     status = "danger",
                     footer = "press make pca plot to generate the new figure",
                     radioButtons(inputId = "NZM_PCAplot_batch", 
                                        label = "select batch", 
                                        choices = list("batch1" = "batch1_samples", "batch2" = "batch2_samples", "both batches" = "NZM_batch1and2_samples"), 
                                        selected = "batch1"),
                     #conditionalPanel(condition = "input.NZM_PCAplot_batch == 'batch1_samples' || input.NZM_PCAplot_batch == 'batch2_samples'",
                     #selectizeInput(inputId = "NZM_PCAplot_PCx",label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE),
                     #selectizeInput(inputId = "NZM_PCAplot_PCy",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE),
                     #selectizeInput(inputId = "NZM_PCAplot_samples_batch",label = "Select your samples", choices= c(batch1_samples, batch2_samples), multiple=TRUE)),
                     conditionalPanel(condition = "input.NZM_PCAplot_batch=='batch1_samples'",
                                      selectizeInput(inputId = "NZM_PCAplot_PCx_b1",label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "NZM_PCAplot_PCy_b1",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g1_b1",label = "Select your samples for group1", choices= batch1_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g2_b1",label = "Select your samples for group2", choices= batch1_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g3_b1",label = "Select your samples for group3", choices= batch1_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g4_b1",label = "Select your samples for group4", choices= batch1_samples, multiple=TRUE),
                                      do.call(actionBttn, c(list(inputId = "NZM_make_PCAplot_b1", label = "make plot", icon = icon("play")), actionBttnParams))),
                     conditionalPanel(condition = "input.NZM_PCAplot_batch=='batch2_samples'",
                                      selectizeInput(inputId = "NZM_PCAplot_PCx_b2", label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "NZM_PCAplot_PCy_b2",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g1_b2",label = "Select your samples for group1", choices= batch2_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g2_b2",label = "Select your samples for group2", choices= batch2_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g3_b2",label = "Select your samples for group3", choices= batch2_samples, multiple=TRUE),
                                      selectizeInput(inputId = "NZM_PCAplot_samples_g4_b2",label = "Select your samples for group4", choices= batch2_samples, multiple=TRUE),
                                      do.call(actionBttn, c(list(inputId = "NZM_make_PCAplot_b2", label = "make plot", icon = icon("play")), actionBttnParams))),
                    conditionalPanel(condition = "input.NZM_PCAplot_batch=='NZM_batch1and2_samples'",
                                      selectizeInput(inputId = "NZM_PCAplot_PCx_b1and2", label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "NZM_PCAplot_PCy_b1and2",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                     selectizeInput(inputId = "NZM_PCAplot_samples_g1_b1and2",label = "Select your samples for group1", choices= NZM_batch1and2_samples, multiple=TRUE),
                                     selectizeInput(inputId = "NZM_PCAplot_samples_g2_b1and2",label = "Select your samples for group2", choices= NZM_batch1and2_samples, multiple=TRUE),
                                     selectizeInput(inputId = "NZM_PCAplot_samples_g3_b1and2",label = "Select your samples for group3", choices= NZM_batch1and2_samples, multiple=TRUE),
                                     selectizeInput(inputId = "NZM_PCAplot_samples_g4_b1and2",label = "Select your samples for group4", choices= NZM_batch1and2_samples, multiple=TRUE),
                                      do.call(actionBttn, c(list(inputId = "NZM_make_PCAplot_b1and2", label = "make plot", icon = icon("play")), actionBttnParams)))
                 )
                 #downloadButton(outputId = "NZM_download_PCAplot_data",label = "Download data")
                 ),
          column(width = 4,
                 box(title = "PCA plot (batch1)",
                     status = "danger",
                     solidHeader = TRUE,
                     width = NULL,
                     plotly::plotlyOutput("NZM_plotly_PCAplot_b1_out"),
                     #DT::DTOutput("NZM_DT_PCA_batch1_out")
                     #plotly::plotlyOutput("NZM_plotly_PCAplot_batch1and2_out")
                     ),
                 box(title = "PCA plot (batch1 and batch2)",
                     status = "danger",
                     solidHeader = TRUE,
                     width = NULL,
                     plotly::plotlyOutput("NZM_plotly_PCAplot_b1and2_out"),
                     #DT::DTOutput("NZM_DT_PCA_batch1_out")
                     #plotly::plotlyOutput("NZM_plotly_PCAplot_batch1and2_out")
                 )),
          column(width = 4,
                 box(title = "PCA plot (batch2)",
                     status = "danger",
                     solidHeader = TRUE,
                     width = NULL,
                     plotly::plotlyOutput("NZM_plotly_PCAplot_b2_out")
                 )
          )
          
)




                     
                     
                     
                     
                     
                     