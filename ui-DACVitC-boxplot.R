fluidPage(column(width = 12,
                 tabBox(title = "", width = NULL,
                        tabPanel(title = tagList(icon("dna"), "Boxplot (all samples)"),
                                 fluidPage(column(width = 3,
                                                  box(title = tagList(icon("dna"), "Enter Parameters"),
                                                      width = NULL,
                                                      solidHeader = TRUE,
                                                      status = "primary",
                                                      footer = "press make plot to generate the new figure",
                                                      textInput(inputId= "DACVitC_boxplot_gene1", label = h3("Enter your gene:"), value = "CD274"),
                                                      checkboxInput("DACVitC_boxplot_samples","Select samples instead of showing all samples?", value=FALSE),
                                                      conditionalPanel("input.DACVitC_boxplot_samples==true",
                                                                       selectizeInput(inputId= "DACVitC_boxplot_samplelist", label = "select your samples:",choices = unique(DACVitC_RNAseqdata_sampleinfo$cellline), multiple = TRUE)),
                                                      checkboxGroupButtons(inputId = "DACVitC_boxplot_treatmentgroup", 
                                                                   label = "select treatment groups", 
                                                                   choices = list("DAC" = "DAC", "VitC" = "VitC", "DAC and VitC" = "DAC.VitC"), 
                                                                   selected = "DAC.VitC",
                                                                   checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
                                                      do.call(actionBttn, c(list( inputId = "DACVitC_make_boxplot_all", label = "make plot", icon = icon("play")), actionBttnParams)),
                                                      downloadButton(outputId = "DACVitC_download_boxplot_data_all", label = "Download data")
                                                  )
                                 ),
                                 column(width = 9,
                                        box(title = "Boxplot all",
                                            status = "primary",
                                            solidHeader = TRUE,
                                            width = NULL,
                                            plotOutput("plot_DACVitC_boxplot_all") %>% withSpinner(),
                                            DT::DTOutput("DT_DACVitC_boxplot") %>% withSpinner())
                                 )
                                 )
                        )
                        
                 )
)
)