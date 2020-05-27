fluidPage(column(width = 12,
                 tabBox(title = "", width = NULL,
                        tabPanel(title = tagList(icon("dna"), "Boxplot (all samples)"),
                                 fluidPage(column(width = 3,
                                                  box(title = tagList(icon("dna"), "Enter Parameters"),
                                                      width = NULL,
                                                      solidHeader = TRUE,
                                                      status = "primary",
                                                      footer = "press make plot to generate the new figure",
                                                      textInput(inputId= "DAC_boxplot_gene1", label = h3("Enter your gene:"), value = "CD274"),
                                                      do.call(actionBttn, c(list( inputId = "DAC_make_boxplot_all", label = "make plot", icon = icon("play")), actionBttnParams)),
                                                      downloadButton(outputId = "DAC_download_boxplot_data_all", label = "Download data")
                                                  )
                                 ),
                                 column(width = 9,
                                        box(title = "Boxplot all",
                                            status = "primary",
                                            solidHeader = TRUE,
                                            width = NULL,
                                            plotOutput("plot_DAC_boxplot_all") %>% withSpinner(),
                                            DT::DTOutput("DT_DAC_boxplot") %>% withSpinner())
                                 )
                                 )
                        ),
                        tabPanel(title = tagList(icon("dna"), "Boxplot (selected samples)"),
                                 fluidPage(column(width = 3, 
                                                  box(title = tagList(icon("dna"), "Enter Parameters for the group boxplot"),
                                                      width = NULL,
                                                      solidHeader = TRUE,
                                                      status = "primary",
                                                      textInput(inputId= "DAC_boxplot_gene2", label = h3("Enter your gene:"), value = ""),
                                                      selectizeInput(inputId = "DAC_sampleselect_g1_boxplot","Select samples",choices= unique(DAC_RNAseqdata_sampleinfo$cellline), multiple=TRUE),
                                                      do.call(actionBttn, c(list( inputId = "DAC_make_boxplot_groups", label = "make plot", icon = icon("play")), actionBttnParams)),
                                                      downloadButton(outputId = "DAC_download_boxplot_data_groups", label = "Download data"))
                                 ),
                                 column(width = 9, 
                                        box(title = "Boxplot groups",
                                            status = "primary",
                                            solidHeader = TRUE,
                                            width = NULL,
                                            plotOutput("DAC_boxplot_groups") %>% withSpinner(),
                                            DT::DTOutput("DT_DAC_boxplot_groups") %>% withSpinner()
                                        )
                                 )
                                 )
                        )
                        
                 )
)
)