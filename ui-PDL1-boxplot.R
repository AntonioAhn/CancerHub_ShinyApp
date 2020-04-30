


fluidPage(column(width = 12,
                 tabBox(title = "", width = NULL,
                        tabPanel(title = tagList(icon("dna"), "Boxplot (all samples)"),
                                 fluidPage(column(width = 3,
                                                  box(title = tagList(icon("dna"), "Enter Parameters"),
                                                      width = NULL,
                                                      solidHeader = TRUE,
                                                      status = "primary",
                                                      footer = "press make plot to generate the new figure",
                                                      textInput(inputId= "PDL1_boxplot_gene1", label = h3("Enter your gene:"), value = "CD274"),
                                                      checkboxGroupInput(inputId = "PDL1_boxplot_mutation", 
                                                                         label = "select mutation subtype", 
                                                                         choices = list("BRAF" = "BRAF", "NRAS" = "NRAS", "WT" = "WT"), 
                                                                         selected = c("BRAF", "NRAS", "WT")), 
                                                      checkboxGroupInput(inputId = "PDL1_boxplot_batch", 
                                                                         label = "select batch group", 
                                                                         choices = list("batch1" = "batch1", "batch2" = "batch2", "batch3" = "batch3", "batch4" = "batch4"), 
                                                                         selected = c("batch1", "batch2", "batch3", "batch4")), 
                                                      checkboxGroupInput(inputId = "PDL1_boxplot_PDL1", 
                                                                         label = "select PDL1 subtype", 
                                                                         choices = list("Inducible" = "Inducible", "Intermediate" = "Intermediate", "Constitutive" = "Constitutive"), 
                                                                         selected = c("Inducible","Intermediate","Constitutive")),
                                                      do.call(actionBttn, c(list( inputId = "PDL1_make_boxplot_all", label = "make plot", icon = icon("play")), actionBttnParams)),
                                                      downloadButton(outputId = "PDL1_download_boxplot_data_all", label = "Download data")
                                                      )
                                                  ),
                                           column(width = 9,
                                                  box(title = "Boxplot all",
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      width = NULL,
                                                      plotly::plotlyOutput("plotly_PDL1_boxplot_all") %>% withSpinner(),
                                                      DT::DTOutput("DT_PDL1_boxplot") %>% withSpinner())
                                           )
                                 )
                        ),
                        tabPanel(title = tagList(icon("dna"), "Boxplot (selected samples)"),
                                 fluidPage(column(width = 3, 
                                                  box(title = tagList(icon("dna"), "Enter Parameters for the group boxplot"),
                                                      width = NULL,
                                                      solidHeader = TRUE,
                                                      status = "primary",
                                                      checkboxInput("PDL1_selectgroups_boxplot","Select sample groups?", value=FALSE), #upload a file of gene names
                                                      conditionalPanel("input.PDL1_selectgroups_boxplot==true",
                                                                       textInput(inputId= "PDL1_boxplot_gene2", label = h3("Enter your gene:"), value = ""),
                                                                       selectizeInput(inputId = "PDL1_sampleselect_g1_boxplot","Select group 1",choices= PDL1_samplenames, multiple=TRUE),
                                                                       selectizeInput(inputId = "PDL1_sampleselect_g2_boxplot","Select group 2",choices= PDL1_samplenames, multiple=TRUE),
                                                                       selectizeInput(inputId = "PDL1_sampleselect_g3_boxplot","Select group 3",choices= PDL1_samplenames, multiple=TRUE),
                                                                       selectizeInput(inputId = "PDL1_sampleselect_g4_boxplot","Select group 4",choices= PDL1_samplenames, multiple=TRUE)
                                                      ),
                                                      do.call(actionBttn, c(list( inputId = "PDL1_make_boxplot_groups", label = "make plot", icon = icon("play")), actionBttnParams)),
                                                      downloadButton(outputId = "PDL1_download_boxplot_data_groups", label = "Download data"))
                                                  ),
                                           column(width = 9, 
                                                  box(title = "Boxplot groups",
                                                      status = "primary",
                                                      solidHeader = TRUE,
                                                      width = NULL,
                                                      plotly::plotlyOutput("PDL1_boxplot_groups") %>% withSpinner(),
                                                      DT::DTOutput("DT_PDL1_boxplot_groups") %>% withSpinner()
                                                      )
                                                  )
                                 )
                        )
                 )
)
)
                                           
                                           
   