

fluidPage(column(width = 3,
                 box(title = tagList(icon("dna"), "Enter Parameters for the PCA plot"),
                     width = NULL,
                     solidHeader = TRUE,
                     status = "primary",
                     footer = "press make pca plot to generate the new figure",
                     radioButtons(inputId = "PDL1_PCAplot_batch", 
                                  label = "select batch", 
                                  choices = list("batch1" = "PDL1_batch1_samples", "batch2" = "PDL1_batch2_samples", "batch3" = "PDL1_batch3_samples", "batch4" = "PDL1_batch4_samples", "all batches" = "PDL1_all_samples"), 
                                  selected = "PDL1_batch1_samples"),
                     conditionalPanel(condition = "input.PDL1_PCAplot_batch=='PDL1_batch1_samples'",
                                      selectizeInput(inputId = "PDL1_PCAplot_PCx_b1",label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "PDL1_PCAplot_PCy_b1",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g1_b1",label = "Select your samples for group1", choices= PDL1_batch1_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g2_b1",label = "Select your samples for group2", choices= PDL1_batch1_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g3_b1",label = "Select your samples for group3", choices= PDL1_batch1_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g4_b1",label = "Select your samples for group4", choices= PDL1_batch1_samples, multiple=TRUE),
                                      hr(),
                                      helpText("Top genes are selected according to the genes with the highest variance after filtering out for genes with zero variance"),
                                      numericInput(inputId = "PDL1_PCAplot_topgenes_b1",label = "Number of top genes",value = 100, min = 0, step = 1),
                                      materialSwitch(inputId = "PDL1_PCAplot_scale_b1",label = "Scale",value = TRUE,right = TRUE,status = "primary"),
                                      materialSwitch(inputId = "PDL1_PCAplot_centre_b1",label = "Centre",value = TRUE,right = TRUE,status = "primary"),
                                      do.call(actionBttn, c(list(inputId = "PDL1_make_PCAplot_b1", label = "make plot", icon = icon("play")), actionBttnParams))),
                     conditionalPanel(condition = "input.PDL1_PCAplot_batch=='PDL1_batch2_samples'",
                                      selectizeInput(inputId = "PDL1_PCAplot_PCx_b2", label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "PDL1_PCAplot_PCy_b2",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g1_b2",label = "Select your samples for group1", choices= PDL1_batch2_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g2_b2",label = "Select your samples for group2", choices= PDL1_batch2_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g3_b2",label = "Select your samples for group3", choices= PDL1_batch2_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g4_b2",label = "Select your samples for group4", choices= PDL1_batch2_samples, multiple=TRUE),
                                      hr(),
                                      helpText("Top genes are selected according to the genes with the highest variance after filtering out for genes with zero variance"),
                                      numericInput(inputId = "PDL1_PCAplot_topgenes_b2",label = "Number of top genes",value = 100, min = 0, step = 1),
                                      materialSwitch(inputId = "PDL1_PCAplot_scale_b2",label = "Scale",value = TRUE,right = TRUE,status = "primary"),
                                      materialSwitch(inputId = "PDL1_PCAplot_centre_b2",label = "Centre",value = TRUE,right = TRUE,status = "primary"),
                                      do.call(actionBttn, c(list(inputId = "PDL1_make_PCAplot_b2", label = "make plot", icon = icon("play")), actionBttnParams))),
                     conditionalPanel(condition = "input.PDL1_PCAplot_batch=='PDL1_batch3_samples'",
                                      selectizeInput(inputId = "PDL1_PCAplot_PCx_b3",label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "PDL1_PCAplot_PCy_b3",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g1_b3",label = "Select your samples for group1", choices= PDL1_batch3_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g2_b3",label = "Select your samples for group2", choices= PDL1_batch3_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g3_b3",label = "Select your samples for group3", choices= PDL1_batch3_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g4_b3",label = "Select your samples for group4", choices= PDL1_batch3_samples, multiple=TRUE),
                                      hr(),
                                      helpText("Top genes are selected according to the genes with the highest variance after filtering out for genes with zero variance"),
                                      numericInput(inputId = "PDL1_PCAplot_topgenes_b3",label = "Number of top genes",value = 100, min = 0, step = 1),
                                      materialSwitch(inputId = "PDL1_PCAplot_scale_b3",label = "Scale",value = TRUE,right = TRUE,status = "primary"),
                                      materialSwitch(inputId = "PDL1_PCAplot_centre_b3",label = "Centre",value = TRUE,right = TRUE,status = "primary"),
                                      do.call(actionBttn, c(list(inputId = "PDL1_make_PCAplot_b3", label = "make plot", icon = icon("play")), actionBttnParams))),
                     conditionalPanel(condition = "input.PDL1_PCAplot_batch=='PDL1_batch4_samples'",
                                      selectizeInput(inputId = "PDL1_PCAplot_PCx_b4",label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "PDL1_PCAplot_PCy_b4",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g1_b4",label = "Select your samples for group1", choices= PDL1_batch4_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g2_b4",label = "Select your samples for group2", choices= PDL1_batch4_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g3_b4",label = "Select your samples for group3", choices= PDL1_batch4_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g4_b4",label = "Select your samples for group4", choices= PDL1_batch4_samples, multiple=TRUE),
                                      hr(),
                                      helpText("Top genes are selected according to the genes with the highest variance after filtering out for genes with zero variance"),
                                      numericInput(inputId = "PDL1_PCAplot_topgenes_b4",label = "Number of top genes",value = 100, min = 0, step = 1),
                                      materialSwitch(inputId = "PDL1_PCAplot_scale_b4",label = "Scale",value = TRUE,right = TRUE,status = "primary"),
                                      materialSwitch(inputId = "PDL1_PCAplot_centre_b4",label = "Centre",value = TRUE,right = TRUE,status = "primary"),
                                      do.call(actionBttn, c(list(inputId = "PDL1_make_PCAplot_b4", label = "make plot", icon = icon("play")), actionBttnParams))),
                     conditionalPanel(condition = "input.PDL1_PCAplot_batch=='PDL1_all_samples'",
                                      selectizeInput(inputId = "PDL1_PCAplot_PCx_bAll",label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                                      selectizeInput(inputId = "PDL1_PCAplot_PCy_bAll",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g1_bAll",label = "Select your samples for group1", choices= PDL1_all_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g2_bAll",label = "Select your samples for group2", choices= PDL1_all_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g3_bAll",label = "Select your samples for group3", choices= PDL1_all_samples, multiple=TRUE),
                                      selectizeInput(inputId = "PDL1_PCAplot_samples_g4_bAll",label = "Select your samples for group4", choices= PDL1_all_samples, multiple=TRUE),
                                      hr(),
                                      helpText("Top genes are selected according to the genes with the highest variance after filtering out for genes with zero variance"),
                                      numericInput(inputId = "PDL1_PCAplot_topgenes_bAll",label = "Number of top genes",value = 100, min = 0, step = 1),
                                      materialSwitch(inputId = "PDL1_PCAplot_scale_bAll",label = "Scale",value = TRUE,right = TRUE,status = "primary"),
                                      materialSwitch(inputId = "PDL1_PCAplot_centre_bAll",label = "Centre",value = TRUE,right = TRUE,status = "primary"),
                                      do.call(actionBttn, c(list(inputId = "PDL1_make_PCAplot_bAll", label = "make plot", icon = icon("play")), actionBttnParams)))
                 )
                 #downloadButton(outputId = "PDL1_download_PCAplot_data",label = "Download data")
),
column(width = 9,
       tabBox(title = "", 
              width = NULL,
              tabPanel(
                title = "PCA plot (batch1)",
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                plotly::plotlyOutput("PDL1_plotly_PCAplot_b1_out") %>% withSpinner()
              ),
              tabPanel(
                title = "PCA plot (batch2)",
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                plotly::plotlyOutput("PDL1_plotly_PCAplot_b2_out") %>% withSpinner()
              ),
              tabPanel(
                title = "PCA plot (batch3)",
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                plotly::plotlyOutput("PDL1_plotly_PCAplot_b3_out") %>% withSpinner()
              ),
              tabPanel(
                title = "PCA plot (batch4)",
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                plotly::plotlyOutput("PDL1_plotly_PCAplot_b4_out") %>% withSpinner()
              ),
              tabPanel(
                title = "PCA plot (all 4 batches)",
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                plotly::plotlyOutput("PDL1_plotly_PCAplot_bAll_out") %>% withSpinner()
              )
       )
)
)









