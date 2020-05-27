

fluidPage(column(width = 3,
                 box(title = tagList(icon("dna"), "Enter Parameters for the PCA plot"),
                     width = NULL,
                     solidHeader = TRUE,
                     status = "primary",
                     footer = "press make pca plot to generate the new figure",
                     selectizeInput(inputId = "PDL1_PCAplot_PCx",label = "Select your PC (x-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC1"),
                     selectizeInput(inputId = "PDL1_PCAplot_PCy",label = "Select your PC (y-axis)",choices= paste0("PC", 1:10), multiple=FALSE, selected = "PC2"),
                     selectizeInput(inputId = "PDL1_PCAplot_samples_g1",label = "Select your samples for group1", choices= PDL1_all_samples, multiple=TRUE),
                     selectizeInput(inputId = "PDL1_PCAplot_samples_g2",label = "Select your samples for group2", choices= PDL1_all_samples, multiple=TRUE),
                     selectizeInput(inputId = "PDL1_PCAplot_samples_g3",label = "Select your samples for group3", choices= PDL1_all_samples, multiple=TRUE),
                     selectizeInput(inputId = "PDL1_PCAplot_samples_g4",label = "Select your samples for group4", choices= PDL1_all_samples, multiple=TRUE),
                     hr(),
                     helpText("Top genes are selected according to the genes with the highest variance after filtering out for genes with zero variance"),
                     numericInput(inputId = "PDL1_PCAplot_topgenes",label = "Number of top genes",value = 100, min = 0, step = 1),
                     materialSwitch(inputId = "PDL1_PCAplot_scale",label = "Scale",value = TRUE,right = TRUE,status = "primary"),
                     materialSwitch(inputId = "PDL1_PCAplot_centre",label = "Centre",value = TRUE,right = TRUE,status = "primary"),
                     do.call(actionBttn, c(list(inputId = "PDL1_make_PCAplot", label = "make plot", icon = icon("play")), actionBttnParams)))
                 ),
          column(width = 9,
                 tabBox(title = "", 
                        width = NULL,
                        tabPanel(
                          title = "PCA plot (select samples)",
                          status = "primary",
                          solidHeader = TRUE,
                          width = NULL,
                          plotly::plotlyOutput("PDL1_plotly_PCAplot_out") %>% withSpinner()
                          )
                        )
                 )
          )









