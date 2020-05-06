# Code line below was done to test what kind of output textAreaInput gives
# output$NZM_geneheatmap_genelist_b1_out <- renderText(input$NZM_geneheatmap_genelist_b1)

# batch1 heatmap 


NZM_hclust_df_b1  <- eventReactive(input$NZM_make_geneheatmap_b1, {
  df <- filter(NZM_RNAseqdata_corplot, symbols %in% unlist(strsplit(x = input$NZM_geneheatmap_genelist_b1, split = '[\r\n]'))) %>% 
    data.frame
  genenames <- df$symbols
  df <- df[,colnames(df) != "symbols"]
  rownames(df) <- genenames
  df %>% 
    dplyr::select(NZM_batch1_samples) %>% 
    t %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    t %>% 
    data.frame()
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

#output$NZM_geneheatmap_samples_b1_output <- renderUI({
#  tagList(checkboxInput("NZM_geneheatmap_samples_b1","Select sample groups?", value=FALSE),
#  conditionalPanel("input.NZM_geneheatmap_samples_b1==true",
#                   selectizeInput(inputId= "NZM_geneheatmap_samplelist_b1", label = "select your samples:",choices = NZM_batch1_samples, multiple = TRUE))
#  )
#})
  
NZM_hclust_df_b1_filtered <- reactive({
  if (input$NZM_geneheatmap_samples_b1) {
    dplyr::select(NZM_hclust_df_b1(), input$NZM_geneheatmap_samplelist_b1)
  } else {
  NZM_hclust_df_b1()
  }
})


NZM_hclust_heatmap_b1 <- eventReactive(input$NZM_make_geneheatmap_b1,{
  heatmaply(
  NZM_hclust_df_b1_filtered(),
  labRow = rownames(NZM_hclust_df_b1_filtered()),
  labCol = colnames(NZM_hclust_df_b1_filtered()),
  fontsize_col = input$NZM_geneheatmap_colsize_b1,
  fontsize_row = input$NZM_geneheatmap_rowsize_b1,
  dist_method = input$NZM_geneheatmap_dist_b1, 
  hclust_method = input$NZM_geneheatmap_hclust_b1,
  colors = get(input$NZM_geneheatmap_color_b1)
  ) %>% 
    config(
      toImageButtonOptions = list(
        format = "PNG",
        filename = "heatmap",
        width = 1500,
        height = 1800
      )
    )
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)



### render plot
output$NZM_hclust_heatmap_b1_out <- renderPlotly(NZM_hclust_heatmap_b1())


#### ------------------------------------------------------------------------------------------------------------------

# batch 2 heatmap 

NZM_hclust_df_b2  <- eventReactive(input$NZM_make_geneheatmap_b2, {
  df <- filter(NZM_RNAseqdata_corplot, symbols %in% unlist(strsplit(x = input$NZM_geneheatmap_genelist_b2, split = '[\r\n]'))) %>% data.frame
  genenames <- df$symbols
  df <- df[,colnames(df) != "symbols"]
  rownames(df) <- genenames
  df %>% 
    dplyr::select(NZM_batch2_samples) %>% 
    t %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    t %>% 
    data.frame()
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

NZM_hclust_df_b2_filtered <- reactive({
  if (input$NZM_geneheatmap_samples_b2) {
    dplyr::select(NZM_hclust_df_b2(), input$NZM_geneheatmap_samplelist_b2)
  } else {
    NZM_hclust_df_b2()
  }
})

NZM_hclust_heatmap_b2 <- eventReactive(input$NZM_make_geneheatmap_b2,{
  heatmaply(
  NZM_hclust_df_b2_filtered(),
  labRow = rownames(NZM_hclust_df_b2_filtered()),
  labCol = colnames(NZM_hclust_df_b2_filtered()),
  fontsize_col = input$NZM_geneheatmap_colsize_b2,
  fontsize_row = input$NZM_geneheatmap_rowsize_b2,
  dist_method = input$NZM_geneheatmap_dist_b2, 
  hclust_method = input$NZM_geneheatmap_hclust_b2,
  colors = get(input$NZM_geneheatmap_color_b2)
  )
},
ignoreNULL = TRUE,
ignoreInit = TRUE)


### render plot
output$NZM_hclust_heatmap_b2_out <- renderPlotly(NZM_hclust_heatmap_b2())


#### ------------------------------------------------------------------------------------------------------------------

# batch 1 and 2 heatmap 

NZM_hclust_df_b1and2  <- eventReactive(input$NZM_make_geneheatmap_b1and2, {
  df <- filter(NZM_RNAseqdata_corplot, symbols %in% unlist(strsplit(x = input$NZM_geneheatmap_genelist_b1and2, split = '[\r\n]'))) %>% data.frame
  genenames <- df$symbols
  df <- df[,colnames(df) != "symbols"]
  rownames(df) <- genenames
  df %>% 
    dplyr::select(c(NZM_batch1_samples, NZM_batch2_samples)) %>% 
    t %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    t %>% 
    data.frame()
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

NZM_hclust_df_b1and2_filtered <- reactive({
  if (input$NZM_geneheatmap_samples_b1and2) {
    dplyr::select(NZM_hclust_df_b1and2(), input$NZM_geneheatmap_samplelist_b1and2)
  } else {
    NZM_hclust_df_b1and2()
  }
})

NZM_hclust_heatmap_b1and2 <- eventReactive(input$NZM_make_geneheatmap_b1and2, {
  heatmaply(
    NZM_hclust_df_b1and2_filtered(),
    labRow = rownames(NZM_hclust_df_b1and2_filtered()),
    labCol = colnames(NZM_hclust_df_b1and2_filtered()),
    fontsize_col = input$NZM_geneheatmap_colsize_b1and2,
    fontsize_row = input$NZM_geneheatmap_rowsize_b1and2,
    dist_method = input$NZM_geneheatmap_dist_b1and2, 
    hclust_method = input$NZM_geneheatmap_hclust_b1and2,
    colors = get(input$NZM_geneheatmap_color_b1and2)
  )
},
ignoreNULL = TRUE,
ignoreInit = TRUE)


### render plot
output$NZM_hclust_heatmap_b1and2_out <- renderPlotly(NZM_hclust_heatmap_b1and2())

