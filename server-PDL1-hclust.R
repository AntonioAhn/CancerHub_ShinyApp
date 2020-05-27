# Code line below was done to test what kind of output textAreaInput gives
# output$PDL1_geneheatmap_genelist_b1_out <- renderText(input$PDL1_geneheatmap_genelist_b1)

PDL1_hclust_df  <- eventReactive(input$PDL1_make_geneheatmap, {
  df <- filter(PDL1_RNAseqdata_corplot, symbols %in% unlist(strsplit(x = input$PDL1_geneheatmap_genelist, split = '[\r\n]'))) %>% data.frame
  genenames <- df$symbols
  df <- df[,colnames(df) != "symbols"]
  rownames(df) <- genenames
  df %>% 
    dplyr::select(PDL1_all_samples) %>% 
    t %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    t %>% 
    data.frame()
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)


PDL1_hclust_df_filtered <- reactive({
  if (input$PDL1_geneheatmap_samples) {
    dplyr::select(PDL1_hclust_df(), input$PDL1_geneheatmap_samplelist)
  } else {
    PDL1_hclust_df()
  }
})



PDL1_hclust_heatmap <- eventReactive(input$PDL1_make_geneheatmap, {
  heatmaply(
    PDL1_hclust_df_filtered(),
    labRow = rownames(PDL1_hclust_df_filtered()),
    labCol = colnames(PDL1_hclust_df_filtered()),
    fontsize_col = input$PDL1_geneheatmap_colsize,
    fontsize_row = input$PDL1_geneheatmap_rowsize,
    dist_method = input$PDL1_geneheatmap_dist, 
    hclust_method = input$PDL1_geneheatmap_hclust,
    colors = get(input$PDL1_geneheatmap_color)
  )
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)


### render plot
output$PDL1_hclust_heatmap_out <- renderPlotly(PDL1_hclust_heatmap())

