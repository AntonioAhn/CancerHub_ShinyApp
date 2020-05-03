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
    t
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

NZM_hclust_heatmap_b1 <- eventReactive(input$NZM_make_geneheatmap_b1,{
  heatmaply(
  NZM_hclust_df_b1(),
  labRow = rownames(NZM_hclust_df_b1()),
  labCol = colnames(NZM_hclust_df_b1()),
  fontsize_col = 5,
  dist_method = input$NZM_geneheatmap_dist_b1, 
  hclust_method = input$NZM_geneheatmap_hclust_b1,
  colors = get(input$NZM_geneheatmap_color_b1)
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
    t
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)

NZM_hclust_heatmap_b2 <- reactive({
  heatmaply(
  NZM_hclust_df_b2(),
  labRow = rownames(NZM_hclust_df_b2()),
  labCol = colnames(NZM_hclust_df_b2()),
  fontsize_col = 5,
  dist_method = input$NZM_geneheatmap_dist_b2, 
  hclust_method = input$NZM_geneheatmap_hclust_b2,
  colors = get(input$NZM_geneheatmap_color_b2)
  )
})


### render plot
output$NZM_hclust_heatmap_b2_out <- renderPlotly(NZM_hclust_heatmap_b2())


#### ------------------------------------------------------------------------------------------------------------------

# batch 2 heatmap 

NZM_hclust_df_b1and2  <- eventReactive(input$NZM_make_geneheatmap_b1and2, {
  df <- filter(NZM_RNAseqdata_corplot, symbols %in% unlist(strsplit(x = input$NZM_geneheatmap_genelist_b1and2, split = '[\r\n]'))) %>% data.frame
  genenames <- df$symbols
  df <- df[,colnames(df) != "symbols"]
  rownames(df) <- genenames
  df %>% 
    dplyr::select(c(NZM_batch1_samples, NZM_batch2_samples)) %>% 
    t %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    t
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)

NZM_hclust_heatmap_b1and2 <- reactive({
  heatmaply(
    NZM_hclust_df_b1and2(),
    labRow = rownames(NZM_hclust_df_b1and2()),
    labCol = colnames(NZM_hclust_df_b1and2()),
    fontsize_col = 5,
    dist_method = input$NZM_geneheatmap_dist_b1and2, 
    hclust_method = input$NZM_geneheatmap_hclust_b1and2,
    colors = get(input$NZM_geneheatmap_color_b1and2)
  )
})


### render plot
output$NZM_hclust_heatmap_b1and2_out <- renderPlotly(NZM_hclust_heatmap_b1and2())

