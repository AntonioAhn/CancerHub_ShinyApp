# Code line below was done to test what kind of output textAreaInput gives
# output$PDL1_geneheatmap_genelist_b1_out <- renderText(input$PDL1_geneheatmap_genelist_b1)

# batch1 heatmap 


PDL1_hclust_df_b1  <- eventReactive(input$PDL1_make_geneheatmap_b1, {
  df <- filter(PDL1_RNAseqdata_corplot, symbols %in% unlist(strsplit(x = input$PDL1_geneheatmap_genelist_b1, split = '[\r\n]'))) %>% 
    data.frame
  genenames <- df$symbols
  df <- df[,colnames(df) != "symbols"]
  rownames(df) <- genenames
  df %>% 
    dplyr::select(PDL1_batch1_samples) %>% 
    t %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    t %>% 
    data.frame()
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

PDL1_hclust_df_b1_filtered <- reactive({
  if (input$PDL1_geneheatmap_samples_b1) {
    dplyr::select(PDL1_hclust_df_b1(), input$PDL1_geneheatmap_samplelist_b1)
  } else {
    PDL1_hclust_df_b1()
  }
})



PDL1_hclust_heatmap_b1 <- eventReactive(input$PDL1_make_geneheatmap_b1,{
  heatmaply(
    PDL1_hclust_df_b1_filtered (),
    labRow = rownames(PDL1_hclust_df_b1_filtered ()),
    labCol = colnames(PDL1_hclust_df_b1_filtered ()),
    fontsize_col = input$PDL1_geneheatmap_colsize_b1,
    fontsize_row = input$PDL1_geneheatmap_rowsize_b1,
    dist_method = input$PDL1_geneheatmap_dist_b1, 
    hclust_method = input$PDL1_geneheatmap_hclust_b1,
    colors = get(input$PDL1_geneheatmap_color_b1)
  )
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)


### render plot
output$PDL1_hclust_heatmap_b1_out <- renderPlotly(PDL1_hclust_heatmap_b1())


#### ------------------------------------------------------------------------------------------------------------------

# batch 2 heatmap 

PDL1_hclust_df_b2  <- eventReactive(input$PDL1_make_geneheatmap_b2, {
  df <- filter(PDL1_RNAseqdata_corplot, symbols %in% unlist(strsplit(x = input$PDL1_geneheatmap_genelist_b2, split = '[\r\n]'))) %>% data.frame
  genenames <- df$symbols
  df <- df[,colnames(df) != "symbols"]
  rownames(df) <- genenames
  df %>% 
    dplyr::select(PDL1_batch2_samples) %>% 
    t %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    t %>% 
    data.frame()
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

PDL1_hclust_df_b2_filtered <- reactive({
  if (input$PDL1_geneheatmap_samples_b2) {
    dplyr::select(PDL1_hclust_df_b2(), input$PDL1_geneheatmap_samplelist_b2)
  } else {
    PDL1_hclust_df_b2()
  }
})



PDL1_hclust_heatmap_b2 <- eventReactive(input$PDL1_make_geneheatmap_b2, {
  heatmaply(
    PDL1_hclust_df_b2_filtered(),
    labRow = rownames(PDL1_hclust_df_b2_filtered()),
    labCol = colnames(PDL1_hclust_df_b2_filtered()),
    fontsize_col = input$PDL1_geneheatmap_colsize_b2,
    fontsize_row = input$PDL1_geneheatmap_rowsize_b2,
    dist_method = input$PDL1_geneheatmap_dist_b2, 
    hclust_method = input$PDL1_geneheatmap_hclust_b2,
    colors = get(input$PDL1_geneheatmap_color_b2)
  )
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)


### render plot
output$PDL1_hclust_heatmap_b2_out <- renderPlotly(PDL1_hclust_heatmap_b2())


#### ------------------------------------------------------------------------------------------------------------------


# batch 3 heatmap 

PDL1_hclust_df_b3  <- eventReactive(input$PDL1_make_geneheatmap_b3, {
  df <- filter(PDL1_RNAseqdata_corplot, symbols %in% unlist(strsplit(x = input$PDL1_geneheatmap_genelist_b3, split = '[\r\n]'))) %>% data.frame
  genenames <- df$symbols
  df <- df[,colnames(df) != "symbols"]
  rownames(df) <- genenames
  df %>% 
    dplyr::select(PDL1_batch3_samples) %>% 
    t %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    t %>% 
    data.frame()
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

PDL1_hclust_df_b3_filtered <- reactive({
  if (input$PDL1_geneheatmap_samples_b3) {
    dplyr::select(PDL1_hclust_df_b3(), input$PDL1_geneheatmap_samplelist_b3)
  } else {
    PDL1_hclust_df_b3()
  }
})


PDL1_hclust_heatmap_b3 <- eventReactive(input$PDL1_make_geneheatmap_b3, {
  heatmaply(
    PDL1_hclust_df_b3_filtered (),
    labRow = rownames(PDL1_hclust_df_b3_filtered ()),
    labCol = colnames(PDL1_hclust_df_b3_filtered ()),
    fontsize_col = input$PDL1_geneheatmap_colsize_b3,
    fontsize_row = input$PDL1_geneheatmap_rowsize_b3,
    dist_method = input$PDL1_geneheatmap_dist_b3, 
    hclust_method = input$PDL1_geneheatmap_hclust_b3,
    colors = get(input$PDL1_geneheatmap_color_b3)
  )
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)



### render plot
output$PDL1_hclust_heatmap_b3_out <- renderPlotly(PDL1_hclust_heatmap_b3())


#### ------------------------------------------------------------------------------------------------------------------

# batch 4 heatmap 

PDL1_hclust_df_b4  <- eventReactive(input$PDL1_make_geneheatmap_b4, {
  df <- filter(PDL1_RNAseqdata_corplot, symbols %in% unlist(strsplit(x = input$PDL1_geneheatmap_genelist_b4, split = '[\r\n]'))) %>% data.frame
  genenames <- df$symbols
  df <- df[,colnames(df) != "symbols"]
  rownames(df) <- genenames
  df %>% 
    dplyr::select(PDL1_batch4_samples) %>% 
    t %>% 
    scale(center = TRUE, scale = TRUE) %>% 
    t %>% 
    data.frame()
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)


PDL1_hclust_df_b4_filtered <- reactive({
  if (input$PDL1_geneheatmap_samples_b4) {
    dplyr::select(PDL1_hclust_df_b4(), input$PDL1_geneheatmap_samplelist_b4)
  } else {
    PDL1_hclust_df_b4()
  }
})



PDL1_hclust_heatmap_b4 <- eventReactive(input$PDL1_make_geneheatmap_b4, {
  heatmaply(
    PDL1_hclust_df_b4_filtered(),
    labRow = rownames(PDL1_hclust_df_b4_filtered()),
    labCol = colnames(PDL1_hclust_df_b4_filtered()),
    fontsize_col = input$PDL1_geneheatmap_colsize_b4,
    fontsize_row = input$PDL1_geneheatmap_rowsize_b4,
    dist_method = input$PDL1_geneheatmap_dist_b4, 
    hclust_method = input$PDL1_geneheatmap_hclust_b4,
    colors = get(input$PDL1_geneheatmap_color_b4)
  )
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)


### render plot
output$PDL1_hclust_heatmap_b4_out <- renderPlotly(PDL1_hclust_heatmap_b4())

#### ------------------------------------------------------------------------------------------------------------------

# batch 1,2,3 and 4 heatmap 

PDL1_hclust_df_bAll  <- eventReactive(input$PDL1_make_geneheatmap_bAll, {
  df <- filter(PDL1_RNAseqdata_corplot, symbols %in% unlist(strsplit(x = input$PDL1_geneheatmap_genelist_bAll, split = '[\r\n]'))) %>% data.frame
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


PDL1_hclust_df_bAll_filtered <- reactive({
  if (input$PDL1_geneheatmap_samples_bAll) {
    dplyr::select(PDL1_hclust_df_bAll(), input$PDL1_geneheatmap_samplelist_bAll)
  } else {
    PDL1_hclust_df_bAll()
  }
})



PDL1_hclust_heatmap_bAll <- eventReactive(input$PDL1_make_geneheatmap_bAll, {
  heatmaply(
    PDL1_hclust_df_bAll_filtered(),
    labRow = rownames(PDL1_hclust_df_bAll_filtered()),
    labCol = colnames(PDL1_hclust_df_bAll_filtered()),
    fontsize_col = input$PDL1_geneheatmap_colsize_bAll,
    fontsize_row = input$PDL1_geneheatmap_rowsize_bAll,
    dist_method = input$PDL1_geneheatmap_dist_bAll, 
    hclust_method = input$PDL1_geneheatmap_hclust_bAll,
    colors = get(input$PDL1_geneheatmap_color_bAll)
  )
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)


### render plot
output$PDL1_hclust_heatmap_bAll_out <- renderPlotly(PDL1_hclust_heatmap_bAll())

