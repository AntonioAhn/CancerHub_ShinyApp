
# batch 1

# [,get(input$PDL1_PCAplot_batch)]
### make the reactive prcomp dataframe
PDL1_prcomp_df_b1 <- eventReactive(input$PDL1_make_PCAplot_b1, {
  PCA.df <- data.frame(PDL1_RNAseqdata_corplot, row.names = 1)[,c(input$PDL1_PCAplot_samples_g1_b1,input$PDL1_PCAplot_samples_g2_b1,input$PDL1_PCAplot_samples_g3_b1,input$PDL1_PCAplot_samples_g4_b1)]
  high_var_genes <- PCA.df %>% apply(1, var) %>% sort(decreasing = TRUE) %>% head(n = input$PDL1_PCAplot_topgenes_b1) %>% names
  prcomp.df <- prcomp(t(PCA.df[high_var_genes,]), center = input$PDL1_PCAplot_centre_b1, scale = input$PDL1_PCAplot_scale_b1)
  prcomp.df
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### get summary prcomp percentage for x and y chosen PC's
PDL1_prcomp_summary_b1 <- eventReactive(input$PDL1_make_PCAplot_b1, {
  c(summary(PDL1_prcomp_df_b1())$importance[2,input$PDL1_PCAplot_PCx_b1] * 100, summary(PDL1_prcomp_df_b1())$importance[2,input$PDL1_PCAplot_PCy_b1] * 100)
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)


### Generate the plot_ly marker plot
PDL1_plotly_PCAplot_b1 <- eventReactive(input$PDL1_make_PCAplot_b1, {
  (PDL1_prcomp_df_b1()$x)[,1:((PDL1_prcomp_df_b1()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples") %>% 
#    mutate(batch = ifelse(samples %in% PDL1_batch1_samples, "batch1", "batch2")) %>% 
    mutate(groups = ifelse(samples %in% input$PDL1_PCAplot_samples_g1_b1, "group1", 
                           ifelse(samples %in% input$PDL1_PCAplot_samples_g2_b1, "group2", 
                                  ifelse(samples %in% input$PDL1_PCAplot_samples_g3_b1,  "group3", "group4")))) %>%  
    plot_ly(symbol = ~groups) %>% 
    add_markers(x = ~get(isolate({input$PDL1_PCAplot_PCx_b1})), y = ~get(isolate({input$PDL1_PCAplot_PCy_b1})), size = 20, hoverinfo = "text", colors = fut_color[1:4],
                text = ~paste0(input$PDL1_PCAplot_PCx_b1, ": ", get(isolate({input$PDL1_PCAplot_PCx_b1})), "<br>",
                               input$PDL1_PCAplot_PCy_b1, ": ", get(isolate({input$PDL1_PCAplot_PCy_b1})), "<br>",
                               'sample: ', samples)) %>% 
    layout(xaxis = list(title = paste0(input$PDL1_PCAplot_PCx_b1, " (",PDL1_prcomp_summary_b1()[1], "%)")), 
           yaxis = list(title = paste0(input$PDL1_PCAplot_PCy_b1, " (",PDL1_prcomp_summary_b1()[2], "%)")))
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)

### render gg_corplot
output$PDL1_plotly_PCAplot_b1_out <- renderPlotly(PDL1_plotly_PCAplot_b1())
#output$PDL1_DT_PCA_batch1_out <- renderDT(
#  (PDL1_prcomp_df_b1()$x)[,1:((PDL1_prcomp_df_b1()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples")
#  )


#### ------------------------------------------------------------------------------------------------------------------

# batch 2

# [,get(input$PDL1_PCAplot_batch)]
### make the reactive prcomp dataframe
PDL1_prcomp_df_b2 <- eventReactive(input$PDL1_make_PCAplot_b2, {
  PCA.df <- data.frame(PDL1_RNAseqdata_corplot, row.names = 1)[,c(input$PDL1_PCAplot_samples_g1_b2,input$PDL1_PCAplot_samples_g2_b2,input$PDL1_PCAplot_samples_g3_b2,input$PDL1_PCAplot_samples_g4_b2)]
  high_var_genes <- PCA.df %>% apply(1, var) %>% sort(decreasing = TRUE) %>% head(n = input$PDL1_PCAplot_topgenes_b2) %>% names
  prcomp.df <- prcomp(t(PCA.df[high_var_genes,]), center = TRUE, scale = TRUE)
  prcomp.df
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### get summary prcomp percentage for x and y chosen PC's
PDL1_prcomp_summary_b2 <- eventReactive(input$PDL1_make_PCAplot_b2, {
  c(summary(PDL1_prcomp_df_b2())$importance[2,input$PDL1_PCAplot_PCx_b2] * 100, summary(PDL1_prcomp_df_b2())$importance[2,input$PDL1_PCAplot_PCy_b2] * 100)
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### Generate the plot_ly marker plot
PDL1_plotly_PCAplot_b2 <- eventReactive(input$PDL1_make_PCAplot_b2, {
  (PDL1_prcomp_df_b2()$x)[,1:((PDL1_prcomp_df_b2()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples") %>% 
#    mutate(batch = ifelse(samples %in% batch1_samples, "batch1", "batch2")) %>% 
    mutate(groups = ifelse(samples %in% input$PDL1_PCAplot_samples_g1_b2, "group1", 
                           ifelse(samples %in% input$PDL1_PCAplot_samples_g2_b2, "group2", 
                                  ifelse(samples %in% input$PDL1_PCAplot_samples_g3_b2,  "group3", "group4")))) %>% 
    plot_ly(symbol = ~groups) %>% 
    add_markers(x = ~get(isolate({input$PDL1_PCAplot_PCx_b2})), y = ~get(isolate({input$PDL1_PCAplot_PCy_b2})), size = 20, hoverinfo = "text", colors = fut_color[1:4],
                text = ~paste0(input$PDL1_PCAplot_PCx_b2, ": ", get(isolate({input$PDL1_PCAplot_PCx_b2})), "<br>",
                               input$PDL1_PCAplot_PCy_b2, ": ", get(isolate({input$PDL1_PCAplot_PCy_b2})), "<br>",
                               'sample: ', samples)) %>% 
    layout(xaxis = list(title = paste0(input$PDL1_PCAplot_PCx_b2, " (",PDL1_prcomp_summary_b2()[1], "%)")), 
           yaxis = list(title = paste0(input$PDL1_PCAplot_PCy_b2, " (",PDL1_prcomp_summary_b2()[2], "%)")))
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)

### render gg_corplot
output$PDL1_plotly_PCAplot_b2_out <- renderPlotly(PDL1_plotly_PCAplot_b2())
#output$PDL1_DT_PCA_batch1_out <- renderDT(
#  (PDL1_prcomp_df_b2()$x)[,1:((PDL1_prcomp_df_b2()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples")
#  )

#### ------------------------------------------------------------------------------------------------------------------

# batch 3

# [,get(input$PDL1_PCAplot_batch)]
### make the reactive prcomp dataframe
PDL1_prcomp_df_b3 <- eventReactive(input$PDL1_make_PCAplot_b3, {
  PCA.df <- data.frame(PDL1_RNAseqdata_corplot, row.names = 1)[,c(input$PDL1_PCAplot_samples_g1_b3,input$PDL1_PCAplot_samples_g2_b3,input$PDL1_PCAplot_samples_g3_b3,input$PDL1_PCAplot_samples_g4_b3)]
  high_var_genes <- PCA.df %>% apply(1, var) %>% sort(decreasing = TRUE) %>% head(n = input$PDL1_PCAplot_topgenes_b3) %>% names
  prcomp.df <- prcomp(t(PCA.df[high_var_genes,]), center = TRUE, scale = TRUE)
  prcomp.df
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### get summary prcomp percentage for x and y chosen PC's
PDL1_prcomp_summary_b3 <- eventReactive(input$PDL1_make_PCAplot_b3, {
  c(summary(PDL1_prcomp_df_b3())$importance[2,input$PDL1_PCAplot_PCx_b3] * 100, summary(PDL1_prcomp_df_b3())$importance[2,input$PDL1_PCAplot_PCy_b3] * 100)
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### Generate the plot_ly marker plot
PDL1_plotly_PCAplot_b3 <- eventReactive(input$PDL1_make_PCAplot_b3, {
  (PDL1_prcomp_df_b3()$x)[,1:((PDL1_prcomp_df_b3()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples") %>% 
#    mutate(batch = ifelse(samples %in% batch1_samples, "batch1", "batch2")) %>% 
    mutate(groups = ifelse(samples %in% input$PDL1_PCAplot_samples_g1_b3, "group1", 
                           ifelse(samples %in% input$PDL1_PCAplot_samples_g2_b3, "group2", 
                                  ifelse(samples %in% input$PDL1_PCAplot_samples_g3_b3,  "group3", "group4")))) %>% 
    plot_ly(symbol = ~groups) %>% 
    add_markers(x = ~get(isolate({input$PDL1_PCAplot_PCx_b3})), y = ~get(isolate({input$PDL1_PCAplot_PCy_b3})), size = 20, hoverinfo = "text", colors = fut_color[1:4],
                text = ~paste0(input$PDL1_PCAplot_PCx_b3, ": ", get(isolate({input$PDL1_PCAplot_PCx_b3})), "<br>",
                               input$PDL1_PCAplot_PCy_b3, ": ", get(isolate({input$PDL1_PCAplot_PCy_b3})), "<br>",
                               'sample: ', samples)) %>% 
    layout(xaxis = list(title = paste0(input$PDL1_PCAplot_PCx_b3, " (",PDL1_prcomp_summary_b3()[1], "%)")), 
           yaxis = list(title = paste0(input$PDL1_PCAplot_PCy_b3, " (",PDL1_prcomp_summary_b3()[2], "%)")))
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)

### render gg_corplot
output$PDL1_plotly_PCAplot_b3_out <- renderPlotly(PDL1_plotly_PCAplot_b3())
#output$PDL1_DT_PCA_batch1_out <- renderDT(
#  (PDL1_prcomp_df_b3()$x)[,1:((PDL1_prcomp_df_b3()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples")
#  )

#### ------------------------------------------------------------------------------------------------------------------

# batch 4

# [,get(input$PDL1_PCAplot_batch)]
### make the reactive prcomp dataframe
PDL1_prcomp_df_b4 <- eventReactive(input$PDL1_make_PCAplot_b4, {
  PCA.df <- data.frame(PDL1_RNAseqdata_corplot, row.names = 1)[,c(input$PDL1_PCAplot_samples_g1_b4,input$PDL1_PCAplot_samples_g2_b4,input$PDL1_PCAplot_samples_g3_b4,input$PDL1_PCAplot_samples_g4_b4)]
  high_var_genes <- PCA.df %>% apply(1, var) %>% sort(decreasing = TRUE) %>% head(n = input$PDL1_PCAplot_topgenes_b4) %>% names
  prcomp.df <- prcomp(t(PCA.df[high_var_genes,]), center = TRUE, scale = TRUE)
  prcomp.df
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### get summary prcomp percentage for x and y chosen PC's
PDL1_prcomp_summary_b4 <- eventReactive(input$PDL1_make_PCAplot_b4, {
  c(summary(PDL1_prcomp_df_b4())$importance[2,input$PDL1_PCAplot_PCx_b4] * 100, summary(PDL1_prcomp_df_b4())$importance[2,input$PDL1_PCAplot_PCy_b4] * 100)
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### Generate the plot_ly marker plot
PDL1_plotly_PCAplot_b4 <- eventReactive(input$PDL1_make_PCAplot_b4, {
  (PDL1_prcomp_df_b4()$x)[,1:((PDL1_prcomp_df_b4()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples") %>% 
#    mutate(batch = ifelse(samples %in% batch1_samples, "batch1", "batch2")) %>% 
    mutate(groups = ifelse(samples %in% input$PDL1_PCAplot_samples_g1_b4, "group1", 
                           ifelse(samples %in% input$PDL1_PCAplot_samples_g2_b4, "group2", 
                                  ifelse(samples %in% input$PDL1_PCAplot_samples_g3_b4,  "group3", "group4")))) %>% 
    plot_ly(symbol = ~groups) %>% 
    add_markers(x = ~get(isolate({input$PDL1_PCAplot_PCx_b4})), y = ~get(isolate({input$PDL1_PCAplot_PCy_b4})), size = 20, hoverinfo = "text", colors = fut_color[1:4],
                text = ~paste0(input$PDL1_PCAplot_PCx_b4, ": ", get(isolate({input$PDL1_PCAplot_PCx_b4})), "<br>",
                               input$PDL1_PCAplot_PCy_b4, ": ", get(isolate({input$PDL1_PCAplot_PCy_b4})), "<br>",
                               'sample: ', samples)) %>% 
    layout(xaxis = list(title = paste0(input$PDL1_PCAplot_PCx_b4, " (",PDL1_prcomp_summary_b4()[1], "%)")), 
           yaxis = list(title = paste0(input$PDL1_PCAplot_PCy_b4, " (",PDL1_prcomp_summary_b4()[2], "%)")))
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)

### render gg_corplot
output$PDL1_plotly_PCAplot_b4_out <- renderPlotly(PDL1_plotly_PCAplot_b4())
#output$PDL1_DT_PCA_batch1_out <- renderDT(
#  (PDL1_prcomp_df_b4()$x)[,1:((PDL1_prcomp_df_b4()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples")
#  )

#### ------------------------------------------------------------------------------------------------------------------

# batch all

# [,get(input$PDL1_PCAplot_batch)]
### make the reactive prcomp dataframe
PDL1_prcomp_df_bAll <- eventReactive(input$PDL1_make_PCAplot_bAll, {
  PCA.df <- data.frame(PDL1_RNAseqdata_corplot, row.names = 1)[,c(input$PDL1_PCAplot_samples_g1_bAll,input$PDL1_PCAplot_samples_g2_bAll,input$PDL1_PCAplot_samples_g3_bAll,input$PDL1_PCAplot_samples_g4_bAll)]
  high_var_genes <- PCA.df %>% apply(1, var) %>% sort(decreasing = TRUE) %>% head(n = input$PDL1_PCAplot_topgenes_bAll) %>% names
  prcomp.df <- prcomp(t(PCA.df[high_var_genes,]), center = TRUE, scale = TRUE)
  prcomp.df
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### get summary prcomp percentage for x and y chosen PC's
PDL1_prcomp_summary_bAll <- eventReactive(input$PDL1_make_PCAplot_bAll, {
  c(summary(PDL1_prcomp_df_bAll())$importance[2,input$PDL1_PCAplot_PCx_bAll] * 100, summary(PDL1_prcomp_df_bAll())$importance[2,input$PDL1_PCAplot_PCy_bAll] * 100)
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### Generate the plot_ly marker plot
PDL1_plotly_PCAplot_bAll <- eventReactive(input$PDL1_make_PCAplot_bAll, {
  (PDL1_prcomp_df_bAll()$x)[,1:((PDL1_prcomp_df_bAll()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples") %>% 
#    mutate(batch = ifelse(samples %in% batch1_samples, "batch1", "batch2")) %>% 
    mutate(groups = ifelse(samples %in% input$PDL1_PCAplot_samples_g1_bAll, "group1", 
                           ifelse(samples %in% input$PDL1_PCAplot_samples_g2_bAll, "group2", 
                                  ifelse(samples %in% input$PDL1_PCAplot_samples_g3_bAll,  "group3", "group4")))) %>% 
    plot_ly(symbol = ~groups) %>% 
    add_markers(x = ~get(isolate({input$PDL1_PCAplot_PCx_bAll})), y = ~get(isolate({input$PDL1_PCAplot_PCy_bAll})), size = 20, hoverinfo = "text", colors = fut_color[1:4],
                text = ~paste0(input$PDL1_PCAplot_PCx_bAll, ": ", get(isolate({input$PDL1_PCAplot_PCx_bAll})), "<br>",
                               input$PDL1_PCAplot_PCy_bAll, ": ", get(isolate({input$PDL1_PCAplot_PCy_bAll})), "<br>",
                               'sample: ', samples)) %>% 
    layout(xaxis = list(title = paste0(input$PDL1_PCAplot_PCx_bAll, " (",PDL1_prcomp_summary_bAll()[1], "%)")), 
           yaxis = list(title = paste0(input$PDL1_PCAplot_PCy_bAll, " (",PDL1_prcomp_summary_bAll()[2], "%)")))
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)

### render gg_corplot
output$PDL1_plotly_PCAplot_bAll_out <- renderPlotly(PDL1_plotly_PCAplot_bAll())
#output$PDL1_DT_PCA_batch1_out <- renderDT(
#  (PDL1_prcomp_df_bAll()$x)[,1:((PDL1_prcomp_df_bAll()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples")
#  )



