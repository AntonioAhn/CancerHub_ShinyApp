
# batch 1

# [,get(input$NZM_PCAplot_batch)]
### make the reactive prcomp dataframe
NZM_prcomp_df_b1 <- eventReactive(input$NZM_make_PCAplot_b1, {
  PCA.df <- data.frame(NZM_RNAseqdata_corplot, row.names = 1)[,c(input$NZM_PCAplot_samples_g1_b1,input$NZM_PCAplot_samples_g2_b1,input$NZM_PCAplot_samples_g3_b1,input$NZM_PCAplot_samples_g4_b1)]
  high_var_genes <- PCA.df %>% apply(1, var) %>% sort(decreasing = TRUE) %>% head(n = input$NZM_PCAplot_topgenes_b1) %>% names
  prcomp.df <- prcomp(t(PCA.df[high_var_genes,]), center = input$NZM_PCAplot_centre_b1, scale = input$NZM_PCAplot_scale_b1)
  prcomp.df
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### get summary prcomp percentage for x and y chosen PC's
NZM_prcomp_summary_b1 <- eventReactive(input$NZM_make_PCAplot_b1, {
  c(summary(NZM_prcomp_df_b1())$importance[2,input$NZM_PCAplot_PCx_b1] * 100, summary(NZM_prcomp_df_b1())$importance[2,input$NZM_PCAplot_PCy_b1] * 100)
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)


### Generate the plot_ly marker plot
NZM_plotly_PCAplot_b1 <- eventReactive(input$NZM_make_PCAplot_b1, {
  (NZM_prcomp_df_b1()$x)[,1:((NZM_prcomp_df_b1()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples") %>% 
    mutate(batch = ifelse(samples %in% NZM_batch1_samples, "batch1", "batch2")) %>% 
    mutate(groups = ifelse(samples %in% input$NZM_PCAplot_samples_g1_b1, "group1", 
                           ifelse(samples %in% input$NZM_PCAplot_samples_g2_b1, "group2", 
                                  ifelse(samples %in% input$NZM_PCAplot_samples_g3_b1,  "group3", "group4")))) %>%  
    plot_ly(symbol = ~groups) %>% 
    add_markers(x = ~get(isolate({input$NZM_PCAplot_PCx_b1})), y = ~get(isolate({input$NZM_PCAplot_PCy_b1})), size = 20, hoverinfo = "text", colors = fut_color[1:4],
              text = ~paste0(input$NZM_PCAplot_PCx_b1, ": ", get(isolate({input$NZM_PCAplot_PCx_b1})), "<br>",
                             input$NZM_PCAplot_PCy_b1, ": ", get(isolate({input$NZM_PCAplot_PCy_b1})), "<br>",
                             'sample: ', samples)) %>% 
    layout(xaxis = list(title = paste0(input$NZM_PCAplot_PCx_b1, " (",NZM_prcomp_summary_b1()[1], "%)")), 
         yaxis = list(title = paste0(input$NZM_PCAplot_PCy_b1, " (",NZM_prcomp_summary_b1()[2], "%)")))
},
ignoreNULL = FALSE,
ignoreInit = TRUE
)

### render gg_corplot
output$NZM_plotly_PCAplot_b1_out <- renderPlotly(NZM_plotly_PCAplot_b1())
#output$NZM_DT_PCA_batch1_out <- renderDT(
#  (NZM_prcomp_df_b1()$x)[,1:((NZM_prcomp_df_b1()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples")
#  )


#### ------------------------------------------------------------------------------------------------------------------

# batch 2

# [,get(input$NZM_PCAplot_batch)]
### make the reactive prcomp dataframe
NZM_prcomp_df_b2 <- eventReactive(input$NZM_make_PCAplot_b2, {
  PCA.df <- data.frame(NZM_RNAseqdata_corplot, row.names = 1)[,c(input$NZM_PCAplot_samples_g1_b2,input$NZM_PCAplot_samples_g2_b2,input$NZM_PCAplot_samples_g3_b2,input$NZM_PCAplot_samples_g4_b2)]
  high_var_genes <- PCA.df %>% apply(1, var) %>% sort(decreasing = TRUE) %>% head(n = input$NZM_PCAplot_topgenes_b2) %>% names
  prcomp.df <- prcomp(t(PCA.df[high_var_genes,]), center = TRUE, scale = TRUE)
  prcomp.df
},
ignoreNULL = FALSE,
ignoreInit = TRUE
)

### get summary prcomp percentage for x and y chosen PC's
NZM_prcomp_summary_b2 <- eventReactive(input$NZM_make_PCAplot_b2, {
  c(summary(NZM_prcomp_df_b2())$importance[2,input$NZM_PCAplot_PCx_b2] * 100, summary(NZM_prcomp_df_b2())$importance[2,input$NZM_PCAplot_PCy_b2] * 100)
},
ignoreNULL = FALSE,
ignoreInit = TRUE
)

### Generate the plot_ly marker plot
NZM_plotly_PCAplot_b2 <- eventReactive(input$NZM_make_PCAplot_b2, {
  (NZM_prcomp_df_b2()$x)[,1:((NZM_prcomp_df_b2()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples") %>% 
    mutate(batch = ifelse(samples %in% NZM_batch1_samples, "batch1", "batch2")) %>% 
    mutate(groups = ifelse(samples %in% input$NZM_PCAplot_samples_g1_b2, "group1", 
                           ifelse(samples %in% input$NZM_PCAplot_samples_g2_b2, "group2", 
                                  ifelse(samples %in% input$NZM_PCAplot_samples_g3_b2,  "group3", "group4")))) %>% 
    plot_ly(symbol = ~groups) %>% 
    add_markers(x = ~get(isolate({input$NZM_PCAplot_PCx_b2})), y = ~get(isolate({input$NZM_PCAplot_PCy_b2})), size = 20, hoverinfo = "text", colors = fut_color[1:4],
                text = ~paste0(input$NZM_PCAplot_PCx_b2, ": ", get(isolate({input$NZM_PCAplot_PCx_b2})), "<br>",
                               input$NZM_PCAplot_PCy_b2, ": ", get(isolate({input$NZM_PCAplot_PCy_b2})), "<br>",
                               'sample: ', samples)) %>% 
    layout(xaxis = list(title = paste0(input$NZM_PCAplot_PCx_b2, " (",NZM_prcomp_summary_b2()[1], "%)")), 
           yaxis = list(title = paste0(input$NZM_PCAplot_PCy_b2, " (",NZM_prcomp_summary_b2()[2], "%)")))
},
ignoreNULL = FALSE,
ignoreInit = TRUE
)

### render gg_corplot
output$NZM_plotly_PCAplot_b2_out <- renderPlotly(NZM_plotly_PCAplot_b2())
#output$NZM_DT_PCA_batch1_out <- renderDT(
#  (NZM_prcomp_df_b2()$x)[,1:((NZM_prcomp_df_b2()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples")
#  )

#### ------------------------------------------------------------------------------------------------------------------

# batch 1 and batch 2

# [,get(input$NZM_PCAplot_batch)]
### make the reactive prcomp dataframe
NZM_prcomp_df_b1and2 <- eventReactive(input$NZM_make_PCAplot_b1and2, {
  PCA.df <- data.frame(NZM_RNAseqdata_corplot, row.names = 1)[,c(input$NZM_PCAplot_samples_g1_b1and2,input$NZM_PCAplot_samples_g2_b1and2,input$NZM_PCAplot_samples_g3_b1and2,input$NZM_PCAplot_samples_g4_b1and2)]
  high_var_genes <- PCA.df %>% apply(1, var) %>% sort(decreasing = TRUE) %>% head(n = input$NZM_PCAplot_topgenes_b1and2) %>% names
  prcomp.df <- prcomp(t(PCA.df[high_var_genes,]), center = TRUE, scale = TRUE)
  prcomp.df
},
ignoreNULL = FALSE,
ignoreInit = TRUE
)

### get summary prcomp percentage for x and y chosen PC's
NZM_prcomp_summary_b1and2 <- eventReactive(input$NZM_make_PCAplot_b1and2, {
  c(summary(NZM_prcomp_df_b1and2())$importance[2,input$NZM_PCAplot_PCx_b1and2] * 100, summary(NZM_prcomp_df_b1and2())$importance[2,input$NZM_PCAplot_PCy_b1and2] * 100)
},
ignoreNULL = FALSE,
ignoreInit = TRUE
)

### Generate the plot_ly marker plot
NZM_plotly_PCAplot_b1and2 <- eventReactive(input$NZM_make_PCAplot_b1and2, {
  (NZM_prcomp_df_b1and2()$x)[,1:((NZM_prcomp_df_b1and2()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples") %>% 
    mutate(batch = ifelse(samples %in% NZM_batch1_samples, "batch1", "batch2")) %>% 
    mutate(groups = ifelse(samples %in% input$NZM_PCAplot_samples_g1_b1and2, "group1", 
                           ifelse(samples %in% input$NZM_PCAplot_samples_g2_b1and2, "group2", 
                                  ifelse(samples %in% input$NZM_PCAplot_samples_g3_b1and2,  "group3", "group4")))) %>% 
    plot_ly(symbol = ~groups) %>% 
    add_markers(x = ~get(isolate({input$NZM_PCAplot_PCx_b1and2})), y = ~get(isolate({input$NZM_PCAplot_PCy_b1and2})), size = 20, hoverinfo = "text", colors = fut_color[1:4],
                text = ~paste0(input$NZM_PCAplot_PCx_b1and2, ": ", get(isolate({input$NZM_PCAplot_PCx_b1and2})), "<br>",
                               input$NZM_PCAplot_PCy_b1and2, ": ", get(isolate({input$NZM_PCAplot_PCy_b1and2})), "<br>",
                               'sample: ', samples)) %>% 
    layout(xaxis = list(title = paste0(input$NZM_PCAplot_PCx_b1and2, " (",NZM_prcomp_summary_b1and2()[1], "%)")), 
           yaxis = list(title = paste0(input$NZM_PCAplot_PCy_b1and2, " (",NZM_prcomp_summary_b1and2()[2], "%)")))
},
ignoreNULL = FALSE,
ignoreInit = TRUE
)

### render gg_corplot
output$NZM_plotly_PCAplot_b1and2_out <- renderPlotly(NZM_plotly_PCAplot_b1and2())
#output$NZM_DT_PCA_batch1_out <- renderDT(
#  (NZM_prcomp_df_b1and2()$x)[,1:((NZM_prcomp_df_b1and2()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples")
#  )





