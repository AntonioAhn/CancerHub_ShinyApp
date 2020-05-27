
### make the reactive prcomp dataframe
PDL1_prcomp_df <- eventReactive(input$PDL1_make_PCAplot, {
  PCA.df <- data.frame(PDL1_RNAseqdata_corplot, row.names = 1)[,c(input$PDL1_PCAplot_samples_g1,input$PDL1_PCAplot_samples_g2,input$PDL1_PCAplot_samples_g3,input$PDL1_PCAplot_samples_g4)]
  high_var_genes <- PCA.df %>% apply(1, var) %>% sort(decreasing = TRUE) %>% head(n = input$PDL1_PCAplot_topgenes) %>% names
  prcomp.df <- prcomp(t(PCA.df[high_var_genes,]), center = input$PDL1_PCAplot_centre, scale = input$PDL1_PCAplot_scale)
  prcomp.df
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)

### get summary prcomp percentage for x and y chosen PC's
PDL1_prcomp_summary <- eventReactive(input$PDL1_make_PCAplot, {
  c(summary(PDL1_prcomp_df())$importance[2,input$PDL1_PCAplot_PCx] * 100, summary(PDL1_prcomp_df())$importance[2,input$PDL1_PCAplot_PCy] * 100)
},
ignoreNULL = TRUE,
ignoreInit = FALSE
)


### Generate the plot_ly marker plot
PDL1_plotly_PCAplot <- eventReactive(input$PDL1_make_PCAplot, {
  (PDL1_prcomp_df()$x)[,1:((PDL1_prcomp_df()$x) %>% colnames %>% length())] %>% as_tibble(rownames = "samples") %>% 
    mutate(groups = ifelse(samples %in% input$PDL1_PCAplot_samples_g1, "group1", 
                           ifelse(samples %in% input$PDL1_PCAplot_samples_g2, "group2", 
                                  ifelse(samples %in% input$PDL1_PCAplot_samples_g3,  "group3", "group4")))) %>%  
    plot_ly(symbol = ~groups) %>% 
    add_markers(x = ~get(isolate({input$PDL1_PCAplot_PCx})), y = ~get(isolate({input$PDL1_PCAplot_PCy})), size = 20, hoverinfo = "text", colors = fut_color[1:4],
                text = ~paste0(input$PDL1_PCAplot_PCx, ": ", get(isolate({input$PDL1_PCAplot_PCx})), "<br>",
                               input$PDL1_PCAplot_PCy, ": ", get(isolate({input$PDL1_PCAplot_PCy})), "<br>",
                               'sample: ', samples)) %>% 
    layout(xaxis = list(title = paste0(input$PDL1_PCAplot_PCx, " (",PDL1_prcomp_summary()[1], "%)")), 
           yaxis = list(title = paste0(input$PDL1_PCAplot_PCy, " (",PDL1_prcomp_summary()[2], "%)")))
},
ignoreNULL = TRUE,
ignoreInit = TRUE
)

### render gg_corplot
output$PDL1_plotly_PCAplot_out <- renderPlotly(PDL1_plotly_PCAplot())



#### ------------------------------------------------------------------------------------------------------------------

