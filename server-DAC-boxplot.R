

DAC_RNAseqdata_boxplot_df <- eventReactive(input$DAC_make_boxplot_all, {
  filter(DAC_RNAseqdata_boxplot, symbols == isolate({input$DAC_boxplot_gene1}))
},
ignoreNULL = FALSE, 
ignoreInit = TRUE 
) 


DAC_boxplot_all <- eventReactive(input$DAC_make_boxplot_all, {
  DAC_RNAseqdata_boxplot_df() %>% 
    ggplot(aes(y = values, x = group, col = group)) + 
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) + 
    geom_point(aes(shape = mut.subtype), alpha = 0.7, size = 5) +
    geom_line(aes(group=lines), col="black") + 
    # geom_jitter(alpha = 1, size = 2, position = position_jitter(seed = 1)) + 
    scale_color_manual(values = starT_color[c(2,1)]) + 
    ggtitle(isolate({input$DAC_boxplot_gene1})) +
    stat_compare_means(paired=TRUE, method = "t.test", comparisons = list(c("control", "DAC"))) +
    # position = position_jitter(seed = 1) taken out  
    geom_text_repel(aes(label = samples), size = 3, vjust = 2, show.legend = FALSE, alpha = 1) +
    theme_bw() + 
    scale_y_continuous("gene expression (log2 (TPM + 1))") +
    theme(axis.text=element_text(size=12),
          axis.title.x.bottom=element_blank(),
          axis.title.y=element_text(size=12), legend.position = "bottom", plot.title = element_text(hjust=0.5,size=15), legend.title = element_blank())
},
ignoreNULL = FALSE, 
ignoreInit = TRUE 
) 


# render boxplot
output$plot_DAC_boxplot_all <- renderPlot({
  validate(
    need(isolate({input$DAC_boxplot_gene1}) != '', 'Please choose a gene to make the plot')
  )
  DAC_boxplot_all()
})

## --------

# output for downloading the data
output$PDL1_download_boxplot_data_all <- downloadHandler(
  filename = "DAC_boxplot_data.csv",
  content = function(file) {
    # Code that creates a file in the path <file>
    write.csv(DAC_RNAseqdata_boxplot_df(), file)
  }
)

## ------
# output for the DT table

output$DT_DAC_boxplot <- DT::renderDT(DAC_RNAseqdata_boxplot_df())

## -------------------------------------------------------------------------



## second boxplot (with selected samples)

# , batch %in% input$boxplot_batch (removed) DAC_RNAseqdata_boxplot_new. This was removed
DAC_boxplot_groups_df <- eventReactive(input$DAC_make_boxplot_groups, {
  filter(DAC_RNAseqdata_boxplot, symbols == input$DAC_boxplot_gene2 & CellLines %in% input$DAC_sampleselect_g1_boxplot) 
#  %>% mutate(newgroup = ifelse(samples %in% input$DAC_sampleselect_g1_boxplot, "group", "rest")) %>% 
#    filter(newgroup %in% c("group"))
}, 
ignoreNULL = FALSE, 
ignoreInit = FALSE
)


DAC_boxplot_groups <- eventReactive(input$DAC_make_boxplot_groups, {DAC_boxplot_groups_df() %>% 
    ggplot(aes(y = values, x = group, col = group)) + 
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) + 
    geom_point(aes(shape = mut.subtype), alpha = 0.7, size = 5) +
    geom_line(aes(group=lines), col="black") + 
    # geom_jitter(alpha = 1, size = 2, position = position_jitter(seed = 1)) + 
    scale_color_manual(values = starT_color[c(2,1)]) + 
    ggtitle(isolate({input$DAC_boxplot_gene1})) +
    stat_compare_means(paired=TRUE, method = "t.test", comparisons = list(c("control", "DAC"))) +
    # position = position_jitter(seed = 1) taken out  
    geom_text_repel(aes(label = samples), size = 3, vjust = 2, show.legend = FALSE, alpha = 1) +
    theme_bw() + 
    scale_y_continuous("gene expression (log2 (TPM + 1))") +
    theme(axis.text=element_text(size=12),
          axis.title.x.bottom=element_blank(),
          axis.title.y=element_text(size=12), legend.position = "bottom", plot.title = element_text(hjust=0.5,size=15), legend.title = element_blank())
})



# render boxplot
output$DAC_boxplot_groups <- renderPlot({
  validate(
    need(isolate({input$DAC_boxplot_gene2}) != '', 'Please choose a gene to make the plot')
  )
  DAC_boxplot_groups()
})

## --------

# output for downloading the data
output$DAC_download_boxplot_data_groups <- downloadHandler(
  filename = "DAC_boxplot_groups_data.csv",
  content = function(file) {
    # Code that creates a file in the path <file>
    write.csv(DAC_boxplot_groups_df(), file)
  }
)


## -------------------------------------------------------------------------

output$DT_DAC_boxplot_groups <- DT::renderDT(DAC_boxplot_groups_df())



