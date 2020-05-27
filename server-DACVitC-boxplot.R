

DACVitC_RNAseqdata_boxplot_df <- eventReactive(input$DACVitC_make_boxplot_all, {
  filter(DACVitC_RNAseqdata_boxplot, symbols == isolate({input$DACVitC_boxplot_gene1}) & group %in% c("control",input$DACVitC_boxplot_treatmentgroup))
},
ignoreNULL = FALSE, 
ignoreInit = FALSE 
) 

DACVitC_RNAseqdata_boxplot_filtered_df <- eventReactive(input$DACVitC_make_boxplot_all, {
  if(input$DACVitC_boxplot_samples){
    dplyr::filter(DACVitC_RNAseqdata_boxplot_df(), CellLines %in% input$DACVitC_boxplot_samplelist)
  } else {
    DACVitC_RNAseqdata_boxplot_df()
  }
},
ignoreNULL = FALSE, 
ignoreInit = FALSE
)



DACVitC_boxplot_all <- eventReactive(input$DACVitC_make_boxplot_all, {
  if ("DAC" %in% input$DACVitC_boxplot_treatmentgroup &&
      "VitC" %in% input$DACVitC_boxplot_treatmentgroup &&
      "DAC.VitC" %in% input$DACVitC_boxplot_treatmentgroup) {
    my_comparisons <- list(c("control", "DAC"), 
                           c("control", "VitC"),
                           c("control", "DAC.VitC"),
                           c("DAC", "VitC"),
                           c("VitC", "DAC.VitC"),
                           c("DAC", "DAC.VitC"))
  } else if("DAC" %in% input$DACVitC_boxplot_treatmentgroup &&
            "VitC" %in% input$DACVitC_boxplot_treatmentgroup){
    my_comparisons <- list(c("control", "DAC"), 
                          c("control", "VitC"),
                          c("DAC", "VitC"))
  } else if("DAC" %in% input$DACVitC_boxplot_treatmentgroup &&
            "DAC.VitC" %in% input$DACVitC_boxplot_treatmentgroup){
    my_comparisons <- list(c("control", "DAC"), 
                           c("control", "DAC.VitC"),
                           c("DAC", "DAC.VitC"))
  } else if("VitC" %in% input$DACVitC_boxplot_treatmentgroup &&
            "DAC.VitC" %in% input$DACVitC_boxplot_treatmentgroup){
    my_comparisons <- list(c("control", "VitC"),
                           c("control", "DAC.VitC"),
                           c("VitC", "DAC.VitC"))
  } else if(input$DACVitC_boxplot_treatmentgroup == "DAC"){
    my_comparisons <- list(c("control", "DAC"))
  } else if(input$DACVitC_boxplot_treatmentgroup == "VitC"){
    my_comparisons <- list(c("control", "VitC"))
  } else {
    my_comparisons <- list(c("control", "DAC.VitC"))
  }
  # if(input$DACVitC_boxplot_treatmentgroup == "DAC.VitC")
  DACVitC_RNAseqdata_boxplot_filtered_df() %>% 
    ggplot(aes(y = values, x = group, col = group)) + 
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) + 
    geom_point(aes(shape = mut.subtype), alpha = 0.7, size = 5) +
    geom_line(aes(group=lines), col="black") + 
    # geom_jitter(alpha = 1, size = 2, position = position_jitter(seed = 1)) + 
    scale_color_manual(values = c(starT_color[c(2,1,3)], fut_color[4])) + 
    ggtitle(isolate({input$DACVitC_boxplot_gene1})) +
    stat_compare_means(method = "t.test", comparisons = my_comparisons) +
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
output$plot_DACVitC_boxplot_all <- renderPlot({
  validate(
    need(isolate({input$DACVitC_boxplot_gene1}) != '', 'Please choose a gene to make the plot')
  )
  DACVitC_boxplot_all()
})

## --------

# output for downloading the data
output$PDL1_download_boxplot_data_all <- downloadHandler(
  filename = "DACVitC_boxplot_data.csv",
  content = function(file) {
    # Code that creates a file in the path <file>
    write.csv(DACVitC_RNAseqdata_boxplot_filtered_df(), file)
  }
)

## ------
# output for the DT table

output$DT_DACVitC_boxplot <- DT::renderDT(DACVitC_RNAseqdata_boxplot_filtered_df())

## --------------------------------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------------------------------

# paired analysis 


DACVitC_paired_RNAseqdata_boxplot_df <- eventReactive(input$DACVitC_paired_make_boxplot_all, {
  pairedcelllines <- filter(DACVitC_paired_RNAseqdata_boxplot, symbols == isolate({input$DACVitC_paired_boxplot_gene1}) & group %in% c(input$DACVitC_paired_boxplot_treatmentgroup))$CellLines
  filter(DACVitC_paired_RNAseqdata_boxplot, symbols == isolate({input$DACVitC_paired_boxplot_gene1}) & group %in% c("control",input$DACVitC_paired_boxplot_treatmentgroup)) %>% 
    filter(CellLines %in% pairedcelllines)
},
ignoreNULL = FALSE, 
ignoreInit = FALSE 
) 

DACVitC_paired_RNAseqdata_boxplot_filtered_df <- eventReactive(input$DACVitC_paired_make_boxplot_all, {
  if(input$DACVitC_paired_boxplot_samples){
    dplyr::filter(DACVitC_paired_RNAseqdata_boxplot_df(), CellLines %in% input$DACVitC_paired_boxplot_samplelist)
  } else {
    DACVitC_paired_RNAseqdata_boxplot_df()
  }
},
ignoreNULL = FALSE, 
ignoreInit = FALSE
)











