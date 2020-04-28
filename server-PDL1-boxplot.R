
## first boxplot (all samples)

PDL1_boxplot_all_df <- eventReactive(input$PDL1_make_boxplot_all, {filter(PDL1_RNAseqdata_boxplot, 
                                                                          symbols == input$PDL1_boxplot_gene1, 
                                                                          mut.subtype %in% input$PDL1_boxplot_mutation, 
                                                                          PDL1.subtype %in% input$PDL1_boxplot_PDL1,
                                                                          batch %in% input$PDL1_boxplot_batch
                                                                          )},
                                     ignoreNULL = FALSE, 
                                     ignoreInit = FALSE)


PDL1_boxplot_all <- reactive({PDL1_boxplot_all_df() %>% 
    plot_ly(y = ~values,  type = "box", boxpoints = "all", jitter = 1, hoverinfo = "text", color = I(fut_color[2]), pointpos = 0,
            text = ~paste0(isolate({input$PDL1_boxplot_gene1}), ": ",values, "<br>",
                           'sample: ', samples, "<br>",
                           'mut.subtype: ', mut.subtype, "<br>",
                           'batch.group: ', batch, "<br>",
                           'PDL1.subtype: ', PDL1.subtype, "<br>")) %>% 
    layout(yaxis = list(title = paste(isolate({input$PDL1_boxplot_gene1}), "(log2 TPM)")), xaxis = list(showticklabels = FALSE, title = ""), showlegend = FALSE)
})


# render boxplot
output$plotly_PDL1_boxplot_all <- plotly::renderPlotly(PDL1_boxplot_all())

## --------

# output for downloading the data
output$PDL1_download_boxplot_data_all <- downloadHandler(
  filename = "PDL1_boxplot_data.csv",
  content = function(file) {
    # Code that creates a file in the path <file>
    write.csv(PDL1_boxplot_all_df(), file)
  }
)

## ------
# output for the DT table

output$DT_PDL1_boxplot <- DT::renderDT(PDL1_boxplot_all_df())

## -------------------------------------------------------------------------



## second boxplot (with selected samples)

# , batch %in% input$boxplot_batch (removed) PDL1_RNAseqdata_boxplot_new. This was removed
PDL1_boxplot_groups_df <- eventReactive(input$PDL1_make_boxplot_groups, {
  filter(PDL1_RNAseqdata_boxplot, symbols == input$PDL1_boxplot_gene2) %>% 
    mutate(newgroup = ifelse(samples %in% input$PDL1_sampleselect_g1_boxplot, "group1",
                             ifelse(samples %in% input$PDL1_sampleselect_g2_boxplot, "group2",
                                    ifelse(samples %in% input$PDL1_sampleselect_g3_boxplot, "group3",
                                           ifelse(samples %in% input$PDL1_sampleselect_g4_boxplot, "group4", "rest"))))) %>% 
    filter(newgroup %in% c("group1", "group2", "group3","group4"))
}, 
ignoreNULL = FALSE, 
ignoreInit = FALSE
)


PDL1_boxplot_groups <- eventReactive(input$PDL1_make_boxplot_groups, {PDL1_boxplot_groups_df() %>% 
    plot_ly(color = ~newgroup) %>% 
    add_trace(x = ~as.numeric(factor(newgroup)), y = ~values, type = "box",  hoverinfo = 'name+y', colors =
                fut_color[1:4]) %>% 
    add_markers(x = ~jitter(as.numeric(factor(newgroup))), y = ~as.numeric(values), colors = fut_color[1:4],
                marker = list(size = 6),
                hoverinfo = "text",
                showlegend = FALSE,
                text = ~paste0(input$PDL1_boxplot_gene2, ": ",values, "<br>",
                               'sample: ', samples,"<br>",
                               'mutation: ', mut.subtype,"<br>",
                               'PDL1: ', PDL1.subtype)) %>% 
    layout(yaxis = list(title = paste(input$PDL1_boxplot_gene2, "(log2 TPM)")), 
           xaxis = list(title = "", showticklabels = FALSE),
           legend = list(orientation = "h", x =0.5, xanchor = "center", y = 1, yanchor = "bottom"))
})



# render boxplot
output$PDL1_boxplot_groups <- plotly::renderPlotly({
  validate(
    need(input$PDL1_boxplot_gene2 != '', 'Please choose a gene for the group analysis')
  )
  PDL1_boxplot_groups()
})

## --------

# output for downloading the data
output$PDL1_download_boxplot_data_groups <- downloadHandler(
  filename = "PDL1_boxplot_groups_data.csv",
  content = function(file) {
    # Code that creates a file in the path <file>
    write.csv(PDL1_boxplot_groups_df(), file)
  }
)


## -------------------------------------------------------------------------

output$DT_PDL1_boxplot_groups <- DT::renderDT(PDL1_boxplot_groups_df())



