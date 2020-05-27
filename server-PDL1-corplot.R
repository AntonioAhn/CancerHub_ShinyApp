
# For correlation plot where all samples are looked at 
### make the reactive dataframe
PDL1_RNAseqdata_corplot_df <- eventReactive(input$PDL1_make_corplot, {
  PDL1_RNAseqdata_corplot %>% 
    filter(symbols %in% c(isolate({input$PDL1_corplot_gene1}), isolate({input$PDL1_corplot_gene2}), isolate({input$PDL1_corplot_select_gene1}))) %>% 
    data.frame(row.names = 1) %>% 
    t %>% 
    as_tibble(rownames = "samples") %>% 
    mutate(mut.subtype = PDL1_RNAseqdata_sampleinfo$mut.subtype[match(samples, PDL1_RNAseqdata_sampleinfo$newnames)])
  
}, 
ignoreNULL = TRUE,
ignoreInit = FALSE)

## -----------
### make the reactive plotly named _corplot
PDL1_plotly_corplot <- eventReactive(input$PDL1_make_corplot ,{
  if(input$PDL1_corplot_optional_gene3 == "none"){
  PDL1_RNAseqdata_corplot_df() %>% 
    plot_ly(x = ~get(isolate({input$PDL1_corplot_gene1})), y = ~get(isolate({input$PDL1_corplot_gene2})), hoverinfo = "text",
            text = ~paste0(isolate({input$PDL1_corplot_gene1}), ": ", get(isolate({input$PDL1_corplot_gene1})), "<br>",
                           isolate({input$PDL1_corplot_gene2}), ": ", get(isolate({input$PDL1_corplot_gene2})), "<br>",
                           'sample: ', samples, "<br>")) %>% 
    layout(xaxis = list(title = paste(isolate({input$PDL1_corplot_gene1}), "(log2 TPM)")), 
           yaxis = list(title = paste(isolate({input$PDL1_corplot_gene2}), "(log2 TPM)"))) %>% 
    add_markers(size = 20)
  } else if(input$PDL1_corplot_optional_gene3 == "third_gene"){
    PDL1_RNAseqdata_corplot_df() %>% 
    plot_ly(x = ~get(isolate({input$PDL1_corplot_gene1})), y = ~get(isolate({input$PDL1_corplot_gene2})), hoverinfo = "text", color = ~get(isolate({input$PDL1_corplot_select_gene1})),
            mode = "markers", type = "scatter", size = 20,
            text = ~paste0(isolate({input$PDL1_corplot_gene1}), ": ", get(isolate({input$PDL1_corplot_gene1})), "<br>",
                           isolate({input$PDL1_corplot_gene2}), ": ", get(isolate({input$PDL1_corplot_gene2})), "<br>",
                           'sample: ', samples, "<br>")) %>% 
      colorbar(title = input$PDL1_corplot_select_gene1) %>% 
      layout(xaxis = list(title = paste(isolate({input$PDL1_corplot_gene1}), "(log2 TPM)")), 
             yaxis = list(title = paste(isolate({input$PDL1_corplot_gene2}), "(log2 TPM)")))
  } else {
    PDL1_RNAseqdata_corplot_df() %>% 
    plot_ly(x = ~get(isolate({input$PDL1_corplot_gene1})), y = ~get(isolate({input$PDL1_corplot_gene2})), hoverinfo = "text", symbol = ~get(isolate({input$PDL1_corplot_mutgrp})),
            mode = "markers", type = "scatter", size = 20,
            text = ~paste0(isolate({input$PDL1_corplot_gene1}), ": ", get(isolate({input$PDL1_corplot_gene1})), "<br>",
                           isolate({input$PDL1_corplot_gene2}), ": ", get(isolate({input$PDL1_corplot_gene2})), "<br>",
                           'sample: ', samples, "<br>")) %>% 
      colorbar(title = input$PDL1_corplot_select_gene1) %>% 
      layout(xaxis = list(title = paste(isolate({input$PDL1_corplot_gene1}), "(log2 TPM)")), 
             yaxis = list(title = paste(isolate({input$PDL1_corplot_gene2}), "(log2 TPM)")))
  }
  },
  ignoreNULL = TRUE,
  ignoreInit = FALSE
)
    

### render gg_corplot
output$PDL1_plotly_corplot_out <- renderPlotly(PDL1_plotly_corplot())

## -----------
### generate correlation values 
PDL1_corvalues <- eventReactive(input$PDL1_make_corplot, {
  df <-  PDL1_RNAseqdata_corplot_df()
  corvalue <- cor(df[[input$PDL1_corplot_gene1]],  df[[input$PDL1_corplot_gene2]])
  corpvalue <- cor.test(df[[input$PDL1_corplot_gene1]],  df[[input$PDL1_corplot_gene2]])$p.value
  paste("Pearson correlation value =",corvalue , "<br>", "pvalue =", corpvalue)
}, 
ignoreNULL = FALSE,
ignoreInit = FALSE)

output$PDL1_corvalues_out <- renderText(HTML(PDL1_corvalues()))

## -----------
# output for data table

output$DT_PDL1_corplot_df <- DT::renderDT(PDL1_RNAseqdata_corplot_df())

## -----------
# output for downloading the data

output$PDL1_download_corplot_data <- downloadHandler(
  filename = "PDL1_corplot_data.csv",
  content = function(file) {
    # Code that creates a file in the path <file>
    write.csv(PDL1_RNAseqdata_corplot_df(), file)
  }
)

## --------------------------------------------------------------------------------

# Co-expression analysis

PDL1_RNAseqdata_coexp_df <- eventReactive(input$PDL1_make_coexp,{
  df <- PDL1_RNAseqdata_corplot %>% data.frame(row.names = TRUE) %>% as.matrix
  # taking out the genes (1003 genes, leaving 23610 genes left) with a row variance of 0. These genes cannot have a correlation value therefore warning meassages that comes up seems to slow down this function
  df <- df[(apply(df, 1, var)) != 0,]
  geneexpr <- df[input$PDL1_coexp_gene1,]
  cor.df <- apply(df, 1, function(x){cor(geneexpr, x, method = input$PDL1_coexp_method) }) %>% 
    sort(decreasing=TRUE) %>% 
    head(n=1001) %>% 
    tail(n=1000) %>% data.frame
  colnames(cor.df) <- "cor"
  cor.df$p.value <- apply(df[rownames(cor.df),], 1, function(x){cor.test(geneexpr, x, method = input$PDL1_coexp_method)$p.value})
  return(cor.df)
}, 
ignoreNULL = FALSE,
ignoreInit = TRUE)


output$DT_PDL1_coexp_df <- DT::renderDT({PDL1_RNAseqdata_coexp_df()})


## -----------
# output for downloading the data

output$PDL1_download_coexp_data <- downloadHandler(
  filename = "PDL1_coexp_data.csv",
  content = function(file) {
    # Code that creates a file in the path <file>
    write.csv(PDL1_RNAseqdata_coexp_df(), file)
  }
)



## ----------------------------------------------------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------------------------------------------------

# For correlation plot where only selected samples are looked at 
### make the reactive dataframe
PDL1_RNAseqdata_corplot_samples_df <- eventReactive(input$PDL1_make_corplot_samples, {
  PDL1_RNAseqdata_corplot %>% 
    filter(symbols %in% c(isolate({input$PDL1_corplot_select_gene1}), isolate({input$PDL1_corplot_select_gene2}))) %>% 
    data.frame(row.names = 1) %>% 
    t %>% 
    as_tibble(rownames = "samples") %>% 
    mutate(mut.subtype = PDL1_RNAseqdata_sampleinfo$mut.subtype[match(samples, PDL1_RNAseqdata_sampleinfo$newnames)])
}, 
ignoreNULL = FALSE,
ignoreInit = TRUE)

## -----------
### make the reactive plotly named _corplot

PDL1_plotly_corplot_samples <- eventReactive(input$PDL1_make_corplot_samples ,{
  if(input$PDL1_corplot_optional_select_gene3 == "none"){
  PDL1_RNAseqdata_corplot_samples_df() %>% 
    filter(samples %in% input$PDL1_sampleselect_g1_corplot) %>% 
    plot_ly(x = ~get(isolate({input$PDL1_corplot_select_gene1})), y = ~get(isolate({input$PDL1_corplot_select_gene2})), hoverinfo = "text",
            text = ~paste0(input$PDL1_corplot_select_gene1, ": ", get(isolate({input$PDL1_corplot_select_gene1})), "<br>",
                           input$PDL1_corplot_select_gene2, ": ", get(isolate({input$PDL1_corplot_select_gene2})), "<br>",
                           'sample: ', samples)) %>% 
    layout(xaxis = list(title = paste(isolate({input$PDL1_corplot_select_gene1}), "(log2 TPM)")), 
           yaxis = list(title = paste(isolate({input$PDL1_corplot_select_gene2}), "(log2 TPM)"))
    ) %>% 
    add_markers(size = 20)
  } else if(input$PDL1_corplot_optional_select_gene3 == "third_gene"){
    PDL1_RNAseqdata_corplot_samples_df() %>% 
      filter(samples %in% input$PDL1_sampleselect_g1_corplot) %>% 
      plot_ly(x = ~get(isolate({input$PDL1_corplot_select_gene1})), y = ~get(isolate({input$PDL1_corplot_select_gene2})), color = ~get(isolate({input$PDL1_corplot_select_gene3})), hoverinfo = "text",
              mode = "markers", type = "scatter", size = 20, 
              text = ~paste0(input$PDL1_corplot_select_gene1, ": ", get(isolate({input$PDL1_corplot_select_gene1})), "<br>",
                             input$PDL1_corplot_select_gene2, ": ", get(isolate({input$PDL1_corplot_select_gene2})), "<br>",
                             'sample: ', samples)) %>% 
      colorbar(title = input$PDL1_corplot_select_gene3) %>% 
      layout(xaxis = list(title = paste(isolate({input$PDL1_corplot_select_gene1}), "(log2 TPM)")), 
             yaxis = list(title = paste(isolate({input$PDL1_corplot_select_gene2}), "(log2 TPM)"))
      )
  } else {
    PDL1_RNAseqdata_corplot_samples_df() %>% 
      filter(samples %in% input$PDL1_sampleselect_g1_corplot) %>% 
      plot_ly(x = ~get(isolate({input$PDL1_corplot_select_gene1})), y = ~get(isolate({input$PDL1_corplot_select_gene2})), symbol = ~get(isolate({input$PDL1_corplot_select_mutgrp})), hoverinfo = "text",
              mode = "markers", type = "scatter", size = 20, 
              text = ~paste0(input$PDL1_corplot_select_gene1, ": ", get(isolate({input$PDL1_corplot_select_gene1})), "<br>",
                             input$PDL1_corplot_select_gene2, ": ", get(isolate({input$PDL1_corplot_select_gene2})), "<br>",
                             'sample: ', samples)) %>% 
      layout(xaxis = list(title = paste(isolate({input$PDL1_corplot_select_gene1}), "(log2 TPM)")), 
             yaxis = list(title = paste(isolate({input$PDL1_corplot_select_gene2}), "(log2 TPM)"))
      )
    }
  }, 
ignoreNULL = FALSE, 
ignoreInit = TRUE)


### render gg_corplot
output$PDL1_plotly_corplot_samples_out <- plotly::renderPlotly({PDL1_plotly_corplot_samples()})


## -----------
### generate correlation values 
PDL1_corvalues_samples <- eventReactive(input$PDL1_make_corplot_samples, {
  df <-  PDL1_RNAseqdata_corplot_samples_df() %>% filter(samples %in% input$PDL1_sampleselect_g1_corplot)
  corvalue <- cor(df[[input$PDL1_corplot_select_gene1]],  df[[input$PDL1_corplot_select_gene2]])
  corpvalue <- cor.test(df[[input$PDL1_corplot_select_gene1]],  df[[input$PDL1_corplot_select_gene2]])$p.value
  paste("Pearson correlation value =",corvalue ,"pvalue =", corpvalue)
}, 
ignoreNULL = FALSE,
ignoreInit = TRUE)

output$PDL1_corvalues_samples_out <- renderText(PDL1_corvalues_samples())

## ------
# datatable for the samples df
output$DT_PDL1_corplot_samples_df <- DT::renderDT(PDL1_RNAseqdata_corplot_samples_df() %>% 
                                                    filter(samples %in% input$PDL1_sampleselect_g1_corplot))

###------
# output for downloading the data

output$PDL1_download_corplot_data_samples <- downloadHandler(
  filename = "PDL1_corplot_data.csv",
  content = function(file) {
    # Code that creates a file in the path <file>
    write.csv(PDL1_RNAseqdata_corplot_samples_df(), file)
  }
)


