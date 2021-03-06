library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)

tagList(dashboardPage(skin = "blue",
    dashboardHeader(
    title = "NZM Cell Lines Bioportal",
    titleWidth = 500,
    dropdownMenu(
        type = "messages",
        messageItem(
            from = "Cancer Hub",
            message = "Go to CancerHub",
            icon = icon("book"),
            time = "Published 2020-05-28",
            href = "https://cancerhub.net/"
        ),
        messageItem(
            from = "Source Code",
            message = "Available on Github",
            time = "Update at 2020-05-28",
            href = "https://github.com/AntonioAhn/CancerHub_ShinyApp"
        ),
        messageItem(
            from = "About Us",
            message = "Go to the Eccles Homepage",
            icon = icon("users"),
            href = "https://www.otago.ac.nz/dsm-pathology/research/otago114692.html"
        ),
        icon = icon("info-circle"),
        headerText = "INFORMATIONS"
    )
    ),
    dashboardSidebar(
      sidebarMenu(id = "side_bar",
                  menuItem(text = "Information",
                             tabName = "Info"),
                  menuItem(text = "NZM cell lines",
                             tabName = "RNAseq_NZM", 
                             badgeLabel = "RNAseq",
                             badgeColor = "blue"),
                  menuItem(text = "PD-L1 cell lines",
                             tabName = "RNAseq_PDL1",
                             badgeLabel = "RNAseq",
                             badgeColor = "blue"),
                  menuItem(text = "DAC/VitC treated",
                           tabName = "RNAseq_DACVitC",
                           badgeLabel = "RNAseq",
                           badgeColor = "blue"),
                  menuItem(text = "NZM cell lines",
                           tabName = "Exome_NZM",
                           badgeLabel = "Exomeseq",
                           badgeColor = "blue"),
                  menuItem(text = "NZM cell lines",
                           tabName = "RRBS_NZM",
                           badgeLabel = "DNA methylation",
                           badgeColor = "blue")
        )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      tabItems(
        tabItem(tabName = "RRBS_NZM"),
        tabItem(tabName = "Exome_NZM"),
        tabItem(tabName = "Info", tabBox(title = "", width = NULL,
                                         tabPanel(title = "Welcome to CanerHub",
                                                  icon = icon("info"),
                                                  fluidRow(column(includeMarkdown("document/Information.md"), width = 12, offset = 0))),
                                         tabPanel(title = "NZM cell lines information",
                                                  icon = icon("info"),
                                                  fluidRow(column(includeMarkdown("document/NZMcellLines.md"), width = 12, offset = 0))),
                                         tabPanel(title = "PDL1 cell lines information",
                                                  icon = icon("info"),
                                                  fluidRow(column(includeMarkdown("document/PDL1cellLines.md"), width = 12, offset = 0)))
                                         )),
        tabItem(tabName = "RNAseq_NZM",
                tabBox(title = "", width = NULL, 
                       tabPanel(title = "Boxplot all samples",
                                source(
                                  file = "ui-NZM-boxplot.R",
                                  local = TRUE,
                                  encoding = "UTF-8"
                                  )
                           ),
                       tabPanel(title = "correlation plot",
                                source(
                                  file = "ui-NZM-corplot.R",
                                  local = TRUE,
                                  encoding = "UTF-8"
                                  )
                                ),
                       tabPanel(title = "PCA plot",
                                source(
                                  file = "ui-NZM-PCA.R",
                                  local = TRUE,
                                  encoding = "UTF-8"
                                  )
                                ),
                       tabPanel(title = "heatmap (sample and gene hclustering)",
                                source(
                                  file = "ui-NZM-hclust.R",
                                  local = TRUE,
                                  encoding = "UTF-8"
                                )
                       )
                    
                    )
            ),
        tabItem(tabName = "RNAseq_PDL1",
                tabBox(title = "", width = NULL, 
                       tabPanel(title = "Boxplot all samples",
                                source(
                                  file = "ui-PDL1-boxplot.R",
                                  local = TRUE,
                                  encoding = "UTF-8"
                                )
                       ),
                        tabPanel(title = "correlation plot",
                                source(
                                  file = "ui-PDL1-corplot.R",
                                  local = TRUE,
                                  encoding = "UTF-8"
                                )
                       ),
                       tabPanel(title = "PCA plot",
                                source(
                                  file = "ui-PDL1-PCA.R",
                                  local = TRUE,
                                  encoding = "UTF-8"
                                  )
                                ),
                       tabPanel(title = "heatmap (sample and gene hclustering)",
                                source(
                                  file = "ui-PDL1-hclust.R",
                                  local = TRUE,
                                  encoding = "UTF-8"
                                  )
                                )
                       )
                ),
        tabItem(tabName = "RNAseq_DACVitC",
                tabBox(title = "", width = NULL, 
                       tabPanel(title = "Boxplot all samples",
                                source(
                                  file = "ui-DACVitC-boxplot.R",
                                  local = TRUE,
                                  encoding = "UTF-8"
                                )
                       )
                )
        )
      )
    )
),
tags$footer(
    tags$p("Copyright © 2019"), 
    tags$a(" maurice wilkins centre", href = "http://www.mauricewilkinscentre.org/"),
    tags$a(" Cancer Hub", href = "https://cancerhub.net/"),
#    tags$a(" Eccles Lab ", href = "https://www.otago.ac.nz/dsm-pathology/research/otago114692.html"), 
#   tags$a(" Chatterjee Lab ", href = "https://www.otago.ac.nz/chatterjee-lab/index.html"),
#    tags$a(" University of Otago ", href = "https://www.otago.ac.nz/"), 
    tags$p("Version 13/04/20"),
    style = "
  bottom:0;
  width:100%;
  color: #B8C7CE;
  padding: 10px;
  background-color: #222D32;
  z-index: 1000;"
    )
)
                         