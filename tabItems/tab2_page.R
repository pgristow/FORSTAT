create_tab2_page <- function(session) {
  tab_page <- fluidPage(
    useShinyjs(),
    ####Main heading of tab
    tags$h1("Allelic frequencies"),
    tags$br(),
    
    
         ####Initiate panels for tab2
         bsCollapse(id = "collapse_tab2", open = "Total allele numbers",
                    
                    ####Operators for allele counts
                    bsCollapsePanel("Total allele numbers",
                                    
                                    ####Heading of panel
                                    tags$h4("Display and download number of alleles"),
                                    
                                    ####Display the allele
                                    actionButton(inputId = "al_disp", label = "Compute")
                                    
                                    ####Download the number of alleles
                                    # disabled(downloadButton('al_dl', 'Download'))
                                     ),
                                    
                                    ####Operator to display allelic frequencies
                    bsCollapsePanel("Allelic frequencies and heatmaps",
                                    uiOutput("input_text")
                                    
                    )
                    )
         )
}