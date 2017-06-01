create_tab4_page <- function(session) {
  tab_page <- fluidPage(
    useShinyjs(),
    tags$h1("Population genetic statistics"),
    
    bsCollapse(id = "collapse_tab4", open = "Probability of exclusion (PE)",
               
               bsCollapsePanel("Probability of exclusion (PE)", 
                               tags$h4("Display and download probability of exclusion"),
                               actionButton(inputId = "PE_disp", label = "Compute")
                               #downloadButton('PE_dl', 'Download')
               ),              
               
               bsCollapsePanel("Random Match Probability (RMP)", 
                               tags$h4("Display and download random match probability"),
                               actionButton(inputId = "RMP_disp", label = "Compute")
                               #downloadButton('RMP_dl', 'Download'),
                               #downloadButton('RMP_plot_dl', 'Download plots')
               ),
               
               bsCollapsePanel("Discrimination Capacity (DC)", tags$h4("Display and download discrimination capacity"),
                               actionButton(inputId = "DC_disp", label = "Compute")
                               #downloadButton('DC_dl', 'Download'),
                               #downloadButton('DC_plot_dl', 'Download plots')
               )
    )
    )
}