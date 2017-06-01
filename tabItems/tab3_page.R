create_tab3_page <- function(session) {
  tab_page <- fluidPage(
    useShinyjs(),
    tags$h1("Allele specific statistics"),
    
    bsCollapse(id = "collapse_tab3", open = "Duplicated genotypes",
               
               bsCollapsePanel("Duplicated genotypes", 
                               tags$h4("Display and download duplicated genotypes"),
                               actionButton(inputId = "Dupli_geno", label = "Compute"),
                               downloadButton('Dupli_geno_dl', 'Download')
               ),              
               
               bsCollapsePanel("Polymorphic information capacity (PIC)", 
                               tags$h4("Display and download polymorphic information capacity (PIC)"),
                               actionButton(inputId = "PIC_disp", label = "Compute")
                               #downloadButton('Al_spec_dl', 'Download'),
                               #downloadButton('Al_spec_plot_dl', 'Download plots')
               ),
               
               bsCollapsePanel("Expected heterozygosity (He)", 
                               tags$h4("Display and download expected heterozygosity (He)"),
                               actionButton(inputId = "He_disp", label = "Compute")
                               #downloadButton('He_spec_dl', 'Download'),
                               #downloadButton('He_spec_plot_dl', 'Download plots')
               ),
               
               bsCollapsePanel("Observed heterozygosity (Ho)", 
                               tags$h4("Display and download observed heterozygosity (Ho)"),
                               actionButton(inputId = "H_obs_disp", label = "Compute")
                               #downloadButton('Hobs_spec_dl', 'Download'),
                               #downloadButton('Hobs_spec_plot_dl', 'Download plots')
               ),
               
               bsCollapsePanel("Observed homozygosity (HObs)", 
                               tags$h4("Display and download observed homozygosity (HObs)"),
                               actionButton(inputId = "Homozy_disp", label = "Compute")
                               #downloadButton('Homo_spec_dl', 'Download'),
                               #downloadButton('Homo_spec_plot_dl', 'Download plots')
               )
    )
  )
}