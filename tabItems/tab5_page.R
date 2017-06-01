create_tab5_page <- function(session) {
  tab_page <- fluidPage(
    useShinyjs(),
    tags$h1("Paternity statistics"),
    tags$br(),
              bsCollapse(id = "collapse_tab5", open = "Paternity index (PI)",
              
              bsCollapsePanel("Paternity index (PI)", tags$h4("Display and download the paternity index"),
                              actionButton(inputId = "PI_disp", label = "Compute")
                                      # downloadButton('PI_dl', 'Download')
                              )
              ))
  }