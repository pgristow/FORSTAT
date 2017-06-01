create_tab6_page <- function(session) {
  tab_page <- fluidPage(
    useShinyjs(),
    ####Main heading on page
    tags$h4("Graphing options"),
    tags$br(),
    
    
    ####Select a resoultion for the images using a slider bar
    sliderInput(inputId = "plot_res",
                label = "Resolution (dpi)" , min = 400, max = 1600, value = 800 ,step = 100),
    
    ####Select the width of the images
    style="display:inline-block",
    numericInput(inputId = "plot_width", label = "Width", value = 9000, width = "30%"),
    
    ####Select the height of the images
    style="display:inline-block",
    numericInput(inputId = "plot_height", label = "Height", value = 4800, width = "30%"),
    
    ####Select the units for height and width of images
    radioButtons(inputId = "plot_units", 
                 label = "Units", choices = c("px", "cm", "in", "mm"), 
                 inline = T, width = "100%"),
    
    downloadButton(outputId = "dlfreqplot",label = "Download frequency plots"),
    tags$br(),
    downloadButton(outputId = "dlAll",label = "Download forensic plots"),
    tags$br(),
    downloadButton(outputId = "dlmetrics",label = "Download metrics"),
    tags$br(),
    tags$hr()
  )
}