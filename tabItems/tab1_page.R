create_tab1_page <- function(session) {
  
  tab_page <- fluidPage(
    tags$style(type="text/css", ".recalculating {opacity:0;}"),
    useShinyjs(),
    ####Main title
    tags$h1("Input, and view data"),
    tags$br(),
    
    ####Create a collapse panel group
    
    bsCollapse(id = "collapse_tab1", open = "1. Load and view your data", ####anything after this comma moves into the group
               
               ####Collapse panels to reduce the clutter
               ####Panel 1 is used to load client data
               ####Data should be in genepop format for initial release
               
               bsCollapsePanel("1. Load and view your data",
                               
                               ####Read the data in using fileInput
                               ####Pressing this button activates reactive for "dat" aka "input$file1"
                               
                               fileInput('file1', 
                                         tags$h4('Select a genepop file (.gen or .txt)'),
                                         accept = c(
                                           'text/csv',
                                           'text/comma-separated-values',
                                           'text/tab-separated-values',
                                           'text/plain',
                                           '.gen',
                                           '.csv',
                                           '.tsv')
                                         ),
                               
                               ####Ability to download test data
                               
                               downloadButton(outputId = "tstdat_dl", label = "Download example"),
                               
                               ####Break lines for aestetics
                               tags$br(),
                               tags$br(),
                               
                               ####Add restrictive access to panel groups
                               
                               tags$h4("Display data"),
                               
                               ####Option to view complete dataset
                               
                               div(style="display:inline-block; float:left",actionButton(inputId = "viewdat", label = "View data")),
                               
                               ####Option to view Population names in dataset
                               
                               div(style="display:inline-block; float:left",actionButton(inputId = "viewPops", label = "View Population groups"))
               ),
               
               ####Collapse panel which allows you to alter your population names
               
               bsCollapsePanel("2. Edit your population names",
                               
                               ####Text which describes how to alter the population group names
                               
                               textInput(inputId = "rnmPops", label = "Rename Population groups", placeholder = "Pop-1<comma><space>Pop-2<comma><space>Pop-3 (e.g African, European, Asian Indian) *Note - No output unless pop size equals label size*",width = "100%"),
                               
                               ####Action button to apply the names
                               
                               actionButton("subButton1", label = "1. Apply names"),
                               
                               ####Action button (upButton1) to submit the given population names from step 1 above
                               
                               actionButton("upButton1", label = "2. Update names")
                               
               ),
               #### Collapse panel which allows you to compute all metrics
               
               bsCollapsePanel("3. Perform computations", 
                               
                               ####Press button (calcButton) to compute all metrics
                               
                               disabled(actionButton("calcButton", label = "Compute")),
                               disabled(actionButton("unlockBTN", label = "Output"))
                               
                               
                               
               )
              
    )
  )
}