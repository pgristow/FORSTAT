##Haploid and dicrimination capacity
##calculations are different for diploid and haploid DC

require("shinydashboard")
require("shinyjs")
header <-dashboardHeader(
  title = "FORSTAT"
)

sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(id="poopie",
    menuItem("Home", tabName = "tab0", icon = icon("home")),
    menuItem("Load and view data", tabName = "tab1", icon = icon("arrow-circle-down"))
  ),
  hidden(sidebarMenu(id="balls",
    menuItem("Allelic Frequencies", tabName = "tab2", icon = icon("bar-chart")),
    menuItem("Allele specific stats", tabName = "tab3", icon = icon("signal")),
    menuItem("Forensic stats", tabName = "tab4", icon = icon("pie-chart")),
    menuItem("Paternity stats", tabName = "tab5", icon = icon("venus-mars")),
    menuItem("Graphing options and downloads", tabName = "tab6", icon = icon("area-chart")))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tab0",
            uiOutput("tab0_loadpage")
            ),
    
    tabItem(tabName = "tab1", 
            
            uiOutput("tab1_page"), 
            tags$hr(),
            verbatimTextOutput('Content2'),
            tags$hr(),
            dataTableOutput('Content')
            ),
    tabItem(tabName = "tab2",
            uiOutput("tab2_page"),
            tags$hr(),
            verbatimTextOutput(outputId = 'Content4'),
            tags$hr(),
            plotOutput('Content3', height = "1000px", width = "100%")
            ),
    
    tabItem(tabName = "tab3",
            uiOutput("tab3_page"),
            dataTableOutput(outputId = 'Content5')
            ),
    
    tabItem(tabName = "tab4",
            uiOutput("tab4_page"),
            plotOutput(outputId = 'Content6'),
            dataTableOutput(outputId = 'Content8')
            ),
    tabItem(tabName = "tab5",
            uiOutput("tab5_page"),
            plotOutput(outputId = 'Content7'),
            dataTableOutput(outputId = 'Content9')
            ),
    
    tabItem(tabName = "tab6",
            uiOutput("tab6_page")
    )
  )
)


ui <-  shinyUI(dashboardPage(header, sidebar, body))