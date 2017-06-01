#Load the required packages
require("shiny")
require("shinydashboard")
require("shinyBS")
require("reshape2")
require("ggplot2")
require("plyr")
require("dplyr")
require("gstudio")
require("shinyjs")
require("xlsx")
source("tabItems/tab0_loadpage.R")
source("tabItems/tab1_page.R")
source("tabItems/tab2_page.R")
source("tabItems/tab3_page.R")
source("tabItems/tab4_page.R")
source("tabItems/tab5_page.R")
source("tabItems/tab6_page.R")


server <- shinyServer(function(input, output, session) {
  
  #Render the HTML pages
  output$tab0_loadpage <- renderUI(create_tab0_loadpage(session = session))
  output$tab1_page <- renderUI(create_tab1_page(session = session))
  output$tab2_page <- renderUI(create_tab2_page(session = session))
  output$tab3_page <- renderUI(create_tab3_page(session = session))
  output$tab4_page <- renderUI(create_tab4_page(session = session))
  output$tab5_page <- renderUI(create_tab5_page(session = session))
  output$tab6_page <- renderUI(create_tab6_page(session = session))
  
  
  
  #Force datasets to be empty
  #dat_new contains the dataset with altered names
  dat_new <- NULL
  #Original dataset with imported names
  dat  <- NULL
  
  #Max file upload size
  options(shiny.maxRequestSize = 9*1024^2)
  
  
  
  #Process the data file
  dat <- reactive({
    
    startTime <- Sys.time()
    
      inFile <- input$file1
    
      if (is.null(inFile))
      return(NULL)
    
      return(read_population(path=inFile$datapath, type = "genepop"))
    
    endTime <- Sys.time()
    cat(endTime-startTime)
    
  })
  
  
  v <- reactiveValues(dat=dat)
  
  #set initial population group names
  
  observeEvent(input$file1 ,{
    startTime <- Sys.time()
    
    if(is.null(dat()))
      return(NULL)
    
    df = dat()
    v$pop <- names(df$Population)
    
    endTime <- Sys.time()
    cat(endTime-startTime)
    
  })
  
  #Set population group names based on whether the 
  observeEvent(input$viewPops,{
    if(input$rnmPops == ""){
      return(v$pop)
    }else{
      df = dat_new()
      v$pop <- levels(factor(df$Population))}
  })
  
  
  
  dat_new <- eventReactive(input$subButton1,{
    
    j <- strsplit(paste(input$rnmPops),split=", ")
    data2 <- dat()
    d <- unique(data2$Population)
    
    if(length(d) != length(j[[1]]))
      return(NULL)
    
    for (i in 1:length(d))
    {
      x <- j
      data2$Population <- gsub(paste("Pop-", i, sep=""), x[[1]][i], data2$Population)
    }
    return(data2)
  })
  
  
  
  
  
  
  
  
  output$input_text <- renderUI({
    #####rnmPops lets us know if new population names were given, if not we continue with these group names
    if(input$rnmPops == ""){
      df <- dat()
    }
    ####Else if the user has renamed the groups we use the new names
    else if(input$rnmPops != ""){
      df <- dat_new()
    }
    #an if statement to check whether there is any data in the data.frame
    #taglist lets us add several input selectors
    if(is.null(df)){
      tagList(
        
        ####The heading of the page
        tags$h4("Plot allele frequencies"),
        
        ####When there is no data we do not want to allow the user to process anything
        ####Div makes a block division for all components to be grouped together
        div(style="display:inline-block; float:left",
            
            ####Add an empty droplist to the page for all individuals
            selectInput('input_text2', 'Select Locus', choices="<Select a locus>", width = "400px")),
        
        ####Div makes a block division for all components to be grouped together
        div(style="display:inline-block; float:left",
            ####Add an empty droplist to the page for populations
            selectInput('input_text3', 'Select Locus', choices="<Select a locus>", width = "400px")),
        
        br(),
        
        ####disable the download freq plots button
        ####downloadButton('freqDL', 'Download frequency plots'),
        tags$hr(),
        tags$hr(),
        tags$hr(),
        tags$hr(),
        
        ####Add subheading for plotting area
        tags$h4("Plot heatmaps"),
        
        #####Add buttons to view the heatmaps
        actionButton(inputId = "heatmap_allele_all", 
                     label = "Heatmap of all individuals", width = "400px",style="margin-right:0px"),
        actionButton(inputId = "heatmap_allele_pop", 
                     label = "Heatmap per population group", width = "400px",style="margin-left:-3px")
        
        ####Disable the download buttons
        #br(),
        #downloadButton('heatmapDL', 'Download heatmaps')
        
      )
      #if there is data we output these
    }else{
      ####Taglist is a container for tags
      tagList(
        
        ####Heading for section
        div(style="display:block",
            tags$b(tags$h4("Plot heatmaps")),
        
        ####Div section for the input droplists
        
            
            ####Offer the user a choice of loci to plot for all individuals
            selectInput('input_text2', 'Select Locus', 
                        choices=c("<Select a locus>", colnames(df)[3:length(df)]), width = "400px"),
        
        
            
            ####Offer the user a choice of loci to plot for all individuals
            selectInput('input_text3', 'Select Locus', 
                        choices=c("<Select a locus>", colnames(df)[3:length(df)]), width = "400px")
            ),
        
        
        ####Download button to download frequency plots
        #downloadButton('freqDL', 'Download frequency plots'),
        # tags$hr(),
        # tags$hr(),
        # tags$hr(),
        # tags$hr(),
        
        
        ####Heading of heatmap section
        div(style="display:block",
            tags$b(tags$h4("Plot heatmaps")),
        
        ####Add actionbuttons to plot heatmaps
        tags$b(tags$h5("View heatmap overall")),
        actionButton(inputId = "heatmap_allele_all", 
                     label = "Heatmap of all individuals", width = "400px"),
        
        tags$b(tags$h5("View heatmap per population")),
        actionButton(inputId = "heatmap_allele_pop", 
                     label = "Heatmap per population group", width = "400px")
        #br(),
        
        ####Download button to download heatmaps
        #downloadButton('heatmapDL', 'Download heatmaps')
      ))
    }
  }
  )
  

  ####Depending on the selection of the dropdown list for frequency plots a plot is created  
  p <- eventReactive(input$input_text2,{
    
    ####save the data to a file
    df <- dat()
    
    ####Check if the 
    if(input$input_text2 == "<Select a locus>"){
      return(NULL)
    
    }else{
    
    ####search df for the user selection and save column to variable x
    x <- df[,which(colnames(df) == input$input_text2)]
    
    ####Return the plot
    return(plot(x))
    }
  }
  )
  
  
  ####When you press calcButton it calculates all the information
  
  
  
  observeEvent(input$calcButton,{
    
    
    startTime <- Sys.time()
    ####Incorporate a progress bar to say the program is working
    
    withProgress(message = "Calculations in progress...",{
      
      
      
      ####Check if the data is renamed or not, the first line checks if the user input is empty
      
      if(input$rnmPops == ""){
        
        ####Data saved to dat2
        dat2 <- dat()
        
        ####If the user input is not empty we use the renamed dataset
        
      }else{
        
        ####Data saved to dat2
        dat2 <- dat_new()
      }
      
      ####The data is partitioned based on the names of population and saved it to v$pops
      v$pops <- partition(dat2, stratum = "Population")
      
      ####Determine the frequency of each allele per population and save it to v$FLp (Frequency Loci population)
      v$FLp <- frequencies(dat2, stratum="Population")
      
      ####Use lappy to tabulate the data with xtabs per population
      ####Each value is tabulate against its respective locus and allele using the data from v$FLp
      v$freqpop.table2 <- lapply(v$pops, function(x){(xtabs(Frequency ~ Allele + Locus, data = v$FLp))})
      
      ####The data is melted for creation of a heatmap in ggplot
      v$FLp.melt <- melt(v$FLp)
      
      ####Determine the dimensions of the dat2 data for amount of loci
      v$nc <- length(names(dat2))
      
      ####Determine the dimensions of the v$pops data for amount of populations
      v$nb <- length(names(v$pops))
      
      ####Provides a empty data.frame for storage
      v$freqpop <- data.frame()
      
      ####Provides a list to store data per population
      v$freqpop.table <- list()
      
      ####Calculates frequencies by looping through datasets
      for(i in 1:v$nb)
      {
        v$freqpop <- v$FLp[which(v$FLp$Stratum == (unique(v$FLp$Stratum))[[i]]),]
        v$freqpop.table[[i]] <- xtabs(Frequency ~ Allele + Locus, data = v$freqpop)
      }
      
      v$fpt <- matrix(v$freqpop.table)
      
      
      ####Find duplicated genotypes
      v$Dupli_geno <- as.data.frame(dat2[duplicated(dat2[3:v$nc]) | duplicated(dat2[,3:v$nc], fromLast=TRUE),1:v$nc])
      
      
      
      
      
      
      
      
      
      ####Polymorphic Information Content
      v$PIC <- v$fpt
      v$PICa <- data.frame()
      v$PICb <- data.frame()
      v$PIC2 <- list()
      v$PIC4 <- list()
      v$PIC_tot_temp <- data.frame()
      v$PIC_tot <- list()
      v$PIC_tot2 <- list()
      
      for (i in 1:nrow(v$fpt))
      {
        for(j in 1:length(v$fpt[[1,1]][1,]))
        {
          ####Extract the frequency of each allele and square the value
          v$PICa <- apply(v$PIC[[i,1]], 2, function(x) sum(x*x))
          
          ####Extract the frequency of each allele and raise to the power 4
          v$PICb <- apply(v$PIC[[i,1]], 2, function(x) sum(x*x*x*x))
          
          ####Store these values per populations
          v$PIC2[[i]] <- v$PICa
          v$PIC4[[i]] <- v$PICb
          
          ####Calculate PIC using the calculated values and store in a temp file
          v$PIC_tot_temp <- 1 - v$PIC2[[i]][j] - (v$PIC2[[i]][j]*v$PIC2[[i]][j]) + v$PIC4[[i]][j]
          
          ####Store the PIC values per locus
          v$PIC_tot[j] <- v$PIC_tot_temp
        }
        
        ####Store the PIC values per population group again
        v$PIC_tot2[[i]] <-v$PIC_tot
      }
      
      ####Turn the data into a matrix
      v$PIC_mat <- matrix(v$PIC_tot2)
      
      ####bind the matrix by row
      v$PICDF <- do.call(rbind,v$PIC_tot2)
      
      ####Change the row names to the respective population
      rownames(v$PICDF) <- names(v$pops)
      
      ####Change the row names to the respective locus
      colnames(v$PICDF) <- names(v$fpt[[1,1]][1,])
      
      ####Transpose the matrix
      v$PICDFt <- t(v$PICDF)
      
      ####Bind the rownames as a new coloumn
      v$PICDFt_out <- cbind(rownames(v$PICDFt),v$PICDFt)
      
      ####Rename the first column name to define it as loci
      colnames(v$PICDFt_out)[1] <- "Loci"
      
      ####Add a row of colnames
      v$PICDFt_out <- rbind(colnames(v$PICDFt_out),v$PICDFt_out)
      
      
      
      
      
      incProgress(1/4)
      ####Calculate the Expected heterozygosity
      v$hexp <- lapply(v$pops, function(x) return(He(x)))
      
      ####Identify the unique population names
      v$d <- unique(names(v$pops))
      
      ####Storage for the He
      v$hexp_data <- data.frame()
      
      ####Make a column called Loci and insert loci names
      v$hexp_data <- data.frame(Loci = v$hexp[[1]]$Locus)
      
      ####Calculate the He
      v$p <- data.frame()
      for(i in 2:(length(v$d)+1))
      {
        v$p <- v$d[i-1]
        v$hexp_data[,i] <- data.frame(p = v$hexp[[i-1]]$He)
        names(v$hexp_data)[i] <- v$p[]
      }
      
      v$shexp_data <- t(v$hexp_data[order(levels(v$hexp_data[,1])),])
      v$shexp_data_out <- t(cbind(rownames(v$shexp_data), v$shexp_data))
      
      
      
      
      
      
      ####Observed heterozygosity
      v$hobs <- lapply(v$pops, function(x) return(Ho(x)))
      
      v$d <- unique(names(v$pops))
      v$hobs_data <- data.frame()
      v$hobs_data <- data.frame(Loci = v$hobs[[1]]$Locus)
      v$p <- data.frame()
      for(i in 2:(length(v$d)+1))
      {
        v$p <- v$d[i-1]
        v$hobs_data[,i] <- data.frame(p = v$hobs[[i-1]]$Ho)
        names(v$hobs_data)[i] <- v$p[]
      }
      
      v$shobs_data <- t(v$hobs_data[order(levels(v$hobs_data[,1])),])
      v$shobs_data_out <- t(cbind(rownames(v$shobs_data), v$shobs_data))
      
      
      
      
      incProgress(1/4)
      ####Heatmap data of observed and expected heterozygotes
      v$Hobsmelt <- melt(v$hobs_data)
      v$Hexpmelt <- melt(v$hexp_data)
      
      
      
      
      
      ####Frequency of homozygotes and Paternity Index of each loci
      v$Homomelt = v$Hobsmelt
      v$Homomelt$value <- (1-v$Hobsmelt$value)
      
      v$homozy <- data.frame(v$hobs_data)
      v$PI <- data.frame(v$hobs_data)
      rownames(v$homozy) <- v$hobs_data[,1]
      for (i in 1:nrow(v$homozy))
      {
        for (j in 2:length(v$homozy))
        {
          v$homozy[[i,j]] <- (1 - (v$homozy[[i,j]]))
          v$PI[[i,j]] <- (1/(2*(v$homozy[i,j])))
        }
      }
      v$homozy <- t(v$homozy)
      v$homozy2 <- v$homozy[2:nrow(v$homozy),]
      v$homozy2_out <- t(cbind(v$d,v$homozy2))
      rownames(v$homozy2_out)[1] <- "Loci"
      v$homozy2_out <- cbind(rownames(v$homozy2_out),v$homozy2_out)
      colnames(v$homozy2_out)[1] <- "Loci"
      v$PI <- v$PI[!v$PI[,1]=="Multilocus",]
      
      
      
      
      
      
      ####Combined Paternity index
      v$CPI <- data.frame(colnames=names(v$pops), CPI=1)
      
      for(i in 2:length(v$PI))
      {
        v$CPI[i-1,2] <- (prod(v$PI[i]))
      }
      
      
      ####Power of Exclusion
      v$pexcl_pops <- v$homozy2
      
      v$pexcl_pops <- v$pexcl_pops[,!colnames(v$pexcl_pops)=="Multilocus"]
      v$nc1 <- length(v$pexcl_pops[1,])
      v$nb1 <- nrow(v$pexcl_pops)
      
      sapply(1:v$nb1, function(i) sapply(1:v$nc1, function(j) {
        v$hom <- as.numeric(v$pexcl_pops[i,j])
        v$he <- 1 - as.numeric(v$pexcl_pops[i,j])
        v$pexcl_pops[[i,j]] <- (v$he*v$he)*(1-(2*(v$he*(v$hom*v$hom))))
      })
      )
      
      
      v$pexcl_pops.melt <- melt(as.matrix(v$pexcl_pops))
      v$pexcl_pops.melt$Y1 <- cut(as.numeric(as.character(v$pexcl_pops.melt$value)),breaks = c(-Inf,0:0.05,0.05:0.10,0.10:0.15,0.15:0.20,0.20:0.25,0.25:0.30,0.30:0.35,0.35:0.40,0.40:0.45,0.45:0.50,0.50:0.55,0.55:0.60,0.60:0.65,0.65:0.70,0.70:0.75,0.75:0.80,0.80:0.85,0.85:0.90,0.90:0.95,Inf),right = FALSE)
      
      #levels(v$pexcl_pops.melt$Var2) <- order(levels(v$pexcl_pops.melt$Var2))
      
      v$pexcl_tot_pop <- sprintf("%.12f", apply(v$pexcl_pops, 1, function(x) (1-prod(1-as.numeric(x)))))
      
      
      
      
      
      
      incProgress(1/4)
      ####Genotype Frequencies
      v$temp <- list()
      v$all <- list()
      
      sapply(1:length(names(v$pops)), function(i){ 
        sapply(3:length(names(dat2)), function(j) {
          v$nr <- nrow(genotype_frequencies(v$pops[[i]][,j], supress_warnings = TRUE ))
          v$vec <- data.frame(x=numeric(v$nr), y=numeric(v$nr), z=numeric(v$nr))
          v$z <- (v$pops[[i]][,j])
          v$vec <- genotype_frequencies(v$z, supress_warnings = TRUE)
          v$temp[[j]] <- v$vec
        })
        v$all[[i]] <- v$temp
      })
      
      v$DF <- do.call(cbind,v$all)
      
      
      
      
      
      ####Random Match Probability
      
      ####Determine length of population names
      v$nc <- length(names(v$pops))
      
      ####Determine length of dataframe which is equal to amount of loci
      v$nr <- length(names(dat2))
      
      ####make a matrix with dimensions above
      v$RMP <- matrix(ncol=v$nc, nrow=v$nr)
      
      ####Rownames are loci
      rownames(v$RMP) <- names(dat2)
      
      ####column names are populations
      colnames(v$RMP) <- names(v$pops)
      
      ####Loop through the matrix
      sapply(1:v$nr, function(x) sapply(1:v$nc, function(z) {
        
        ####Sum all frequencies squares is the RMP
        v$RMP[x,z] <- (sum((v$DF[[x,z]][,2]/nrow(v$pops[[z]]))^2))
      }))
      
      ####coerce to dataframe
      v$RMP.df <- as.data.frame(v$RMP)
      
      ####remove columns 1 and 2
      v$RMP.df1 <- v$RMP.df[-c(1,2),]
      
      ####transpose and order dataframe
      v$RMP.df2 <- t(v$RMP.df1[order(rownames(v$RMP.df1), decreasing = F),order(colnames(v$RMP.df1),decreasing = T)])
      
      
      
      
      incProgress(1/4)
      ####Prep data for graphing Match Probability per loci
      ####prep storage area
      v$GF.rmp.melt <- data.frame()
      
      ####Populate dataframe with RMP values
      v$GF.rmp.melt <- melt(v$RMP.df2)
      
      ####Set bins for the heatmap
      v$GF.rmp.melt$Y1 <- cut(v$GF.rmp.melt$value,breaks = c(-Inf,0:0.05,0.05:0.10,0.10:0.15,0.15:0.20,0.20:0.25,0.25:0.30,0.30:0.35,0.35:0.40,0.40:0.45,0.45:0.50,0.50:0.55,0.55:0.60,0.60:0.65,0.65:0.70,0.70:0.75,0.75:0.80,0.80:0.85,0.85:0.90,0.90:0.95,Inf),right = FALSE)
      
      
      
      
      
      ####Combined Match Probability (CMP)
      ####CMP is product of RMP for each locus
      v$CMP <- data.frame(apply(v$RMP.df2, 1, function(x) prod(x)))
      
      ####Set column name to Combined
      colnames(v$CMP) <- "Combined"
      
      ####Bind the column the RMP dataframe
      v$CMP_out <- cbind(v$RMP.df2, v$CMP)
      
      
      
      
      
      ####Discrimination Capacity (DC)
      ####DC is 1 - RMP so populate a dataframe with RMP values
      v$DCpl <- v$RMP.df2
      
      ####Loop through dataframe and calculate DC per locus
      sapply(1:length(v$RMP.df2[2,]), function(x) {
        sapply(1:length(v$RMP.df2[,1]), function(z) {
          v$DCpl[[z,x]] <- (1 - v$RMP.df2[z,x])
        })})
      
      ####transpose dataframe for plotting
      v$DCpl <- t(v$DCpl)
      
      ####Prep data toplot DC per loci
      ####melt the data
      v$DCpl.melt <- melt(as.matrix(t(v$DCpl)))
      
      ####Prep bins for heatmap
      v$DCpl.melt$Y1 <- cut(v$DCpl.melt$value,breaks = c(-Inf,0:0.05,0.05:0.10,0.10:0.15,0.15:0.20,0.20:0.25,0.25:0.30,0.30:0.35,0.35:0.40,0.40:0.45,0.45:0.50,0.50:0.55,0.55:0.60,0.60:0.65,0.65:0.70,0.70:0.75,0.75:0.80,0.80:0.85,0.85:0.90,0.90:0.95,Inf),right = FALSE)
      
      
      
      
      
      ####Combined DC (CDC)
      ####CDC equals 1 - product(DC of loci)
      v$CDC_out <- v$CMP
      
      ####sapply to calculate CMP, sprintf to round up
      sapply(1:nrow(v$CMP), function(i){
        v$CDC_out[i,1] <- sprintf("%.20f", 1 - v$CMP[i,1])
      }
      )
      
      
      
      ####Plot of allele frequency of all population groups
      v$plot$freqallheat <- print(ggplot(v$FLp.melt, aes(x = Locus, y = Allele)) +
                                    ggtitle("Allele frequencies of all loci of all individuals") +
                                    geom_tile(aes(fill = value), colour = "black") +
                                    scale_fill_gradient(low = "yellow", high = "red") +
                                    theme(plot.title = element_text(lineheight=.8, face="bold"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), axis.text.x=element_text(angle=90, hjust = 1, size = 5), axis.text.y=element_text(size = 4, hjust = 1)))
      
      ####Plot of allele frequency per population group
      v$plot$freqpopheat <- print(ggplot(v$FLp.melt, aes(x = Locus, y = Allele)) +
                                    facet_wrap(~Stratum, ncol=v$nb) + ggtitle("Allele frequencies of all loci within populations") +
                                    geom_tile(aes(fill = value), colour = "black") +
                                    scale_fill_gradient(low = "yellow", high = "red") +
                                    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), axis.text.x=element_text(angle=90, hjust = 1, size = 6), axis.text.y=element_text(hjust = 1, size = 6)))
      
      
    })
    observe(exists('v$pops'),{enable("unlockBTN")})
    
    endTime <- Sys.time()
    
    cat(endTime-startTime)
    
  })
  
  observeEvent(input$unlockBTN ,{
    showElement("balls")
    
  })
  
  
  
  ####To unlock the compute button we need the data to be loaded
  observeEvent(input$file1,{
    
    enable("calcButton")
  })
  
  
  
  
  ####When the upButton1 is pressed the population names will be submitted
  observeEvent(input$upButton1,{
    if(is.null(dat_new)){
      v$dat <-strsplit(paste(input$rnmPops),split=", ")
    }else{v$dat <- dat_new()}
  })
  
  
  
  ####Pressing on viewdat will open the dat datafile
  observeEvent(input$viewdat,{
    if(!is.null(dat_new))
      v$dat <- dat()
    
    v$dat <- dat_new()
  })
  
  
  
  
  ####Initial text to display on tab1
  output$Content2 <- renderText({
    if(is.null(v$pop)){
      print("You have no data to show. Please upload a data file.")
    }else{
      
      print(c("Your population names are: ", v$pop, "\n\n\n*Update list by clicking 'view population groups' above*"))}
  })
  
  
  
  ####Insert an option to download some test data
  output$tstdat_dl <- downloadHandler(
    
    filename = function() {
      paste('GOATexample.gen', sep='')
    },
    content = function(file) {
      file.copy("data/innoRSA.gen", file)
    }
  )
  
  
  
  
  ####Display the amount of alleles
  All_out <- eventReactive(input$al_disp, {
    # enable('al_dl')
    dat1 <- A(dat())
    colnames(dat1)[2] <- "Alleles"
    return(dat1)
  })
  
  
  
  
  ####This renders the datatable and displays the data
  output$Content <- renderDataTable({
    if(is.null(v$dat))
      return()
    v$dat
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ####TAB2
  freqTab <- eventReactive(input$freq_disp, {
    x <- frequencies(dat())
    return((xtabs(Frequency ~ Allele + Locus, dat = x)))
  })
  
  observeEvent(input$freq_disp,{
    v$freq_plot <- freqTab()
  })
  
  
  
  # output$al_dl <- downloadHandler(
  #   
  #   filename = function() {
  #     paste('locus_Allele_total ', Sys.Date(), '.xlsx', sep='')
  #   },
  #   content = function(file) {
  #     write.xlsx(All_out(), file,sheetName = "Freqs", row.names = FALSE)
  #   }
  # )
  
  
  
  observeEvent(input$input_text3,{
    if(input$input_text3=="<Select a locus>")
      return(NULL)
    
    
    v$flname <- paste("Allele frequency of", input$input_text3, "between populations", sep = " ")
    v$f <- v$FLp[v$FLp$Locus %in% input$input_text3,]
    v$plot$freqpop <- print(ggplot(v$f) + 
                              ggtitle(v$flname) + 
                              geom_frequencies(v$f) + 
                              facet_grid(Stratum~.) +
                              theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)))
    
    output$Content3 <- renderPlot({
      if(!is.null(v$plot$freqpop))
        print(v$plot$freqpop)
    }, height = v$nb*200)
  })
  
  observeEvent(input$heatmap_allele_all,{
    output$Content3 <- renderPlot({
      if(!is.null(v$plot$freqallheat))
        print(v$plot$freqall)
    }, height = 1000)})
  
  observeEvent(input$heatmap_allele_pop,{
    output$Content3 <- renderPlot({
      if(!is.null(v$plot$freqpopheat))
        print(v$plot$freqpopheat)
    }, height = 1000)})
  
  observeEvent(input$al_disp,{
    v$dat2 <- All_out()
    
    output$Content4 <- renderPrint({
      v$dat2
    })
    output$Content3 <- renderPlot({
      print(NULL)
    }, height = 500)
  })
  
  observeEvent(input$input_text2,{
    #v$dat2 <- All_out()
    
    output$Content4 <- renderPrint({
      NULL
    })
    
    output$Content3 <- renderPlot({
      if(!is.null(p))
        print(p())
    }, height = 500)
  })
  
  
  
  
  
  ####TAB3
  observeEvent(input$Dupli_geno,{
    output$Content5 <- renderDataTable({
      if(!is.null(v$Dupli_geno))
        v$Dupli_geno
    },options = list(scrollX = TRUE))
  })
  
  observeEvent(input$PIC_disp,{
    output$Content5 <- renderDataTable({
      if(!is.null(v$PICDFt_out))
        
        v$PICDFt_out[-1,]
      
    },options = list(scrollX = TRUE))
  })
  
  observeEvent(input$He_disp,{
    v$shexp_disp <- data.frame(arrange(as.data.frame(v$shexp_data_out), Loci))
    v$shexp_disp <- v$shexp_disp[v$shexp_disp[,1]!="Loci",]
    output$Content5 <- renderDataTable({
      v$shexp_disp
        
    },options = list(scrollX = TRUE))
  })
  
  
  observeEvent(input$H_obs_disp,{
    output$Content5 <- renderDataTable({
      if(!is.null(v$shobs_data_out))
        v$shobs_data_out[-1,]
    },options = list(scrollX = TRUE))
  })
  
  observeEvent(input$Homozy_disp,{
    output$Content5 <- renderDataTable({
      if(!is.null(v$homozy2_out))
        v$homozy2_out[-1,]
    },options = list(scrollX = TRUE))
  })
  
  
  
  
  
  
  
  
  
  ####TAB4
  observeEvent(input$PE_disp,{
    
    v$plot$PEheat <- print(ggplot(v$pexcl_pops.melt, aes(x = Var2, y = Var1)) +
                             ggtitle("Power of exclusion of loci within populations") +
                             geom_tile(aes(fill = Y1), colour = "black", height = 1) +
                             scale_fill_brewer("PE range",palette = 1)+
                             theme(axis.text.x=element_text(angle=90, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), plot.title = element_text(face="bold", size = 24, hjust = 0.5)) +
                             ylim(rev(levels(v$pexcl_pops.melt$Var1))) +
                             xlim(sort(levels(v$pexcl_pops.melt$Var2))) +
                             ylab("Population groups") +
                             xlab("Locus"))
    
    v$pexcl_pops_out <- v$pexcl_pops[,order(colnames(v$pexcl_pops))]
    
    v$pexcl_pops_out <- cbind("Pop Name" = rownames(v$pexcl_pops_out),v$pexcl_pops_out)
    
    output$Content8 <- renderDataTable({
      if(!is.null(v$pexcl_pops_out))
        v$pexcl_pops_out
    },options = list(scrollX = TRUE))
    
    output$Content6 <- renderPlot({
      if(!is.null(v$plot$PEheat))
        print(v$plot$PEheat)
    })
  })
  
  
  
  
  
  observeEvent(input$RMP_disp,{
    
    v$plot$rmpheat <- print(ggplot(v$GF.rmp.melt, aes(x = Var2, y = Var1)) + 
                              ggtitle("Random match probability of loci between populations") +
                              geom_tile(aes(fill = Y1), colour = "black") +
                              scale_fill_brewer("RMP range",palette = 1)+
                              theme(axis.text.x=element_text(angle=90, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), plot.title = element_text(face="bold", size = 24, hjust = 0.5)) +
                              ylab("Population groups") + xlab("Locus"))
    
    
    v$CMP_out2 <- v$CMP_out[order(rownames(v$CMP_out),decreasing = F),]
    v$CMP_out2 <- cbind("Pop Name" = rownames(v$CMP_out2),v$CMP_out2)
    
    output$Content8 <- renderDataTable({
      if(!is.null(v$CMP_out))
        v$CMP_out2
    },options = list(scrollX = TRUE))
    
    output$Content6 <- renderPlot({
      if(!is.null(v$plot$rmpheat))
        print(v$plot$rmpheat)
    })
  })
  
  
  
  
  
  
  
  observeEvent(input$DC_disp,{
    if(!is.null(v$CDC_out))
      v$CDC_out2 <- cbind("Pop name" = rownames(v$CDC_out),t(v$DCpl),v$CDC_out)
    v$CDC_out2 <- v$CDC_out2[order(rownames(v$CDC_out2)),]
    v$plot$DCheat <- ggplot(v$DCpl.melt, aes(x = Var2, y = Var1)) + 
      ggtitle("Discrimination capacity of each loci within populations")+ 
      geom_tile(aes(fill = Y1), colour = "black", height = 1) +
      scale_fill_brewer("DC range",palette = 1)+
      theme(axis.text.x=element_text(angle=90, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), plot.title = element_text(face="bold", size = 24, hjust = 0.5)) +
      ylab("Population groups") + 
      xlab("Locus")
    
    output$Content8 <- renderDataTable({
      v$CDC_out2
    },options = list(scrollX = TRUE))
    
    output$Content6 <- renderPlot({
      if(!is.null(v$plot$DCheat))
        print(v$plot$DCheat)
    })
    
  })
  
  ####TAB5
  observeEvent(input$PI_disp,{
    
    v$PI_out <- t(v$PI)
    v$PI_out <- v$PI_out[,order(as.character(v$PI_out[1,]))]
    colnames(v$PI_out) <- as.character(v$PI_out[1,])
    v$PI_out <- cbind("Pop Name" = rownames(v$PI_out),v$PI_out, "Combined" = v$CPI[,2])
    v$PI_out <- v$PI_out[-1,]
    v$PI.melt <- melt(as.matrix(v$PI_out[,2:(ncol(v$PI_out)-1)]))
    
    v$PI.melt$Y1 <- cut(as.numeric(as.character(v$PI.melt$value)),breaks = seq(0,100,0.2),right = FALSE)
    v$plot$PIheat <- print(ggplot(v$PI.melt, aes(x = Var2, y = Var1)) + 
                             ggtitle("The paternity index of all loci in each population") + 
                             geom_tile(aes(fill = Y1), colour = "black", height = 1) + 
                             scale_fill_brewer("PI range", palette = 1) + 
                             theme(axis.text.x=element_text(angle=90, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) +
                             #ylim(rev(levels(v$PI.melt$variable)))+
                             ylab("Population groups")+
                             xlab("Locus")
                           )
    
    output$Content9 <- renderDataTable({
      if(!is.null(v$PI))
        v$PI_out
    },
    options = list(scrollX = TRUE, rownames=T)
    )
    
    
    output$Content7 <- renderPlot({
      if(!is.null(v$plot$PIheat))
        print(v$plot$PIheat)
    })
    
    ###Tab 6

    
    
    # v$pexcl_pops_out <- v$pexcl_pops[,order(colnames(v$pexcl_pops))]
    # 
    # v$pexcl_pops_out <- cbind("Pop Name" = rownames(v$pexcl_pops_out),v$pexcl_pops_out)
    # 
    # output$Content8 <- renderDataTable({
    #   if(!is.null(v$pexcl_pops_out))
    #     v$pexcl_pops_out
    # v$plot$rmpheat <- print(ggplot(v$GF.rmp.melt, aes(x = Var2, y = Var1)) + 
    #                           ggtitle("Random match probability of loci between populations") +
    #                           geom_tile(aes(fill = Y1), colour = "black") +
    #                           scale_fill_brewer("RMP range",palette = 1)+
    #                           theme(axis.text.x=element_text(angle=90, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), plot.title = element_text(face="bold", size = 24, hjust = 0.5)) +
    #                           ylab("Population groups") + xlab("Locus"))
    # 
    # v$CMP_out2 <- v$CMP_out[order(rownames(v$CMP_out),decreasing = F),]
    # v$CMP_out2 <- cbind("Pop Name" = rownames(v$CMP_out2),v$CMP_out2)
    #
    #   if(!is.null(v$CMP_out))
    #     v$CMP_out2
    
    # v$PI_out <- t(v$PI)
    # v$PI_out <- v$PI_out[,order(as.character(v$PI_out[1,]))]
    # colnames(v$PI_out) <- as.character(v$PI_out[1,])
    # v$PI_out <- cbind("Pop Name" = rownames(v$PI_out),v$PI_out, "Combined" = v$CPI[,2])
    # v$PI_out <- v$PI_out[-1,]
    # v$PI.melt <- melt(as.matrix(v$PI_out[,2:(ncol(v$PI_out)-1)]))
    # 
    # v$PI.melt$Y1 <- cut(as.numeric(as.character(v$PI.melt$value)),breaks = seq(0,100,0.2),right = FALSE)
    # v$plot$PIheat <- print(ggplot(v$PI.melt, aes(x = Var2, y = Var1)) + 
    #                          ggtitle("The paternity index of all loci in each population") + 
    #                          geom_tile(aes(fill = Y1), colour = "black", height = 1) + 
    #                          scale_fill_brewer("PI range", palette = 1) + 
    #                          theme(axis.text.x=element_text(angle=90, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) +
    #                          #ylim(rev(levels(v$PI.melt$variable)))+
    #                          ylab("Population groups")+
    #                          xlab("Locus")
    # )
  #   if(!is.null(v$PI))
  #     v$PI_out
    
    # if(!is.null(v$CDC_out))
    #   v$CDC_out2 <- cbind("Pop name" = rownames(v$CDC_out),t(v$DCpl),v$CDC_out)
    # v$CDC_out2 <- v$CDC_out2[order(rownames(v$CDC_out2)),]
    # v$plot$DCheat <- ggplot(v$DCpl.melt, aes(x = Var2, y = Var1)) + 
    #   ggtitle("Discrimination capacity of each loci within populations")+ 
    #   geom_tile(aes(fill = Y1), colour = "black", height = 1) +
    #   scale_fill_brewer("DC range",palette = 1)+
    #   theme(axis.text.x=element_text(angle=90, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), plot.title = element_text(face="bold", size = 24, hjust = 0.5)) +
    #   ylab("Population groups") + 
    #   xlab("Locus")
    # v$CDC_out2
    
  })
  
  output$dlfreqplot<- downloadHandler(
    
    filename = function() {
      paste("FORSTAT allele freqs",Sys.time(),"zip", sep=".")
    },
    
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      

      fs <- "al_freq.tiff"
      dat3 <- v$dat()
      
      for (i in 3:length(dat3))
      {
        
        fs <- c(fs,paste0("plot_heat_", names(dat3[i]), ".tiff", sep=""),paste0("plot_", names(dat3[i]), ".tiff", sep=""))
        
        tiff(paste0("plot_heat_", names(dat3[i]), ".tiff", sep=""), height = 6, width = 6, units = 'in', res=300, compression = "lzw")
        v$flname <- paste("Allele frequency of", names(dat3[i]), "within all individuals", sep = " ")
        print(ggplot(v$FLp.melt, aes(x = Locus, y = Allele)) +
                ggtitle("Allele frequencies of all loci of all individuals") +
                geom_tile(aes(fill = value), colour = "black") +
                scale_fill_gradient(low = "yellow", high = "red") +
                theme(plot.title = element_text(lineheight=.8, face="bold"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      axis.text.x=element_text(angle=90, hjust = 1, size = 5),
                      axis.text.y=element_text(size = 4, hjust = 1)
                      )
              )
        dev.off()
        
        tiff(paste0("plot_", names(dat3[i]), ".tiff", sep=""), height = 6, width = 6, units = 'in', res=300, compression = "lzw")
        v$flname <- paste("Allele frequency of", names(dat3[i]), "within all individuals", sep = " ")
        print(plot(dat3[[i]]) + ggtitle(paste0(v$flname)) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10)))
        dev.off()
        
        }
      

        fs <- c(fs,"FreqLoci_pop.tiff")
        
        tiff("FreqLoci_pop.tiff", height = 10, width = 12, units = 'in', res=300)
        print(ggplot(v$FLp.melt, aes(x = Locus, y = Allele)) +
                facet_wrap(~Stratum, ncol=v$nb) + ggtitle("Allele frequencies of all loci within populations") +
                geom_tile(aes(fill = value), colour = "black") +
                scale_fill_gradient(low = "yellow", high = "red") +
                theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), axis.text.x=element_text(angle=90, hjust = 1, size = 6), axis.text.y=element_text(hjust = 1, size = 6)))
        dev.off()
      
      
        
      v$d_plot <- unique(v$FLp[,2])
      for (i in 1:length(v$d_plot))
      {
        fs <- c(fs, paste0("freqsplot_", v$d_plot[[i]], ".tiff", sep=""))
        v$flname <- paste("Allele frequency of", v$d_plot[[i]], "between populations", sep = " ")
        v$f <- v$FLp[v$FLp$Locus %in% c(v$d_plot[i]),]
        tiff(paste0("freqsplot_", 
                    v$d_plot[[i]], 
                    ".tiff", sep=""),
             height = 6, 
             width = 6, units = 'in', 
             res=300, 
             compression = "lzw")
        
        
        print(ggplot(v$f) + 
                ggtitle(paste0(v$flname)) + 
                geom_frequencies(v$f) + 
                facet_grid(Stratum ~ .) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6))
              )
        
        dev.off()
      }
      
      print(fs)
      
      zip(zipfile=fname, files=fs)
    },
    
    contentType = "application/zip"
  )
  
  
  output$dlAll <- downloadHandler(
    
    filename = function() {
      paste("Outputs FORSTAT",Sys.time(),"zip", sep=".")
    },
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      
      fs <- c("PEheatplot.tiff","DCheatplot.tiff","PIheatplot.tiff","RMP.tiff")
      
      #Download PI heatmap
      
      tiff(filename = "PIheatplot.tiff", 
           width = input$plot_width, 
           height = input$plot_height, 
           units = input$plot_units,
           res = input$plot_res,
           compression = "lzw")
      
      v$PI_out <- t(v$PI)
      v$PI_out <- v$PI_out[,order(as.character(v$PI_out[1,]))]
      colnames(v$PI_out) <- as.character(v$PI_out[1,])
      v$PI_out <- cbind("Pop Name" = rownames(v$PI_out),v$PI_out, "Combined" = v$CPI[,2])
      v$PI_out <- v$PI_out[-1,]
      v$PI.melt <- melt(as.matrix(v$PI_out[,2:(ncol(v$PI_out)-1)]))

      v$PI.melt$Y1 <- cut(as.numeric(as.character(v$PI.melt$value)),breaks = seq(0,100,0.2),right = FALSE)
      print(ggplot(v$PI.melt, aes(x = Var2, y = Var1)) +
                               ggtitle("The paternity index of all loci in each population") +
                               geom_tile(aes(fill = Y1), colour = "black", height = 1) +
                               scale_fill_brewer("PI range", palette = 1) +
              theme(axis.text.x=element_text(angle=90, hjust = 1),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(), 
                    plot.title = element_text(face="bold", size = 24, hjust = 0.5)) +
                               #ylim(rev(levels(v$PI.melt$variable)))+
                               ylab("Population groups")+
                               xlab("Locus")
      )
              
      
      
      dev.off()
      
      tiff(filename = "DCheatplot.tiff", 
           width = input$plot_width, 
           height = input$plot_height, 
           units = input$plot_units,
           res = input$plot_res,
           compression = "lzw")
      
      print(ggplot(v$DCpl.melt, aes(x = Var2, y = Var1)) + 
        ggtitle("Discrimination capacity of each loci within populations")+ 
        geom_tile(aes(fill = Y1), colour = "black", height = 1) +
        scale_fill_brewer("DC range",palette = 1)+
        theme(axis.text.x=element_text(angle=90, hjust = 1),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(), 
              plot.title = element_text(face="bold", size = 24, hjust = 0.5)) +
        ylab("Population groups") + 
        xlab("Locus")
        )
      
      dev.off()
      
      #Download PE heatmap
      
      
      tiff(filename = "PEheatplot.tiff", 
           width = input$plot_width, 
           height = input$plot_height, 
           units = input$plot_units,
           res = input$plot_res,
           compression = "lzw")
      
      print(ggplot(v$pexcl_pops.melt, aes(x = Var2, y = Var1)) +
              ggtitle("Power of exclusion of loci within populations") +
              geom_tile(aes(fill = Y1), colour = "black", height = 1) +
              scale_fill_brewer("PE range",palette = 1)+
              theme(axis.text.x=element_text(angle=90, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(), plot.title = element_text(face="bold", size = 24, hjust = 0.5)) +
              ylim(rev(levels(v$pexcl_pops.melt$Var1))) +
              xlim(sort(levels(v$pexcl_pops.melt$Var2))) +
              ylab("Population groups") +
              xlab("Locus")
      )
      
      dev.off()
      
      #Download RMP
      tiff(filename = "RMP.tiff", 
           width = input$plot_width, 
           height = input$plot_height, 
           units = input$plot_units,
           res = input$plot_res,
           compression = "lzw")
      
      
      print(ggplot(v$GF.rmp.melt, aes(x = Var2, y = Var1)) + 
              ggtitle("Random match probability of loci between populations") +
              geom_tile(aes(fill = Y1), colour = "black") +
              scale_fill_brewer("RMP range",palette = 1)+
              theme(axis.text.x=element_text(angle=90, hjust = 1),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    plot.title = element_text(face="bold", size = 24, hjust = 0.5))+
              ylab("Population groups")+
              xlab("Locus")
            )
      
      dev.off()
      
      print(fs)

      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
  
  
  #Download metrics Excel
  
  output$dlmetrics <- downloadHandler(
    filename = function() {
      paste("Metrics FORSTAT", Sys.time(), "zip", sep = ".")
    },
    
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("metrics.xlsx","Frequencies.xlsx")
      
      v$CMP_out2 <- v$CMP_out[order(rownames(v$CMP_out), decreasing = F), ]
      v$CMP_out2 <- t(cbind("Pop Name" = rownames(v$CMP_out2), v$CMP_out2))
      
      
      write.xlsx(x=v$CMP_out2,
        file = "metrics.xlsx",
        sheetName = "CMP",
        col.names = F,
        row.names = T,
        append = F
      )
      
      write.xlsx(x=v$PICDFt_out,
                 file = "metrics.xlsx",
                 sheetName = "Polymorphic information content",
                 col.names = F,
                 row.names = F,
                 append = T
      )
      
      v$shexp_disp <- data.frame(arrange(as.data.frame(v$shexp_data_out), Loci))
      v$shexp_disp <- v$shexp_disp[v$shexp_disp[,1]!="Loci",]
      
      write.xlsx(x=v$shexp_disp,
      file = "metrics.xlsx",
      sheetName = "H exp",
      col.names = T,
      row.names = F,
      append = T
      )
      
      v$shobs_dl <- data.frame(arrange(as.data.frame(v$shobs_data_out), Loci))
      v$shobs_dl <- v$shobs_dl[v$shobs_dl[,1]!="Multilocus",]
      v$shobs_dl <- v$shobs_dl[v$shobs_dl[,1]!="Loci",]
      
      write.xlsx(x=v$shobs_dl,
      file = "metrics.xlsx",
      sheetName = "H obs",
      col.names = T,
      row.names = F,
      append = T
      )

      v$homozy3_out <- data.frame(arrange(as.data.frame(v$homozy2_out), Loci))
      v$homozy3_out <- v$homozy3_out[v$homozy3_out[,1]!="Multilocus",]
      v$homozy3_out <- v$homozy3_out[v$homozy3_out[,1]!="Loci",]
      
      write.xlsx(x=v$homozy3_out,
      file = "metrics.xlsx",
      sheetName = "homozygosity",
      col.names = T,
      row.names = F,
      append = T
      )
      
      v$PI_out <- t(v$PI)
      v$PI_out <- v$PI_out[,order(as.character(v$PI_out[1,]))]
      colnames(v$PI_out) <- as.character(v$PI_out[1,])
      v$PI_out <- cbind("Pop Name" = rownames(v$PI_out),v$PI_out, "Combined" = v$CPI[,2])
      v$PI_out <- t(v$PI_out[-1,])
      
      write.xlsx(x=v$PI_out,
      file = "metrics.xlsx",
      sheetName = "Paternity index",
      col.names = F,
      row.names = T,
      append = T
  )
      v$pexcl_pops_out2 <- v$pexcl_pops[,order(colnames(v$pexcl_pops))]
      v$pexcl_pops_out2 <- t(cbind("Loci" = rownames(v$pexcl_pops_out2),v$pexcl_pops_out2, "Combined"=v$pexcl_tot_pop))
            
      write.xlsx(x=v$pexcl_pops_out2,
      file = "metrics.xlsx",
      sheetName = "Power of exclusion",
      col.names = F,
      row.names = T,
      append = T
  )
      
      
      
      v$DCpl_dl <- cbind("Loci" = rownames(v$CDC_out),t(v$DCpl),v$CDC_out)
      v$DCpl_dl <- t(v$DCpl_dl[order(rownames(v$DCpl_dl)),])
      
      write.xlsx(x=v$DCpl_dl,
      file = "metrics.xlsx",
      sheetName = "Discrimination capacity",
      col.names = F,
      row.names = T,
      append = T
  )

      write.xlsx(x=v$FLp,
                 file = "Frequencies.xlsx",
                 sheetName = "Freq_overall",
                 col.names = T,
                 row.names = F,
                 append = F
      )
      
      rownames(v$fpt) <- names(v$pops)
      for(i in 1:v$nb)
      {
        v$fpt2 <- do.call(cbind,v$fpt[i])
        v$fpt2 <- v$fpt2[,order(colnames(v$fpt2)) ]
        write.xlsx(v$fpt2, "Frequencies.xlsx", sheetName = paste0(names(v$pops)[[i]],"_freq"), append = TRUE, row.names=TRUE)
      }
      
      print(fs)
      
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  
})