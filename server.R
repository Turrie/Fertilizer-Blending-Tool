

library(ggplot2)
library(gridExtra)
library(shiny)
library(shinythemes)
library(emojifont)
library(shiny)
require(rintrojs)
library(magrittr)
library(shinyWidgets)
library(colourpicker)
require(plyr)
library(shiny)
library(emojifont)
library(xtable)
require(tidyr)
library(plotly)
library(leaflet)
library(shinydashboard)
library(extrafont)
library(ggplot2)
library(dplyr)

#setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/FertBlendingTool")

#setwd("/home/akilimo/projects/FertBlendingTool")
source("cassavaCropMasterFunctions.R", local=TRUE)
source("FertilizerTesting.R", local=TRUE)

# values <- NULL
# input <- NULL
# input$nFert <- 3
# input$country <- "TZ"
# input$newFert3BagWt <- 50
# input$FCY <- "12 tonnes/ha"
# input$rootUP_NG <- 12000
# input$rootUP_TZ <- 180000
# input$plantingMonths <- c("July", "August", "September")
# input$state <- "Oyo"
# input$region <- "Mwanza"
# input$newFert1name <- "AA"
# input$newFert1N_Perc <- 46
# input$newFert1P2O5_Perc <- 0
# input$newFert1K2O_Perc <- 0
# input$newFert1CostperBag <- c(58000, 61000)
# input$newFert1BagWt <- 50
# input$newFert2name <- "BB"
# input$newFert2N_Perc <- 15
# input$newFert2P2O5_Perc <- 7
# input$newFert2K2O_Perc <- 12
# input$newFert2CostperBag <- 63000
# input$newFert2BagWt <- 50
# input$newFert3name <- "KK"
# input$newFert3N_Perc <- 0
# input$newFert3P2O5_Perc <- 20
# input$newFert3K2O_Perc <- 18
# input$newFert3CostperBag <- 56000
# input$newFert3BagWt <- 50


#addResourcePath(prefix = 'pics', directoryPath = "C:/Users/Turry/Documents/ACAI/FertBlendingTool2/server version 25082020")
tags$link(rel="stylesheet", type="text/css", href="styles.css")

#Server
server = function(input, output, session){
  
  # Reactive Values --------------------------
  values <- reactiveValues()
  values$data <- data.frame(stringsAsFactors = F)
  values$work_data <- data.frame(stringsAsFactors = F)
  values$pmf_data <- data.frame(stringsAsFactors = F)
  values$reject_line <- 0
  values$selected_columns <- c()
  values$Fetilizer_Notore <- NULL
  #Bookmarker
  setBookmarkExclude(c("bookmark1","tour"))
  
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  

  #Tour 
  observeEvent(input$tour,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Back"
               )))
  
  #Start Tabset
  output$mytabs <- renderUI({
    
    nTabs = input$nFert
    if(is.null(input$nFert) ) {
      nTabs <- 3
    }
    
   
    
    # if(nTabs > 5) {
    #     number <- input$nFert
    #     min_number <- 3
    #     if(number < min_number | number > 5) {
    #       ## Conditions
    #       number2 <- warning(paste0("Please provide at least 3 fertilizer options and a maximum of 5"))
    #     }
    #     output$out2 <- renderText({number2})
    # }
    colours <- heat.colors(nTabs) 
    
    myTabs = lapply(1:nTabs, function(i) {
      tabPanel(paste0("Fertilizer ",i),
               hr(),
               
               fluidRow(column(3, textInput(paste0("newFert",i, "name"), paste0("Name of Fertilizer", i,  sep = " "))),
                        column(1,textInput(paste0("newFert",i, "N_Perc", sep = ""), "N (%)")),
                        column(1,textInput(paste0("newFert",i, "P2O5_Perc", sep = ""), "P2O5 (%)")),
                        column(1,textInput(paste0("newFert",i, "K2O_Perc", sep = ""), "K2O (%)")),
                        column(2,textInput(paste0("newFert", i, "BagWt", sep = ""), "Bag weight (Kg)")),
                        
                       column(4, 
                               if(input$country == "NG"){
                                 conditionalPanel(
                                   condition = "input.country == 'NG'",
                                   textInput(paste0("newFert", i, "CostperBag", sep = ""), paste0("Costs per bag (Naira) e.g 6000,7000,8000", sep = "")))
                               }else{
                                 conditionalPanel(
                                   condition = "input.country == 'TZ'",
                                   textInput(paste0("newFert", i, "CostperBag", sep = ""), paste0("Costs per bag (TZS) e.g 58000,60000,54000", sep = "")))
                               }
                          #      
                          # conditionalPanel(
                          #   condition = "input.country == 'NG'",
                          #   textInput(paste0("newFert", i, "CostperBag", sep = ""), paste0("Costs per bag (Naira) e.g 6000,7000,8000", sep = ""))),
                          # 
                          # conditionalPanel(
                          #   condition = "input.country == 'TZ'",
                          #   textInput(paste0("newFert", i, "CostperBag", sep = ""), paste0("Costs per bag (TZS) e.g 58000,60000,54000", sep = "")))
                                     
               
      ))
      )
      
    })
    
    #Border Tab inserted manually   
    # myTabs[[nTabs + 1]] <-  tabPanel("Cassava information", 
    #                                  
    #                                  h3("Text"),
    #                                
    #                                    fluidRow( 
    #                                      column(3,
    #                                             selectInput("country", "Country", 
    #                                                         choices = list("Tanzania" = "TZ", "Nigeria" = "NG"),
    #                                                         selected = 1)),
    #                                      column(3,conditionalPanel(
    #                                        condition = "input.country == 'NG'",
    #                                        textInput("rootUP_NG", "Cassava root price (NGN):")),
    #                                        
    #                                        conditionalPanel(
    #                                          condition = "input.country == 'TZ'",
    #                                          textInput("rootUP_TZ", "Cassava root price (TZ):")
    #                                        )),
    #                                      column(3,  selectInput(
    #                                        "FCY", "Typical cassava root size in target area (Tonnes):", choices = c("5", "12", "18")
    #                                        
    #                                      )),
    #                                      
    #                                      
    #                                      column(3,textInput("plantingmonths", "Which are your main planting months? (Separate months using a comma e.g May, June, July")),
    #                                      
    #                                      fluidRow(
    #                                        
    #                                        column(12, actionButton("btn_go", "Submit for testing", class = "btn btn-primary btn-lg"))
    #                                      ) ,
    #                                      
    #                                      data.step = 2,
    #                                      data.intro = "Start by providing information on your country and cassava.")
    #                                  
    #                                  
    # )
    do.call(tabsetPanel,myTabs)
  })
  
  #button to ensure all fields are filled before pressing submit button
  
  
  observeEvent(input$btn_set,{
    observe(
      if(input$nFert == 3 & is.null(input$newFert3BagWt) || input$newFert3BagWt == ""){
        
        
        showModal(modalDialog(title ="Warning!!!", "Please fill all the fertilizer options before you click the Submit buttion!!!"))
      }
      else{
        output$btn_go <- renderUI({
          actionButton("btn_go", "Submit for testing", icon("paper-plane"),
                       style="color: #fff; background-color: green; border-color: #2e6da4")
          
        })
      })
  })
  
  observeEvent(input$btn_go, {
    
    shinybusy::show_modal_spinner(
      spin = "cube-grid",
      color = "firebrick",
      text = "Please wait..."
    )
    # Sys.sleep(4)
    # remove_modal_spinner()
  })
  
  observeEvent(input$btn_go, {
    
    if(!is.null(input$country) & !is.null(input$nFert) & !is.null(input$FCY)) {
      
      
      rootUP_NG <- as.numeric(input$rootUP_NG)
      rootUP_TZ <- as.numeric(input$rootUP_TZ)
      country <- input$country
      FCY <- input$FCY
      
      if (input$country == "NG"){
        rootUP <- as.numeric(input$rootUP_NG)
        regions <- input$state
        maxInv = maxInv_NG
      }else{## NGN
        rootUP <- as.numeric(input$rootUP_TZ)# TZS
        regions <- input$regions
        maxInv = maxInv_TZ## NGN
      }
      # 
      
     
      plantingMonths <- input$plantingMonths
      print(plantingMonths)
       
     
      newFert1name <- input$newFert1name
      newFert1N_Perc <- as.numeric(input$newFert1N_Perc)
      newFert1P2O5_Perc <- as.numeric(input$newFert1P2O5_Perc)
      newFert1K2O_Perc <- as.numeric(input$newFert1K2O_Perc)
      newFert1CostperBag <- c(input$newFert1CostperBag)
      newFert1BagWt <- as.numeric(input$newFert1BagWt)
      
      newFert2name <- input$newFert2name
      newFert2N_Perc <- as.numeric(input$newFert2N_Perc)
      newFert2P2O5_Perc <- as.numeric(input$newFert2P2O5_Perc)
      newFert2K2O_Perc <- as.numeric(input$newFert2K2O_Perc)
      newFert2CostperBag <- c(input$newFert2CostperBag)
      newFert2BagWt <- as.numeric(input$newFert2BagWt)
      
    
      newFert3name <- input$newFert3name
      newFert3N_Perc <- as.numeric(input$newFert3N_Perc)
      newFert3P2O5_Perc <- as.numeric(input$newFert3P2O5_Perc)
      newFert3K2O_Perc <- as.numeric(input$newFert3K2O_Perc)
      newFert3CostperBag <- c(input$newFert3CostperBag)
      newFert3BagWt <- as.numeric(input$newFert3BagWt)
      
      
      print(newFert1CostperBag)
      print(newFert2CostperBag)
      print(newFert3CostperBag)
      
      if(input$nFert == 4){
        newFert4name <- input$newFert4name
        newFert4N_Perc <- as.numeric(input$newFert4N_Perc)
        newFert4P2O5_Perc <- as.numeric(input$newFert4P2O5_Perc)
        newFert4K2O_Perc <- as.numeric(input$newFert4K2O_Perc)
        newFert4CostperBag <- c(input$newFert4CostperBag)
        newFert4BagWt <- as.numeric(input$newFert4BagWt)
      }else{
        newFert4name <- NA
        newFert4name <- NA
        newFert4N_Perc <- NA
        newFert4P2O5_Perc <- NA
        newFert4K2O_Perc <- NA
        newFert4CostperBag <- NA
        newFert4BagWt <- NA
      }
      
      if(input$nFert == 5){
        newFert5name <- input$newFert5name
        newFert5N_Perc <- as.numeric(input$newFert5N_Perc)
        newFert5P2O5_Perc <- as.numeric(input$newFert5P2O5_Perc)
        newFert5K2O_Perc <- as.numeric(input$newFert5K2O_Perc)
        newFert5CostperBag <- c(input$newFert5CostperBag)
        newFert5BagWt <- as.numeric(input$newFert5BagWt)
      }else{
        newFert5name <- NA
        newFert5name <- NA
        newFert5N_Perc <- NA
        newFert5P2O5_Perc <- NA
        newFert5K2O_Perc <- NA
        newFert5CostperBag <- NA
        newFert5BagWt <- NA
      }
      
      print(newFert1BagWt)
      print(newFert2BagWt)
      print(newFert3BagWt)
      values$work_data <- data.frame(country=country, newFert1name=newFert1name, newFert1N_Perc, newFert1P2O5_Perc,newFert1K2O_Perc, newFert1CostperBag, newFert1BagWt, 
                                     newFert2name, newFert2N_Perc, newFert2P2O5_Perc, newFert2K2O_Perc, newFert2CostperBag,newFert2BagWt,
                                     newFert3name, newFert3N_Perc, newFert3P2O5_Perc, newFert3K2O_Perc, newFert3CostperBag,newFert3BagWt,
                                     newFert4name, newFert4N_Perc, newFert4P2O5_Perc, newFert4K2O_Perc, newFert4CostperBag,newFert4BagWt, 
                                     newFert5name, newFert5N_Perc, newFert5P2O5_Perc, newFert5K2O_Perc, newFert5CostperBag,newFert5BagWt, 
                                     stringsAsFactors = FALSE)
      
      print(values$work_data)
      
      
      
      
      
      ### Define fertilizers
      # 
     # fertilizer_forTesting(newFert1name='lala', newFert1N_Perc=46, newFert1P2O5_Perc=0,newFert1K2O_Perc=0, newFert1CostperBag=7000, newFert1BagWt=50,
     #     newFert2name="dada", newFert2N_Perc=15, newFert2P2O5_Perc=15, newFert2K2O_Perc=15, newFert2CostperBag= "6000, 7000", newFert2BagWt=50,
     #     newFert3name="tata", newFert3N_Perc=20, newFert3P2O5_Perc=12, newFert3K2O_Perc=26, newFert3CostperBag=8000, newFert3BagWt=50,
     #     newFert4name=NA, newFert4N_Perc=NA, newFert4P2O5_Perc=NA, newFert4K2O_Perc=NA, newFert4CostperBag=NA, newFert4BagWt=NA,
     #     newFert5name=NA, newFert5N_Perc=NA, newFert5P2O5_Perc=NA, newFert5K2O_Perc=NA, newFert5CostperBag=NA, newFert5BagWt=NA)
     #  # # # 
      # # 
      
      
      Fetilizer_Notore <- fertilizer_forTesting(newFert1name =newFert1name, newFert1N_Perc =newFert1N_Perc, newFert1P2O5_Perc =newFert1P2O5_Perc,
                                                newFert1K2O_Perc =newFert1K2O_Perc, newFert1CostperBag =newFert1CostperBag,newFert1BagWt =newFert1BagWt,
                                                newFert2name =newFert2name, newFert2N_Perc =newFert2N_Perc,newFert2P2O5_Perc =newFert2P2O5_Perc,
                                                newFert2K2O_Perc =newFert2K2O_Perc, newFert2CostperBag =newFert2CostperBag, newFert2BagWt =newFert2BagWt,
                                                newFert3name =newFert3name, newFert3N_Perc =newFert3N_Perc, newFert3P2O5_Perc =newFert3P2O5_Perc,
                                                newFert3K2O_Perc =newFert3K2O_Perc, newFert3CostperBag =newFert3CostperBag, newFert3BagWt =newFert3BagWt,
                                                newFert4name =newFert4name, newFert4N_Perc =newFert4N_Perc, newFert4P2O5_Perc =newFert4P2O5_Perc,
                                                newFert4K2O_Perc =newFert4K2O_Perc, newFert4CostperBag =newFert4CostperBag, newFert4BagWt =newFert4BagWt,
                                                newFert5name =newFert5name, newFert5N_Perc =newFert5N_Perc, newFert5P2O5_Perc =newFert5P2O5_Perc,
                                                newFert5K2O_Perc =newFert5K2O_Perc, newFert5CostperBag =newFert5CostperBag, newFert5BagWt =newFert5BagWt)
      
      
      values$Fetilizer_Notore <- Fetilizer_Notore
      print(values$Fetilizer_Notore)
      # bind data
      values$data <- bind_rows(values$data, values$work_data)
      
     
      if(FCY=="5 tonnes/ha"){
        FCY <- 1
      }else if(FCY=="12 tonnes/ha"){
        FCY <- 2
      }else if(FCY=="18 tonnes/ha"){
        FCY <- 3
      }
     
      state <- input$state
      regions <- input$region
      plantingMonths <- input$plantingMonths
      
      if(country == 'NG'){
        if(FCY == 2){
          SoilDatacy <- unique(SoilData_fcy2_LGA) ## soil data for FCY2
          WLYDataA <- WLY_CY_FCY2_LGA ## WLY data for FCY 2
        }else if(FCY == 1){
          SoilDatacy <- unique(SoilData_fcy1_LGA) ## soil data for FCY2
          WLYDataA <- WLY_CY_FCY1_LGA ## WLY data for FCY 2
        }else if(FCY == 3){
          SoilDatacy <- unique(SoilData_fcy3_LGA) ## soil data for FCY2
          WLYDataA <- WLY_CY_FCY3_LGA ## WLY data for FCY 2
        }
      } else if (country == 'TZ'){
        if(FCY == 2){
          SoilDatacy <- unique(SoilData_fcy2_Region)
          WLYDataA <- WLY_CY_FCY2_Region
        } else if(FCY == 1){
          SoilDatacy <- unique(SoilData_fcy1_Region) ## soil data for FCY2
          WLYDataA <- SoilData_fcy1_Region ## WLY data for FCY 2
        }else if(FCY == 3){
          SoilDatacy <- unique(SoilData_fcy3_Region) ## soil data for FCY2
          WLYDataA <- SoilData_fcy3_Region ## WLY data for FCY 2
        }
      }
      
      
      type = c(newFert1name=newFert1name, newFert2name=newFert2name, newFert3name=newFert3name, newFert4name=newFert4name, newFert5name=newFert5name)
      type <- type[!is.na(type)]
     
      
      ### create a function gto get Scenarios_Recom with input data from the server
      DF <- Scenarios_Recom(country = country, FCY = FCY, plantingMonths = plantingMonths, Fetilizer_Notore = Fetilizer_Notore, SoilDatacy=SoilDatacy, WLYDataA=WLYDataA,maxInv=maxInv, rootUP=rootUP, type=type, regions=regions)
      
      # DF <- readRDS("Scenarios_Recom.RDS")
      
      output$Ferttable <- renderTable(Fetilizer_Notore)
      
    #   url<-reactive(
    #     paste0("https://www.rstudio.com/wp-content/uploads/2014/07/", input$imageOptions)
    #   )
    #   output$logo<-renderText({
    #     validate(need(input$getImage, "")) #I'm sending an empty string as message.
    #     input$getImage
    #     URL<-isolate(url())
    #     print(URL)
    #     Sys.sleep(2)
    #     c('<center><img src="', URL, '"width="50%" height="50%" align="middle"></center>')
    #   })
    # }
      
      output$gg<-renderPlot({
        if (!is.null(DF)){
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         sum(runif(10000000,0,1))
                       }
                     })
        gg <-  ggplot(data = DF, aes(x=Fertilizer, y=FertTon, fill=Fertilizer)) +
          geom_bar(stat="identity")+
          facet_wrap(~price)+
          ggtitle("Price ratios shown in the facets")+
          xlab("") + ylab("Fertilizer amount (ton)") +
          theme_bw()+
          theme(legend.position = "none")
        
        print(gg)
        }
      })
      
      # observeEvent(input$btn_go, {
      #   output$spinner <- renderUI({
      #     withSpinner(plotOutput("gg"), color="black")
      #   })
      #   
      #   output$gg<-renderPlot({
      #     
      #     if (!is.null(DF)){
      #       # validate(need(input$btn_go, "")) #I'm sending an empty string as message.
      #       # input$btn_go
      #       gg <-  ggplot(data = DF, aes(x=Fertilizer, y=FertTon, fill=Fertilizer)) +
      #         geom_bar(stat="identity")+
      #         facet_wrap(~price)+
      #         ggtitle("Price ratios shown in the facets")+
      #         xlab("") + ylab("Fertilizer amount (ton)") +
      #         theme_bw()+
      #         theme(legend.position = "none")
      #       
      #       print(gg)
      #       
      #       
      #       #ggpng <- ggsave(paste("Tested_Fertilizers_", Sys.Date(), ".png", sep=""), gg, width=10, height = 8)
      #     }
      #   })
      #   #removeModal()
      # })
      
    
      
      output$tbl = DT::renderDataTable(
        DF, options = list(
          lengthChange = FALSE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
            "}"),
          autowidth = TRUE,
          columnDefs = list(list(width = '70%', targets = 1))
        )
      )
    }# save
    
    
  })
  
  
  
  output$down <- downloadHandler(
    filename =  function() {
      paste("file" ,".pdf", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      ggsave(paste("Tested_Fertilizers_", Sys.Date(), ".png", sep=""), gg, width=10, height = 8)
      dev.off()
      
    })
  
  output$down2 <- downloadHandler(
    filename =  function() {
      paste("file" ,".png", sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      ggsave(paste("Tested_Fertilizers_", Sys.Date(), ".png", sep=""), gg, width=10, height = 8)
      dev.off()
      
    })
  
}




#Run

#shinyApp(ui=ui, server=server, enableBookmarking = "url")



