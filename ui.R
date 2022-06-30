
#Choice list
library(ggplot2)
library(gridExtra)
library(shiny)
library(shinythemes)
library(emojifont)
library(shiny)
require(rintrojs)
library(magrittr)
library(colourpicker)
library(shinyWidgets)
require(plyr)
library(shiny)
library(xtable)
require(tidyr)
library(plotly)
library(leaflet)
library(shinydashboard)
library(extrafont)
library(ggplot2)
require(lubridate)

#UI
ui =  function(request) {
  fluidPage(introjsUI(),
            ## use the prefix defined in
            ## addResourcePath
            #shinythemes::themeSelector(),
            
            theme = shinytheme("yeti"),
            
            titlePanel(div(
              h2(img(height = 100, width = 150, src = "pics/akilimo4.jpg"), id="big-heading", "SCENARIO ANALYSIS FOR FERTILIZER BLENDS"),
              tags$style(HTML("#big-heading{color: green;font-style: bold;}")))
              
            ),
            
            
            sidebarPanel(
              width = 3,
              
              
              p("Click the button below to see how the tool works."),
              
              actionButton("tour","Start Tour", icon("paper-plane"), 
                           style="color: #fff; background-color: #C660CE; border-color: #C660CE"),
              #actionButton("tour","Start Tour", class = "btn btn-primary btn-lg"),
              tags$hr(style="border-color: green;"),
              
              introBox(
                data.step = 1,
                data.intro = "Give cassava information here.",
                
                fluidRow(
                  column(5,selectInput("country", "Country", 
                                       choices = list("Tanzania" = "TZ", "Nigeria" = "NG"))),
                  
                  column(7,conditionalPanel(
                    condition = "input.country == 'NG'",
                    textInput("rootUP_NG", "Cassava root price (NGN): e.g. 12000")),
                    #, placeholder = "12000"
                    conditionalPanel(
                      condition = "input.country == 'TZ'",
                      textInput("rootUP_TZ", "Cassava root price (TZS): e.g 180000")
                      #, placeholder = "180000"
                    )),
                  column(12,conditionalPanel(
                    condition = "input.country == 'NG'",
                    selectInput("state", "Select states:", choices = c("Oyo", "Ogun", "Osun", "Ekiti", 
                                                                "Ondo", "Kogi", "Edo", "Benue","Cross River", "Enugu", "Anambra",
                                                                "Abia", "Akwa Ibom", "Delta", "Ebonyi",  
                                                                "Enugu", "Imo", "Kogi", "Kwara", "Taraba"), multiple = TRUE)),
                    
                    conditionalPanel(
                      condition = "input.country == 'TZ'",
                      selectInput("region", "Select up to 3 regions:", choices = c("Mtwara", "Lindi", "Tanga", "Geita",  "Kagera", "Kigoma", "Mara", 
                                                                   "Mwanza", "Pwani", "Shinyanga", "Simiyu", "Zanzibar North",
                                                                   "Zanzibar West", "Zanzibar South and Central Zanzibar West"), multiple = TRUE)
                    ))),
                
                tags$hr(style="border-color: green;"),
                fluidRow(
                  column(12,  selectInput(
                    "FCY", h5("Typical cassava root size in target area:"), choices = c("5 tonnes/ha", "12 tonnes/ha", "18 tonnes/ha"))),
                  column(12,
                         
                         awesomeCheckboxGroup(
                           inputId = "plantingMonths",
                           label = h5("Select your 4 main planting months below:"), 
                           #choices = c("Jan", "B", "C"),
                           choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                                       "October", "November", "December"),
                           
                           inline = TRUE, 
                           status = "warning"
                         )
                         
                  ),
                  column(12,
                         
                         numericInput("harvestDay", h5("How many months after planting will you harvest your cassava?"), value=12, min=8, max=15)
                         
                  )
                )),
              
             
              
              introBox(
                
                selectInput("nFert", h4('No. of Fertilizers to test:'),choices = c("3","4","5")),
                
                data.step = 2,
                data.intro = "Specify how many fertilizer options you would like testing on. The minimum is 3. 
                On the right you can fill out information for each of these fertilizer options."
              ),
              
              
              tags$hr(style="border-color: green;"),
              introBox(
                
                tags$a(href="https://akilimo.org", "Find out more about AKILIMO"),
                data.step=8,
                data.intro="See what we do at AKILIMO or get in contact with us!")),
            
            
            
            
            mainPanel(
              #helpText("Click on EACH of the fertilizer panels below and ensure you fill out all information before proceeding"),
              
              introBox(
                data.step = 3,
                data.intro = "Fill out information for each of the fertilizer options you requested",
                uiOutput('mytabs')),
              
              h5(id="each", "Click on EACH of the fertilizer panels ABOVE and ensure you fill out all information before proceeding. 
                 For N, P2O5 and K2O, give percent content, for example for NPK 15:15:15, the values are N=15, P2O5=7, K20=1.3"),
              tags$style(HTML("#each{font-style: italic; color: green;}")),
              
              tags$hr(style="border-color: green;"),
              
              introBox(
                data.step = 4,
                data.intro = "After filling out information for each fertilizer option, you are ready to submit for testing!!",
                
                
                fluidRow(
                  #     column(4, "Fertilizer Testing Tool"), 
                  #     column(4, img(height = 100, width = 150, src = "pics/akilimo4.jpg"))
                  #   )
                  column(4,actionButton("btn_set", "Ready to submit", icon("spinner"),
                                        style="color: #fff; background-color: red; border-color: #2e6da4")
                         
                  ),
                  uiOutput("btn_go")
                )
                
              ),
              
              br(),  
              
              fluidRow( 
                column(6, h5("Different scenarios will be creaed as a combination of types and prices")),
                column(6, tableOutput('Ferttable'))
                ),
              
              
              br(),
              
              tabsetPanel(type = "tabs", 
                          tabPanel("Plot", 
                                   
                                   introBox(
                                     title = "Plot", width = 100,height = '2000px', solidHeader = T, status = "success",
                                     #uiOutput("spinner"),
                                     data.step = 5,
                                     data.intro = "You can preview your plot and table here."
                                   )),
                          
                          tabPanel("Table", 
                                   
                                   fluidPage(DT::dataTableOutput('tbl')))),
              
              
              fluidRow( 
                column(4,
                       introBox(
                         bookmarkButton(id = "bookmark1", label = "Save Progress", class = "btn btn-primary btn-sm",
                                        style="color: #fff; background-color: #034012; border-color: #2e6da4"),
                         
                         data.step=6,
                         data.intro = "Save your progress here. Copy and Paste the link for another time!"),
                       br()),
                column(4,
                       introBox(
                         downloadButton(outputId = "down2", label = "Download .png",class = "btn btn-primary btn-sm",
                                        style="color: #fff; background-color: #0288D1; border-color: #2e6da4"),
                         
                         data.step=7,
                         data.intro="Download as png"),
                       br()),
                column(4,
                       introBox(
                         downloadButton(outputId = "down", label = "Download .pdf", class = "btn btn-primary btn-sm",
                                        style="color: #fff; background-color: #009688; border-color: #2e6da4"),
                         data.step=8,
                         data.intro="Download as high res pdf"))),
              
              
              hr()
              
              
              
              
            )) 
}

























