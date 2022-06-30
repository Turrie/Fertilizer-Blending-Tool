
#setwd("C:/Users/Turry/Documents/ACAI/DASHBOARDS/FertBlendingTool")
#setwd("/home/akilimo/projects/FertBlendingTool") 
#####  fertilizer scenario testing ...
# 
# library(dplyr)
# library(plyr)
# library(tidyr)
# library(knitr)
# library(rmarkdown)
# library(png)
# library(grid)
# library(kableExtra)
# library(leaflet)
# library(magrittr)
# library(mapview)
# library(mailR)
# library(httr)
# library(raster)
# library(dismo)
# library(randomForest)
# library(caret)
# library(rgdal)
# library(ggplot2)
# require(limSolve)
# require(lpSolve)
# require(lpSolveAPI)
# library(foreach)
# require(gtools)
# #library(doParallel)

source("cassavaCropMasterFunctions.R")
#####################################################################################
##  fertilizers available to be tested: user should give fertilizer name, NPK content, and price range and
## based on the prices given for the different fertilizers, there can be many combinations to be tested
## here there are three fertilizers, Urea with one price and teh other two NPKs  each with 3 different prices
## combinationa made 9 possible combinations of prices  = Nototre_prices
## 9 prices with 3 fertilizers result in 27 combinations where every three rwo give one possible price setting
## therefore when we generate recommendations, we should consider one scenario at a time
#####################################################################################


# str(Fetilizer_Notore)




#####################################################################################
## maximum investment, root prices, area measurement, farm size and planting and harvest dates
## plant and harvest dates: st= planting day, en =  harvest date (both in the ith dte of the year), weeknr = planting week number
## upto 15 monthes after planting but the user should select how long they assume the plant will stay on the field
#####################################################################################

maxInv_NG <- 72000 ## 200 $
maxInv_TZ <- 450000 ## 200 $
# if (country == "NG"){
#   maxInv = maxInv_NG
# }else{## NGN
#   maxInv = maxInv_TZ## NGN
# }



# rootUP = rootUP
# rootUP_NG <- 12000## NGN
# rootUP_TZ <- 180000## TZS

# country = country
# FCY = FCY
# areaHa <- 1 # one ha
# areaUnits <- "ha"
# Planting_Harvest <- data.frame(st=seq(1, 365, 7), en = (seq(1, 365, 7) + 454), weekNr = seq(1:53))


#setwd("C:/Users/Turry/Documents/ACAI/FertBlendingTool")
#setwd("/home/akilimo/projects/FertBlendingTool") 
source("cassavaCropMasterFunctions.R")

###########################################################################
## read soil data for the five soil fertility classes: we nortmally assum the average soil class being the FCY2
## if fertilizer companies want to specialize for por and fertilize soils differntly let them define the tool should allow

SoilData_fcy1_LGA <- readRDS("SoilData_fcy1_LGA.RDS")


SoilData_fcy2_LGA <- readRDS("SoilData_fcy2_LGA.RDS")


SoilData_fcy3_LGA <- readRDS("SoilData_fcy3_LGA.RDS")

# SoilData_fcy4_LGA <- readRDS("SoilData_fcy4_LGA.RDS")
# SoilData_fcy5_LGA <- readRDS("SoilData_fcy5_LGA.RDS")


SoilData_fcy1_Region <- readRDS("SoilData_fcy1_Region.RDS")


SoilData_fcy2_Region <- readRDS("SoilData_fcy2_Region.RDS")


SoilData_fcy3_Region <- readRDS("SoilData_fcy3_Region.RDS")

# SoilData_fcy4_Region <- readRDS("SoilData_fcy4_Region.RDS")
# SoilData_fcy5_Region <- readRDS("SoilData_fcy5_Region.RDS")


###########################################################################
## WLY data; computed assuming differnt sol fertility levels, the soil and the WLY data should align to the choice of the FCY
###########################################################################

## getting the day of the year when it is harvested (HD) only for NG, TZ has it already
getHD <- function(mdata){
  mdata$HD <- ifelse(mdata$harvestDay <= 365, mdata$harvestDay,
                     ifelse(mdata$harvestDay > 365 & abs(730 - mdata$harvestDay) <= 365,
                            abs(730 - mdata$harvestDay), abs(1095 - mdata$harvestDay)))
  return(mdata)
}


##

## NG LGA WLY and CY
WLY_CY_FCY1_LGA_A <- readRDS('WLY_CY_FCY1_LGA_A.RDS')
WLY_CY_FCY1_LGA_B <- readRDS('WLY_CY_FCY1_LGA_B.RDS')
WLY_CY_FCY1_LGA <- getHD(rbind(WLY_CY_FCY1_LGA_A, WLY_CY_FCY1_LGA_B))
WLY_CY_FCY2_LGA <- getHD(readRDS('WLY_CY_FCY2_LGA_A.RDS'))
# WLY_CY_FCY3_LGA_A <- readRDS('WLY_CY_FCY3_LGA_A.RDS')
# WLY_CY_FCY3_LGA_B <- readRDS('WLY_CY_FCY3_LGA_B.RDS')
# WLY_CY_FCY3_LGA <- getHD(rbind(WLY_CY_FCY3_LGA_A, WLY_CY_FCY3_LGA_B))
# WLY_CY_FCY4_LGA_A <- readRDS('WLY_CY_FCY4_LGA_A.RDS')
# WLY_CY_FCY4_LGA_B <- readRDS('WLY_CY_FCY4_LGA_B.RDS')
# WLY_CY_FCY4_LGA <- getHD(rbind(WLY_CY_FCY4_LGA_A, WLY_CY_FCY4_LGA_B))
# WLY_CY_FCY5_LGA <- getHD(readRDS('WLY_CY_FCY5_LGA.RDS'))


## here we are selecting that we work for 361 harvest at 361 (representing one year) days after planting

WLY_CY_FCY1_LGA <- WLY_CY_FCY1_LGA[WLY_CY_FCY1_LGA$daysOnField == "361", ]
WLY_CY_FCY2_LGA <- WLY_CY_FCY2_LGA[WLY_CY_FCY2_LGA$daysOnField == "361", ]
# WLY_CY_FCY3_LGA <- WLY_CY_FCY3_LGA[WLY_CY_FCY3_LGA$daysOnField == "361", ]
# WLY_CY_FCY4_LGA <- WLY_CY_FCY4_LGA[WLY_CY_FCY4_LGA$daysOnField == "361", ]
# WLY_CY_FCY5_LGA <- WLY_CY_FCY5_LGA[WLY_CY_FCY5_LGA$daysOnField == "361", ]



## TZ LGA WLY and CY
WLY_CY_FCY1_Region <- getHD(readRDS('WLY_CY_FCY1_Region.RDS'))
WLY_CY_FCY2_Region <- getHD(readRDS('WLY_CY_FCY2_Region.RDS'))
# WLY_CY_FCY3_Region <- getHD(readRDS('WLY_CY_FCY3_Region.RDS'))
# WLY_CY_FCY4_Region <- getHD(readRDS('WLY_CY_FCY4_Region.RDS'))
# WLY_CY_FCY5_Region <- getHD(readRDS('WLY_CY_FCY5_Region.RDS'))

WLY_CY_FCY1_Region <- WLY_CY_FCY1_Region[WLY_CY_FCY1_Region$daysOnField == "361", ]
WLY_CY_FCY2_Region <- WLY_CY_FCY2_Region[WLY_CY_FCY2_Region$daysOnField == "361", ]
# WLY_CY_FCY3_Region <- WLY_CY_FCY3_Region[WLY_CY_FCY3_Region$daysOnField == "361", ]
# WLY_CY_FCY4_Region <- WLY_CY_FCY4_Region[WLY_CY_FCY4_Region$daysOnField == "361", ]
# WLY_CY_FCY5_Region <- WLY_CY_FCY5_Region[WLY_CY_FCY5_Region$daysOnField == "361", ]



###########################################################################
## FCY 2 Notore fertilizers; for selected months of planting (Nototre indicated which months are main planting months)
## can be done for the whole year but it takes time so it is good if the tool can indicate which month to work with
## It can be done for the whoie region for which there are soil and WLY data for but Notore selects thier AOI (tool should allow this)
###########################################################################

##MC: condition this by input$FCY


get_FRRecom_FertTesting <- function(WLYDataA, fertilizers, SoilDatacy, maxInv, rootUP, country){
  Lintul_Recom_Notore <- NULL
  coords <- unique(WLYDataA$location)[1:2]
  for(loc in coords[1:length(coords)]){
    print(loc)
    wlyd <- WLYDataA[WLYDataA$location == loc, ]
    lat <- unique(wlyd$NAME_1)
    lon <- unique(wlyd$NAME_2)
    fertilizers <- fertilizers
    SoilData <- unique(SoilDatacy[SoilDatacy$location==loc, ])
    SoilData <- SoilData[complete.cases(SoilData),]
    plDates <- unique(wlyd$pl_Date)
    maxInv = maxInv
    rootUP = rootUP
    areaHa=1
    country=country
    
    perloc <- NULL
    for( i in 1:length(plDates)){
      WLYData <- unique(wlyd[wlyd$pl_Date == plDates[i], ])
      HD <- WLYData$HD
      getfr <- getFRrecommendations_NO(lat = lat,
                                       lon = lon,
                                       PD = plDates[i],
                                       maxInv = maxInv,
                                       fertilizers=fertilizers,
                                       wlyd = wlyd ,
                                       SoilData = SoilData,
                                       rootUP = rootUP, areaHa=1, country=country)
      perloc <- rbind(perloc, getfr)
    }
    Lintul_Recom_Notore <- rbind(Lintul_Recom_Notore, perloc)
  }
  return(Lintul_Recom_Notore)
}

#Scenarios_Recom <- readRDS("Scenarios_Recom.RDS")
# 
# gg <- ggplot(data = Scenarios_Recom, aes(x=Fertilizer, y=FertTon, fill=Fertilizer)) +
#         geom_bar(stat="identity")+
#         facet_wrap(~price)+
#         ggtitle("Price ratios shown in the facets")+
#         xlab("") + ylab("FSertilizer amount (ton)") +
#         theme_bw()+
#         theme(legend.position = "none")
# 
# 
# ggsave(paste("Tested_Fertilizers_", Sys.Date(), ".png", sep=""), gg, width=10, height = 8)
# 
# head(Scenarios_Recom)

# 
# country <- "TZ"
# region <-"Mtwara"
# plantingMonths
# plantingMonths <- c("August", "September", "October")
# SoilDatacy <- SoilData_fcy2_Region
# WLYDataA <- WLY_CY_FCY2_Region
# maxInv <- 72000
# rootUP <- 180000

# Scenarios_Recom <- function(country, FCY, region, plantingMonths, Fetilizer_Notore, SoilDatacy, WLYDataA, maxInv, rootUP){
#   
#   Month_WkNr <- data.frame(monthName = c(rep("January", 5), rep("February", 4), rep("March", 5), rep("April", 4),
#                                          rep("May", 5), rep("June", 4), rep("July", 5), rep("August", 4),
#                                          rep("September", 4), rep("October", 5), rep("November", 4),
#                                          rep("December", 4)), WeekNr = c(1:53) )
#   
#   
#   ## input$plantingMonths coming form the tool as vector of planting months selected
#   PlantingWeekNr <- droplevels(Month_WkNr[Month_WkNr$monthName %in% plantingMonths, ])
#   
#   WLYDataA <- droplevels(WLYDataA[WLYDataA$PlweekNr %in% c(min(PlantingWeekNr$WeekNr):max(PlantingWeekNr$WeekNr)), ])
#   
#   if(country == "NG"){
#     WLYDataA <- droplevels(WLYDataA[WLYDataA$NAME_1 %in% state, ])
#   }else if ( country == "TZ"){
#     WLYDataA <- droplevels(WLYDataA[WLYDataA$NAME_1 %in% region, ])
#   }
#   
#   #WLYDataA <- droplevels(WLYDataA[WLYDataA$NAME_1 %in% c("Oyo", "Ogun", "Osun", "Ekiti", "Ondo", "Kogi", "Edo", "Benue","Cross River", "Enugu", "Anambra"), ])
#   
#   # maxInv <- maxInv_NG
#   # rootUP <- rootUP_NG
#   #country <- c("TZ", "NG")
#   SoilDatacy$location <- paste(SoilDatacy$NAME_1, SoilDatacy$NAME_2, sep="_")
#   WLYDataA$location <- paste(WLYDataA$NAME_1, WLYDataA$NAME_2, sep="_")
#   
#   nrow(SoilDatacy)
#   nrow(WLYDataA)
#   head(SoilDatacy)
#   head(WLYDataA)
#   
#   unique(Fetilizer_Notore$scenario) ## sceanrios are combination of fertilzer types and prices and recommendatinshould be generated per scenario
#   
#   Scenarios_Recom <- NULL
#   for(scenarios in unique(Fetilizer_Notore$scenario)){
#     fertilizers <- droplevels(Fetilizer_Notore[Fetilizer_Notore$scenario == scenarios,])
#     fertilizers <- fertilizers[, c("type", "N_cont", "P_cont", "K_cont","price")]
#     oneScenario_Recom <- get_FRRecom_FertTesting(WLYDataA=WLYDataA, fertilizers=fertilizers, SoilDatacy=SoilDatacy, maxInv=maxInv, rootUP=rootUP, country=country)
#     oneScenario_Recom$price <- paste(fertilizers$price[1]*50, ":", fertilizers$price[2]*50, ":", fertilizers$price[3]*50, sep="")
#     Scenarios_Recom <- rbind(Scenarios_Recom, oneScenario_Recom)
#     saveRDS(oneScenario_Recom, paste("oneScenario_Recom", scenarios, ".RDS", sep="")) ## incase server goes down, by scenario result will be saved
#   }
#   
#   head(Scenarios_Recom)
#   
#   type = c(newFert1name=newFert1name, newFert2name=newFert2name, newFert3name=newFert3name, newFert4name=newFert4name, newFert5name=newFert5name)
#   type <- type[!is.na(type)]
#   
#   
#   Scenarios_Recom <- gather(Scenarios_Recom[, c("lat","lon", "plDate", "price",type)], Fertilizer, measurement,
#                             type[1]:type[length(type)], factor_key=TRUE)
#   Scenarios_Recom$FertTon <- Scenarios_Recom$measurement/1000
#   
#   if(file.exists("Scenarios_Recom.RDS")) file.remove("Scenarios_Recom.RDS")
#   
#   saveRDS(Scenarios_Recom, "Scenarios_Recom.RDS")
#   return(Scenarios_Recom)
#   
# }
# 



Scenarios_Recom <- function(country, FCY, plantingMonths, Fetilizer_Notore, SoilDatacy, WLYDataA, maxInv, rootUP, type, regions){
  
  Month_WkNr <- data.frame(monthName = c(rep("January", 5), rep("February", 4), rep("March", 5), rep("April", 4),
                                         rep("May", 5), rep("June", 4), rep("July", 5), rep("August", 4),
                                         rep("September", 4), rep("October", 5), rep("November", 4),
                                         rep("December", 4)), WeekNr = c(1:53) )
  
  
  ## input$plantingMonths coming form the tool as vector of planting months selected
  PlantingWeekNr <- droplevels(Month_WkNr[Month_WkNr$monthName %in% plantingMonths, ])
  
  WLYDataA <- droplevels(WLYDataA[WLYDataA$PlweekNr %in% c(min(PlantingWeekNr$WeekNr):max(PlantingWeekNr$WeekNr)), ])
  WLYDataA <- droplevels(WLYDataA[WLYDataA$NAME_1 %in% regions, ])
  
  
  #WLYDataA <- droplevels(WLYDataA[WLYDataA$NAME_1 %in% c("Oyo", "Ogun", "Osun", "Ekiti", "Ondo", "Kogi", "Edo", "Benue","Cross River", "Enugu", "Anambra"), ])
  
  # maxInv <- maxInv_NG
  # rootUP <- rootUP_NG
  #country <- c("TZ", "NG")
  SoilDatacy$location <- paste(SoilDatacy$NAME_1, SoilDatacy$NAME_2, sep="_")
  WLYDataA$location <- paste(WLYDataA$NAME_1, WLYDataA$NAME_2, sep="_")
  
  nrow(SoilDatacy)
  nrow(WLYDataA)
  head(SoilDatacy)
  head(WLYDataA)
  
  
  unique(Fetilizer_Notore$scenario) ## sceanrios are combination of fertilzer types and prices and recommendatinshould be generated per scenario
  
  Scenarios_Recom <- NULL
  for(scenarios in unique(Fetilizer_Notore$scenario)){
    fertilizers <- droplevels(Fetilizer_Notore[Fetilizer_Notore$scenario == scenarios,])
    fertilizers <- fertilizers[, c("type", "N_cont", "P_cont", "K_cont","price")]
    oneScenario_Recom <- get_FRRecom_FertTesting(WLYDataA=WLYDataA, fertilizers=fertilizers, SoilDatacy=SoilDatacy, maxInv=maxInv, rootUP=rootUP, country=country)
    oneScenario_Recom$price <- paste(fertilizers$price[1]*50, ":", fertilizers$price[2]*50, ":", fertilizers$price[3]*50, sep="")
    Scenarios_Recom <- rbind(Scenarios_Recom, oneScenario_Recom)
    saveRDS(oneScenario_Recom, paste("oneScenario_Recom", scenarios, ".RDS", sep="")) ## incase server goes down, by scenario result will be saved
  }
  
  Scenarios_Recom <- gather(Scenarios_Recom[, c("lat","lon", "plDate", "price",type)], Fertilizer, measurement,
                            type[1]:type[length(type)], factor_key=TRUE)
  Scenarios_Recom$FertTon <- Scenarios_Recom$measurement/1000
  
  # if(file.exists("Scenarios_Recom.RDS")) file.remove("Scenarios_Recom.RDS")
  
  saveRDS(Scenarios_Recom, "Scenarios_Recom.RDS")
  return(Scenarios_Recom)
  
}



