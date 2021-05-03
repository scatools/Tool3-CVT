library(shiny)
library(shinyalert)
library(leaflet)
library(leafgl)
library(leaflet.extras)
library(leaflet.providers)
library(shinyjs)
library(httr)
library(dplyr)
library(curl)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(shinydashboardPlus)
library(sf)
library(mapview)
library(colourvalues)

####IMPORTANT WQ NAMING CHANGE#####
#Since we removed stream abundance (wq2), the naming convention in the code has required a shift in naming wq parameters.  
#Thus, hydro response to land use change is now "wq2" instead of "wq3" (and so on) so that the code in the server doesn't have to shift.  
#Please reflect that change in the CPT names as well.
hab_measure<-c( "Connectivity to Existing Protected Area "="hab1","Structural Connectivity Index "="hab2","Threat of Urbanization "="hab3","Land Cover - Composition of Natural Lands "="hab4")
wq_measure<-c("Impaired Watershed Area -- EPA '303(d)' list "="wq1","Hydrologic Response to Land-Use Change "="wq2","Percent Irrigated Agriculture "="wq3","Lateral Connectivity to Floodplain "="wq4","Composition of Riparian Zone Lands "="wq5")
lcmr_measure<-c("Biodiversity Index "="lcmr1", "Threatened and Endangered Species - Critical Habitat Area "="lcmr2","Threatened and Endangered Species - Number of Species "="lcmr3","Light Pollution Index "="lcmr4")
cl_measure<-c("National Register of Historic Places "="cl1","National Heritage Area "="cl2","Social Vulnerability Index "="cl3","Community Threat Index "="cl4")
eco_measure<-c("Working Lands "="eco1","Commercial Fishery Index "="eco2", "Recreational Fishery Index "="eco3","Access & Recreation"="eco4")



hab_measure_name<-c( "Connectivity to Existing Protected Area ","Structural Connectivity Index ","Threat of Urbanization ","Land Cover - Composition of Natural Lands ")
wq_measure_name<-c("Impaired Watershed Area -- EPA '303(d)' list ","Hydrologic Response to Land-Use Change ","Percent Irrigated Agriculture ","Lateral Connectivity to Floodplain ","Composition of Riparian Zone Lands ")
lcmr_measure_name<-c("Biodiversity Index ", "Threatened and Endangered Species - Critical Habitat Area ","Threatened and Endangered Species - Number of Species ","Light Pollution Index ")
cl_measure_name<-c("National Register of Historic Places ","National Heritage Area ","Social Vulnerability Index ","Community Threat Index ")
eco_measure_name<-c("Working Lands ","Commercial Fishery Index ", "Recreational Fishery Index ","Access & Recreation")





data0<-st_read('./data/sca_landonly_withdata3.shp')
data0<-st_transform(data0,crs = 4326)
data1<-data0

datastate<-st_read('./data/SECoastStates.shp')
datastate<-st_transform(datastate,crs = 4326)
b <- c(-Inf,0 , 0.25, 0.5, 0.75, 0.9, 0.95, 1, Inf)  ### #
nameshab<-c("hab1","hab2","hab3","hab4")
nameshab1<-c("padus","area_conne","Sleuth_v2","conl_index")
nameswq<-c("wq1","wq2","wq3","wq4","wq5")
nameswq1<-c("Impaired_w","wq2","wq3","wq4","wq5")### #
nameslcmr<-c("lcmr1","lcmr2","lcmr3","lcmr4")
nameslcmr1<-c("Biodiversi","TnE_area","TnE_Count","area_light")
namescl<-c("cl1","cl2","cl3","cl4")
namescl1<-c("HistoricPl","area_nha","SOVInew","THREATINDE")
nameseco<-c("eco1","eco2","eco3","eco4")
nameseco1<-c("WORKINGLAN","ComEng_ct","RecEng_ct","AR_boat")

weightindex<-c("Zero","Low","Medium","High")
  