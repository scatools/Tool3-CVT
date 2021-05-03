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

hab_measure<-c( "Connectivity to Existing Protected Area "="hab1","Structural Connectivity Index "="hab2","Threat of Urbanization "="hab3","Land Cover - Composition of Natural Lands "="hab4")
wq_measure<-c("Impaired Watershed Area -- EPA '303(d)' list "="wq1","Stream Abundance "="wq2")
lcmr_measure<-c("Biodiversity Index "="lcmr1", "Threatened and Endangered Species - Critical Habitat Area "="lcmr2","Threatened and Endangered Species - Number of Species "="lcmr3","Light Pollution Index "="lcmr4")
cl_measure<-c("National Register of Historic Places "="cl1","National Heritage Area "="cl2","Social Vulnerability Index "="cl3","Community Threat Index "="cl4")
eco_measure<-c("Working Lands "="eco1","Commercial Fishery Index "="eco2", "Recreational Fishery Index "="eco3","Access & Recreation"="eco4")



hab_measure_name<-c( "Connectivity to Existing Protected Area ","Structural Connectivity Index ","Threat of Urbanization ","Land Cover - Composition of Natural Lands ")
wq_measure_name<-c("Impaired Watershed Area -- EPA '303(d)' list ","Stream Abundance ")
lcmr_measure_name<-c("Biodiversity Index ", "Threatened and Endangered Species - Critical Habitat Area ","Threatened and Endangered Species - Number of Species ","Light Pollution Index ")
cl_measure_name<-c("National Register of Historic Places ","National Heritage Area ","Social Vulnerability Index ","Community Threat Index ")
eco_measure_name<-c("Working Lands ","Commercial Fishery Index ", "Recreational Fishery Index ","Access & Recreation")





data0<-st_read('./data/sca_landonly_withdata.shp')
data0<-st_transform(data0,crs = 4326)
data1<-data0

datastate<-st_read('./data/SECoastStates.shp')
datastate<-st_transform(datastate,crs = 4326)

b <- c(-Inf, 0.25, 0.5, 0.75, Inf)
nameshab<-c("hab1","hab2","hab3","hab4")
nameshab1<-c("padus","area_conne","Sleuth_v2","conl_index")
nameswq<-c("wq1","wq2")
nameswq1<-c("Impaired_w","stream_abu")
nameslcmr<-c("lcmr1","lcmr2","lcmr3","lcmr4")
nameslcmr1<-c("Biodiversi","TnE_area","TnE_Count","area_light")
namescl<-c("cl1","cl2","cl3","cl4")
namescl1<-c("HistoricPl","area_nha","SOVInew","THREATINDE")
nameseco<-c("eco1","eco2","eco3","eco4")
nameseco1<-c("WORKINGLAN","ComEng_ct","RecEng_ct","AR_boat")

weightindex<-c("Zero","Low","Medium","High")
  