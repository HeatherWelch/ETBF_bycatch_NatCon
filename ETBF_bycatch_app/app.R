#
# This is a Shiny web application. You can run the application by clicking

###### 1. load libraries #####
library(tidyverse)
library(shiny)
library(rsconnect)
library(raster)
library(leaflet)
library(shinythemes)
library(RColorBrewer)
library(zoo)
library(rgdal)

###### 2. define global objects #####
full_models_dir="data/SDMs/"
threat_dir="data/total_threat_tif/"
species=c("Blue shark","Bronze whaler shark","Dusky whaler shark","Oceanic whitetip shark","Shortfin mako shark","Silky shark", "Tiger shark")
years=as.character(seq(1998,2007))
mn=c("01","02","03","04","05","06","07","08","09","10","11","12")
months=c("January","February","March","April","May","June","July","August","September","October","November","December")
yearmon=list()
for(year in years){
  for(month in months){
  t=paste0(month," ",year)
  yearmon=unlist(list(yearmon,t))
  }}

yearmon_numeric=list()
for(year in years){
  for(m in mn){
    t=paste0(year,m)
    yearmon_numeric=unlist(list(yearmon_numeric,t))
  }}

NTA_2015=readOGR("data/layers/protected_2015_line.shp")
NTA_2012=readOGR("data/layers/protected_2012_line.shp")
removed=readOGR("data/layers/removed.shp")
###### 3. UI #####

ui <- shinyUI(fluidPage(theme = shinytheme("cyborg"),
                        fluidRow(
                          column(12,style="background-color:white;",titlePanel(tags$div(h3(style="text-align:center;color:black","Using temporally explicit habitat suitability models to assess threats to mobile species and evaluate the effectiveness of marine protected areas ")))),
                          column(12,style="background-color:white;",titlePanel(tags$div(h4(style="text-align:center;color:black;background-color:white",HTML(paste("Heather Welch",tags$sup(1,2,3,"*"),", ","Robert L. Pressey, ",tags$sup(4),", ","April E. Reside",tags$sup(5))))))),
                          column(12,style="background-color:white;",titlePanel(tags$div(h6(style="text-align:center;color:black;background-color:white",HTML(paste(tags$sup(1),"School of Marine and Tropical Biology, James Cook University, Townsville, QLD 4811, AUS, ",tags$sup(2),"Environmental Research Division, SWFSC, NMFS, NOAA, Monterey, CA 93940, USA, ",tags$sup(3),"Institute of Marine Science, University of California Santa Cruz, Santa Cruz, CA 95064, USA, ",tags$sup(4),"Australian Research Council Centre of Excellence for Coral Reef Studies, James Cook University, 1 James Cook Dr, Townsville, QLD 4811, AUS, ",tags$sup(5),"Centre for Biodiversity and Conservation Science, The University of Queensland, St. Lucia, QLD, AUS, ",tags$sup("*"),"Corresponding author: heather.welch@noaa.gov")))))),
                          column(12,
                          tabsetPanel(type = "tabs",
######## ------------> Tab 1 #######
                          tabPanel("Explore total bycatch threat",
                                   hr(),
                                   fluidRow(#style="background-color:darkgrey;display:center-align;",
                          column(3, checkboxInput("NTA_2012",h6(style="color:white;","Show reserves from the 2012 proclaimed network that prohibit longlining"),value = F,width = "100%")),
                          column(2, actionButton("zoomCS",h6("Zoom to Coral Sea Network"))),
                          column(2, actionButton("zoomTE",h6("Zoom to Temperate East Network")))
                                   ),
                                   fluidRow(#style="background-color:darkgrey;",
                          column(3, checkboxInput("NTA_2015",h6(style="color:white;","Show reserves from the 2015 panel-recommended network that prohibit longlining"),value = F,width="100%")),
                          column(2, actionButton("zoomSE",h6("Zoom to South East Network"))),
                          column(2, actionButton("zoomFL",h6("Zoom to full extent")))
                                   ),
                                   fluidRow(
                          column(3,titlePanel(tags$div(h4(style="text-align:center;","Blue shark")))),
                          column(3,titlePanel(tags$div(h4(style="text-align:center;","Bronze whaler shark")))),
                          column(3,titlePanel(tags$div(h4(style="text-align:center;","Tiger shark")))),
                          column(3,titlePanel(tags$div(h4(style="text-align:center;","Dusky whaler shark"))))
                          ),
                                  fluidRow(
                          column(3,leafletOutput("bs")),
                          column(3,leafletOutput("bws")),
                          column(3,leafletOutput("ts")),
                          column(3,leafletOutput("dws"))
                                  ),
                          fluidRow(
                            column(3,titlePanel(tags$div(h4(style="text-align:center;","Oceanic whitetip shark")))),
                            column(3,titlePanel(tags$div(h4(style="text-align:center;","Silky shark")))),
                            column(3,titlePanel(tags$div(h4(style="text-align:center;","Shortfin mako shark")))),
                            column(3,titlePanel(tags$div(h4(style="text-align:center;","All sharks"))))
                          ),
                          fluidRow(
                            column(3,leafletOutput("ows")),
                            column(3,leafletOutput("ss")),
                            column(3,leafletOutput("sfm")),
                            column(3,leafletOutput("all"))
                          )),

######## ------------> Tab 2 #######
                          tabPanel("Explore monthly habitat suitability",
                                   hr(),
                          fluidRow(#style="background-color:darkgrey;",
                            column(4,style="background-color:black;",
                                  sliderInput("date",h6(style="color:white;","Number of months since January 1998"),1,120,value = 1,sep="",width="100%")),
                            column(3, checkboxInput("aNTA_2012",h6(style="color:white;","Show reserves from the 2012 proclaimed network that prohibit longlining"),value = F,width = "100%")),
                            column(2, actionButton("azoomCS",h6("Zoom to Coral Sea Network"))),
                            column(2, actionButton("azoomTE",h6("Zoom to Temperate East Network")))
                            ),
                          fluidRow(#style="background-color:darkgrey;",
                            column(4,style="background-color:black;",h6(style="color:white;background-color:black;text-align:center;",textOutput("dateOutput"))),
                            column(3, checkboxInput("aNTA_2015",h6(style="color:white;","Show reserves from the 2015 panel-recommended network that prohibit longlining"),value = F,width="100%")),
                            column(2, actionButton("azoomSE",h6("Zoom to South East Network"))),
                            column(2, actionButton("azoomFL",h6("Zoom to full extent")))
                                    ),
                            fluidRow(
                              column(3,titlePanel(tags$div(h4(style="text-align:center;","Blue shark")))),
                              column(3,titlePanel(tags$div(h4(style="text-align:center;","Bronze whaler shark")))),
                              column(3,titlePanel(tags$div(h4(style="text-align:center;","Tiger shark")))),
                              column(3,titlePanel(tags$div(h4(style="text-align:center;","Dusky whaler shark"))))
                            ),
                            fluidRow(
                              column(3,leafletOutput("bs1")),
                              column(3,leafletOutput("bws1")),
                              column(3,leafletOutput("ts1")),
                              column(3,leafletOutput("dws1"))
                            ),
                            fluidRow(
                              column(3,titlePanel(tags$div(h4(style="text-align:center;","Oceanic whitetip shark")))),
                              column(3,titlePanel(tags$div(h4(style="text-align:center;","Silky shark")))),
                              column(3,titlePanel(tags$div(h4(style="text-align:center;","Shortfin mako shark")))),
                              column(3,titlePanel(tags$div(h4(style="text-align:center;","All sharks"))))
                            ),
                            fluidRow(
                              column(3,leafletOutput("ows1")),
                              column(3,leafletOutput("ss1")),
                              column(3,leafletOutput("sfm1")),
                              column(3,leafletOutput("all1")))

                          ),

######## ------------> Tab 3 #######
tabPanel("Monthly habitat suitability gifs",
         hr(),
fluidRow(
  column(3,selectInput("sp","Select species",choices = species)),
  column(12,imageOutput("gif"))
)
 )
                          )))))
             


server <- shinyServer(function(input, output) {
  
  ##### definte extents ####
  ras_ext=list()
  ras_ext$lon=155.8333
  ras_ext$lat=-30.25
  ras_ext$zoom=3

  cs_ext=list()
  cs_ext$lon=151.8333
  cs_ext$lat=-17
  cs_ext$zoom=5
  
  te_ext=list()
  te_ext$lon=147
  te_ext$lat=-42.25
  te_ext$zoom=5
  
  se_ext=list()
  se_ext$lon=161.8333
  se_ext$lat=-30
  se_ext$zoom=5
  
  v <- reactiveValues(data = NULL)
  av <- reactiveValues(data = NULL)
  
  #####tabset 1
  observeEvent(input$zoomCS, {
    #need this null to force a data change and a re-render
    v$data<-NULL
    v$data <- cs_ext
  })
  
  observeEvent(input$zoomTE, {
    #need this null to force a data change and a re-render
    v$data<-NULL
    v$data <- te_ext
  }) 
  
  observeEvent(input$zoomSE, {
    #need this null to force a data change and a re-render
    v$data<-NULL
    v$data <- se_ext
  }) 
  
  observeEvent(input$zoomFL, {
    #need this null to force a data change and a re-render
    v$data<-NULL
    v$data <- ras_ext
  }) 
  
  #####tabset 1
  observeEvent(input$azoomCS, {
    #need this null to force a data change and a re-render
    av$data<-NULL
    av$data <- cs_ext
  })
  
  observeEvent(input$azoomTE, {
    #need this null to force a data change and a re-render
    av$data<-NULL
    av$data <- te_ext
  }) 
  
  observeEvent(input$azoomSE, {
    #need this null to force a data change and a re-render
    av$data<-NULL
    av$data <- se_ext
  }) 
  
  observeEvent(input$azoomFL, {
    #need this null to force a data change and a re-render
    av$data<-NULL
    av$data <- ras_ext
  }) 
  #####
  
  ##### collect global reactive elements ####
  show2012=reactive({input$NTA_2012})
  show2015=reactive({input$NTA_2015})
  ashow2012=reactive({input$aNTA_2012})
  ashow2015=reactive({input$aNTA_2015})
  
  ########## --------------------------------------------------------------- > tabset 1 ####
  output$bs <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- v$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(threat_dir,"total_bs.tif"))
    full[full>20000]=20000
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,full@data@max)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent")
    
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    lmap <- addLegend(lmap, "topright", values = vals,color=pal,labels=c("Low","","","","","","","","","","High"),title = "Total threat")
    lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(show2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(show2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
    })
  })
  
  output$bws <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- v$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(threat_dir,"total_bws.tif"))
    full[full>20000]=20000
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,full@data@max)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent")
    
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Total threat")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(show2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(show2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$ts <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- v$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(threat_dir,"total_ts.tif"))
    full[full>20000]=20000
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,full@data@max)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent")
    
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Total threat")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(show2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(show2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$dws <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- v$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(threat_dir,"total_dws.tif"))
    full[full>20000]=20000
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,full@data@max)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent")
    
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Total threat")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(show2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(show2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$ss <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- v$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(threat_dir,"total_ss.tif"))
    full[full>20000]=20000
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,full@data@max)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent")
    
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Total threat")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(show2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(show2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$sfm <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- v$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(threat_dir,"total_sfm.tif"))
    full[full>20000]=20000
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,full@data@max)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent")
    
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Total threat")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(show2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(show2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$ows <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- v$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(threat_dir,"total_ows.tif"))
    full[full>20000]=20000
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,full@data@max)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent")
    
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Total threat")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(show2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(show2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
    })
  
  output$all <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- v$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(threat_dir,"total_all.tif"))
    full[full>140000]=140000
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,full@data@max)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent")
    
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Total threat")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(show2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(show2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })

  ########## --------------------------------------------------------------- > tabset 2 ####
  ##### collect reactive elements 
  textDate=reactive({yearmon[input$date]})
  output$dateOutput=renderText({paste0("Display date is ",textDate())})
  numDate=reactive({as.character(yearmon_numeric[input$date])})

  pal <- brewer.pal(11,"RdYlBu")
  vals <- NULL
  vals=c(0,1)
  palette2 <- colorNumeric(pal, vals,
                           na.color = "transparent")
  
  output$bs1 <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- av$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(full_models_dir,"bs_",numDate(),".asc"))
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    lmap <- addLegend(lmap, "topright", values = vals,color=pal,labels=c("Low","","","","","","","","","","High"),title = "Habitat suitability")
    lmap <- addLegend(lmap,"topright",colors = "grey",labels="Removed",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(ashow2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(ashow2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$bws1 <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- av$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(full_models_dir,"bws_",numDate(),".asc"))
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "topright", values = vals,pal=palette2,title = "Habitat suitability")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(ashow2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(ashow2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$ts1 <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- av$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(full_models_dir,"ts_",numDate(),".asc"))
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "topright", values = vals,pal=palette2,title = "Habitat suitability")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(ashow2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(ashow2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$dws1 <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- av$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(full_models_dir,"Dws_",numDate(),".asc"))
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "topright", values = vals,pal=palette2,title = "Habitat suitability")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(ashow2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(ashow2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$ss1<- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- av$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(full_models_dir,"ss_",numDate(),".asc"))
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "topright", values = vals,pal=palette2,title = "Habitat suitability")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(ashow2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(ashow2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$sfm1 <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- av$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(full_models_dir,"sfm_",numDate(),".asc"))
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "topright", values = vals,pal=palette2,title = "Habitat suitability")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(ashow2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(ashow2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$ows1 <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- av$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=raster(paste0(full_models_dir,"ows_",numDate(),".asc"))
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "topright", values = vals,pal=palette2,title = "Habitat suitability")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(ashow2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(ashow2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  output$all1 <- renderLeaflet({
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
    target_ext <- av$data
    if(is.null(target_ext)){
      target_ext<-ras_ext
    }
    
    full=list.files(full_models_dir,pattern = numDate(),full.names = T)%>%stack()%>%calc(.,fun=sum)
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,full@data@max)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent")
    
    #full=raster(paste0(full_models_dir,"all_",numDate(),".asc"))
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap=leaflet()
    lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = F))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap <- addLegend(lmap, "topright", values = vals,pal=palette2,title = "Habitat suitability")
    #lmap <- addLegend(lmap,"topright",colors = "grey",labels = "Removed areas",opacity = 1)
    lmap <-addPolygons(lmap,data=removed,color="grey",opacity = 1,fillOpacity = 1)
    if(ashow2012()){
      lmap <-addPolylines(lmap,data=NTA_2012,color="black",weight = 2)
    }
    if(ashow2015()){
      lmap <-addPolylines(lmap,data=NTA_2015,color="blue",weight = 2)
    }
    lmap
  })
  })
  
  ######
  
  ########## --------------------------------------------------------------- > tabset 3
  output$gif=renderImage({
    spp=input$sp
    if(spp=="Blue shark"){
      a="bs"
    }else if(spp=="Bronze whaler shark"){
      a="bws"
    }else if(spp=="Dusky whaler shark"){
      a="Dws"
    }else if(spp=="Oceanic whitetip shark"){
      a="ows"
    }else if(spp=="Shortfin mako shark"){
      a="sfm"
    }else if(spp=="Silky shark"){
      a="ss"
    }else if(spp=="Tiger shark"){
      a="ts"}
    
    list(src=paste0("data/gifs/",a,".gif"),
         contentType = 'image/gif',
         width = 850,
         height = 1000
         # alt = "This is alternate text"
    )},deleteFile = F)

})

shinyApp(ui = ui, server = server)
