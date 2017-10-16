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

###### 2. define global objects #####
full_models_dir="data/SDMs/"
species=c("Blue shark","Bronze whaler shark","Dusky whaler shark","Oceanic whitetip shark","Shortfin mako shark","Silky shark", "Tiger shark")
years=as.character(seq(1998,2007))
months=c("January","February","March","April","May","June","July","August","September","October","November","December")


###### 3. UI #####

ui <- shinyUI(fluidPage(theme = shinytheme("cyborg"),
                        fluidRow(
                          column(12, absolutePanel(top = 15, left = 14, height=120, width=1714, fixed=TRUE,
                                                   style = "text-align:center;border-radius: 3px; padding: 0px; border: 1px solid; background: white; ",
                                                   titlePanel(tags$div(h3(style="text-align:center;color:black","Using temporally explicit habitat suitability models to assess threats to mobile species and evaluate the effectiveness of marine protected areas "))))),
                          column(12,absolutePanel(top = 125, left = 14, height=50, width=1714, fixed=TRUE,
                                                  style = "text-align:center;border-radius: 3px; padding: 0px; opacity: 0.92;", 
                                                  titlePanel(tags$div(h4(HTML(paste("Heather Welch",tags$sup(1,"*"),", ","Robert L. Pressey, ",tags$sup(2,3,"*","°"),", ","April E. Reside",tags$sup(4)))))))),
                          column(12, absolutePanel(top = 180, left = 650, height=131, width=200, fixed=TRUE,
                                                  style = "text-align:center;border-radius: 0px; padding: 0px; opacity: 0.92;border: 1px solid; background: black; ", 
                                                  selectInput("years",tags$div(h6(tags$b(style="padding:0px;","Select year"))),years,width = 400,selectize = F,size=4)),
                                 absolutePanel(top = 180, left = 870, height=131, width=200, fixed=TRUE,
                                               style = "text-align:center;border-radius: 0px; padding: 0px; opacity: 0.92;border: 1px solid; background: black; ", 
                                               selectInput("months",tags$div(h6(tags$b(style="padding:0px;","Select month"))),months,width = 400,selectize = F,size=4))
                                 
                                 )),
                        tabsetPanel(tabPanel(
                        fluidRow(absolutePanel(top = 300, left = 14, height=600, width=1714, fixed=TRUE,
                                      style = "text-align:center;border-radius: 3px; padding: 0px; opacity: 0.92;", 
                          column(3,#titlePanel(tags$div(h4(style="text-align:center;","bs"))),
                                      leafletOutput("bs",width=400,height = 500),
                                 absolutePanel(top=300,left=200,height=30,width=200,fixed=TRUE),
                                               h3(style="color:black;","Blue shark"))),
                          column(3, #titlePanel(tags$div(h4(style="text-align:center;","bws"))),
                                      leafletOutput("bws")),
                          column(3,#titlePanel(tags$div(h4(style="text-align:center;","ts"))),
                                      leafletOutput("ts")),
                          column(3,#titlePanel(tags$div(h4(style="text-align:center;","dws"))),
                                      leafletOutput("dws")))),
                        # fluidRow(absolutePanel(top = 300, left = 14, height=100, width=1714, fixed=TRUE,
                        #                        style = "text-align:center;border-radius: 3px; padding: 0px; opacity: 0.92; background=white;",
                        #                        column(3,titlePanel(tags$div(h4(style="text-align:center;color=white;","bs")))),
                        #                               #leafletOutput("bs",width=400,height = 500)),
                        #                        column(3, titlePanel(tags$div(h4(style="text-align:center;","bws")))),
                        #                               #leafletOutput("bws")),
                        #                        column(3,titlePanel(tags$div(h4(style="text-align:center;","ts")))),
                        #                               leafletOutput("ts")),
                        #                        column(3,titlePanel(tags$div(h4(style="text-align:center;","dws"))))),
                        #                               #leafletOutput("dws")))),
                        fluidRow(absolutePanel(top = 780, left = 14, height=600, width=1714, fixed=TRUE,
                                      style = "text-align:center;border-radius: 3px; padding: 0px; opacity: 0.92;",
                          column(3,titlePanel(tags$div(h4(style="text-align:center;","ows"))),
                                      leafletOutput("ows")),
                          column(3,titlePanel(tags$div(h4(style="text-align:center;","sfm"))),
                                     leafletOutput("sfm")),
                          column(3,titlePanel(tags$div(h4(style="text-align:center;","ss"))),
                                     leafletOutput("ss"))))
              
)))


server <- shinyServer(function(input, output) {

  pal <- brewer.pal(11,"RdYlBu")
  vals <- NULL
  vals=c(0,1)
  palette2 <- colorNumeric(pal, vals,
                           na.color = "transparent")
  ras_ext=list()
  ras_ext$lon=155.8333
  ras_ext$lat=-30.25
  ras_ext$zoom=3.6


  year=reactive({as.character(input$years)})




month=reactive({if (input$months == "January"){
  return("01")
}else if(input$months=="February"){
  return("02")
}else if(input$months=="March"){
  return("03")
}else if(input$months=="April"){
  return("04")
}else if(input$months=="May"){
  return("05")
}else if(input$months=="June"){
  return("06")
}else if(input$months=="July"){
  return("07")
}else if(input$months=="August"){
  return("08")
}else if(input$months=="September"){
  return("09")
}else if(input$months=="October"){
  return("10")
}else if(input$months=="November"){
  return("11")
}else if(input$months=="December"){
  return("12")
}
})
######

  # date=reactive({paste0(as.character(year()),as.character(month()))})
  
  output$bs <- renderLeaflet({
    year=input$years
    month=input$months
    full=raster(paste0(full_models_dir,"bs_",year(),month(),".asc"))
    crs(full)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    lmap <- leaflet(options=leafletOptions(title="Blue shark"))
    lmap <-setView(lmap,ras_ext$lon,ras_ext$lat,zoom=4)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
    #lmap<-fitBounds(lmap,lng1=138,lat1=-50,lng2=170,lat2=-10)
    lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Habitat suitability")
  })
  
  # output$bws <- renderLeaflet({
  #   full=raster(paste0(full_models_dir,"bws_",year,month,".asc"))
  #   lmap <- leaflet()
  #   lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
  #   lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
  #   lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Species habitat suitability")
  # })
  # 
  # output$dws <- renderLeaflet({
  #   sp=input$dws
  #   full=raster(paste0(full_models_dir,sp,"_",year,month,".asc"))
  #   lmap <- leaflet()
  #   lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
  #   lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
  #   lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Species habitat suitability")
  # })
  # 
  # output$ows <- renderLeaflet({
  #   sp=input$ows
  #   full=raster(paste0(full_models_dir,sp,"_",year,month,".asc"))
  #   lmap <- leaflet()
  #   lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
  #   lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
  #   lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Species habitat suitability")
  # })
  # 
  # output$sfm <- renderLeaflet({
  #   sp=input$sfm
  #   full=raster(paste0(full_models_dir,sp,"_",year,month,".asc"))
  #   lmap <- leaflet()
  #   lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
  #   lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
  #   lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Species habitat suitability")
  # })
  # 
  # output$ss <- renderLeaflet({
  #   sp=input$ss
  #   full=raster(paste0(full_models_dir,sp,"_",year,month,".asc"))
  #   lmap <- leaflet()
  #   lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
  #   lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
  #   lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Species habitat suitability")
  # })
  # 
  # output$ts <- renderLeaflet({
  #   sp=input$ts
  #   full=raster(paste0(full_models_dir,sp,"_",year,month,".asc"))
  #   lmap <- leaflet()
  #   lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
  #   lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
  #   lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Species habitat suitability")
  # })
  
  
})

shinyApp(ui = ui, server = server)
