library(shiny)
library(Quandl)
library(dygraphs)
library(shinydashboard)

##

ui <- dashboardPage(
  dashboardHeader(title = "Quandl Application"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Economic Indicators", tabName = "indicators", icon=icon("dashboard")),
      menuItem("SEC Filings", tabName = "filings", icon=icon("th")),
      menuItem("Currencies", tabName = "currency", icon=icon("dollar"))
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "indicators",
              selectInput("dataset", "Choose a Dataset", 
                          choices = c("Nominal Potential Gross Domestic Product",
                                      "Natural Rate of Unemployment (Short-Term)",
                                      "Natrual Rate of Unemployment (Long-Term)"),
                          width = '100%'),
              dygraphOutput("nominal")),
      tabItem(tabName = "filings",
              selectInput("secselect", "Choose a Company", 
                          choices = c("Apple - Current Assets",
                                      "Blackhawk Network - Current Assets",
                                      "Intermec, Inc - Current Assets"),
                          width = '100%'),
              dygraphOutput("cassets"),
              selectInput("longselect", "Choose a Company", 
                          choices = c("Apple - NonCurrent Assets",
                                      "Blackhawk Network - NonCurrent Assets",
                                      "Intermec, Inc - NonCurrent Assets"),
                          width = '100%'),
              dygraphOutput("lassets")),
      tabItem(tabName = "currency",
              selectInput("curselect", "Choose an Exchange", 
                          choices = c("JPY vs SEK",
                                      "JPY vs CHF",
                                      "JPY vs CAD"),
                          width = '100%'),
              dygraphOutput("dycurrency"))
    )
  )
)


server <- function(input, output){
  
  Quandl.api_key("mmiYq3yxKc5JvTFa1Uzr")
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Nominal Potential Gross Domestic Product" = Quandl("FRED/NGDPPOT"),
           "Natural Rate of Unemployment (Short-Term)" = Quandl("FRED/NROUST"),
           "Natrual Rate of Unemployment (Long-Term)" = Quandl("FRED/NROU"))
  })
  
  output$nominal <- renderDygraph({
    nominal.potentialGDP <- datasetInput()
    nominal.astime <- nominal.potentialGDP$VALUE
    names(nominal.astime) <- nominal.potentialGDP$DATE
    dygraph(nominal.astime, main=input$dataset)
  })
  
  # sec tab 
  
  secInput <- reactive({
    switch(input$secselect,
           "Apple - Current Assets" = Quandl("SEC/AAPL_ASSETSCURRENT_A"),
           "Blackhawk Network - Current Assets" = Quandl("SEC/HAWK_ASSETSCURRENT_A"),
           "Intermec, Inc - Current Assets" = Quandl("SEC/IN_ASSETSCURRENT_A"))
  })
  
  output$cassets <- renderDygraph({
    assets <- secInput()
    assets.astime <- assets$Value
    names(assets.astime) <- assets$Date
    dygraph(assets.astime, main=input$secselect)
  })
  
  noncurrentInput <- reactive({
    switch(input$longselect,
           "Apple - NonCurrent Assets" = Quandl("SEC/AAPL_OTHERASSETSNONCURRENT_A"),
           "Blackhawk Network - NonCurrent Assets" = Quandl("SEC/HAWK_OTHERASSETSNONCURRENT_A"),
           "Intermec, Inc - NonCurrent Assets" = Quandl("SEC/IN_OTHERASSETSNONCURRENT_A"))
  })
  
  output$lassets <- renderDygraph({
    nassets <- noncurrentInput()
    nassets.astime <- nassets$Value
    names(nassets.astime) <- nassets$Date
    dygraph(nassets.astime, main=input$longselect)
  })
  
  #currency tab
  
  curInput <- reactive({
    switch(input$curselect,
           "JPY vs SEK" = Quandl("CURRFX/JPYSEK"),
           "JPY vs CHF" = Quandl("CURRFX/JPYCHF"),
           "JPY vs CAD" = Quandl("CURRFX/JPYCAD"))
  })
  
  output$dycurrency <- renderDygraph({
    cur <- curInput()
    cur.astime <- cur$Rate
    names(cur.astime) <- cur$Date
    dygraph(cur.astime, main=input$curselect)
  })
  
  
}

shinyApp(ui, server)







