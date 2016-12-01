rm(list = ls())
library(dplyr)
library(shiny)


# source("~/git/ices-dk/shinyStockTrends/StockTrends/helpers.R")
source("helpers.R")

ui <- fluidPage(
  titlePanel("Trends in stock status"),
  sidebarLayout(
    sidebarPanel(
      
      h5("1. Click the icon to query data from ICES databases"),
      actionButton(inputId = "queryICES",
                   label = img(src = "http://www.ices.dk/SiteCollectionImages/ICES%20logos/ICES%20logo%20acronym%20PNG%20format.png",
                               width = "100px")
      ),
      h6("Be patient, this might take a minute..."),
      h6(textOutput(outputId = "nrows")),
      tags$hr(),
      
      h5("2. Choose ecoregion(s)"),
      actionButton(inputId = "selectEco",
                   label = "Select all ecoregions"
      ),
      checkboxGroupInput(
        inputId = "ecoregion",
        label = "",
        choices = ""
      ),
      
      h5("3. Choose fishery guild(s)"),
      actionButton(inputId = "selectGuild",
                   label = "Select all guilds"
      ),
      checkboxGroupInput(
        inputId = "guild",
        label = "",
        choices = ""
      ),
      
      h5("4. Choose stock(s)"),
      actionButton(inputId = "selectStock",
                   label = "Select all stocks"
      ),
      checkboxGroupInput(
        inputId = "stock.code",
        label = "",
        choices = ""
      )
    ),
    mainPanel(
      plotOutput("linePlot", width = "80%", height = "500px"),
      hr(),
      textInput("text", label = h5("5. If you want, type in a new file name:"), value = "stockTrends"),
      downloadButton('downloadPlot', 'Download plot (.png)'),
      downloadButton('downloadData', 'Download data (.csv)'),
      h5("6. Explore the data"),
      dataTableOutput('dataTable')
    )
  )
)


server = function(input, output, session){
  
  ######################
  ### Input handling ###
  ######################
  
  # When the "query ICES databases" button is pushed, run the queryICES() function
  stockTrends <- eventReactive(input$queryICES, {
    queryICES()
    # readRDS("~/git/ices-dk/shinyStockTrends/tester.rds")
    })
  
  # When the ICES databases are called, update ecoregions based on available data
  outVarEco = reactive({
    req(input$queryICES)
    sort(unique(stockTrends()$ECOREGION))
  })
  
  observeEvent(input$queryICES, {
    updateCheckboxGroupInput(session, "ecoregion", choices = outVarEco())
  })

  # When select all button is toggled, select all available ecoregions
  observe({
    req(input$queryICES)
    if(input$selectEco > 0) {
      if(input$selectEco %% 2 == 0){
        updateCheckboxGroupInput(session = session,
                                 inputId = "ecoregion",
                                 choices = outVarEco(),
                                 selected = c())
        
      } else {
        updateCheckboxGroupInput(session = session,
                                 inputId = "ecoregion",
                                 choices = outVarEco(),
                                 selected = c(outVarEco()))
      }
    }
  })
  
  # When ecoregions are selected display the available guilds
  outVarGuild = reactive({
    req(input$queryICES)
    sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion)]))
  })
  observeEvent(input$ecoregion, {
    updateCheckboxGroupInput(session, "guild", choices = outVarGuild())
  })
  # When select all button is toggled, select all available guilds
  observe({
    req(input$queryICES)
      if(input$selectGuild > 0) {
        if(input$selectGuild %% 2 == 0){
          updateCheckboxGroupInput(session = session,
                                   inputId = "guild",
                                   choices = outVarGuild(),
                                   selected = c())
          
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "guild",
                                   choices = outVarGuild(),
                                   selected = c(outVarGuild()))
        }
    }
  })
  
  # When ecoregion and guilds are selected display the available stocks
  outVarStock = reactive({
    req(input$queryICES, input$ecoregion, input$guild)

    sort(unique(stockTrends()$STOCK.CODE[which(stockTrends()$ECOREGION %in% input$ecoregion &
                                               stockTrends()$FISHERIES.GUILD %in% input$guild)]))
  })
  observeEvent(input$guild, {
    updateCheckboxGroupInput(session, "stock.code", choices = outVarStock())
  })
  
  # When select all button is toggled, select all available stocks
  observe({
    req(input$queryICES)
    if(input$selectStock > 0) {
      if(input$selectStock %% 2 == 0){
        updateCheckboxGroupInput(session = session,
                                 inputId = "stock.code",
                                 choices = outVarStock(),
                                 selected = c())
        
      } else {
        updateCheckboxGroupInput(session = session,
                                 inputId = "stock.code",
                                 choices = outVarStock(),
                                 selected = c(outVarStock()))
      }
    }
  })
  
  ###############
  ### OUTPUTS ###
  ###############
  
  # Print the number of rows downloaded
  output$nrows <- renderText({
    paste0("Success! ", scales::comma(nrow(stockTrends())), " rows of data are loaded.")
  })
  
  # Print the line plot of stock trends
  output$linePlot <- renderPlot({
    validate(
      need(input$queryICES, "Please click the ICES logo to download data"),
      need(input$ecoregion, "Please select ecoregion"),
      need(input$guild, "Please select guild"),
      need(input$stock.code, "Please select stock")
    )
    
    dat <- stockTrends() %>%
      filter(ECOREGION %in% input$ecoregion,
             FISHERIES.GUILD %in% input$guild,
             STOCK.CODE %in% input$stock.code,
             METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
    
    validate(
      need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
    )
    stockSummaryTrends(dat = dat, overallMean = TRUE, legend.cex = 1)
  })
  
  # Print user-defined file name
  output$value <- renderPrint({ input$text })
  
  # Print the data table
  output$dataTable <- renderDataTable({
    validate(
      need(input$queryICES, "Please click the ICES logo to download data"),
      need(input$ecoregion, "Please select ecoregion"),
      need(input$guild, "Please select guild"),
      need(input$stock.code, "Please select stock")
    )
    
    dat <- stockTrends() %>%
      filter(ECOREGION %in% input$ecoregion,
             FISHERIES.GUILD %in% input$guild,
             STOCK.CODE %in% input$stock.code,
             METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
    
    validate(
      need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
    )
    dat
  })
  
  # The download data button
  output$downloadData <- downloadHandler(
    filename = function() {paste0(input$text, ".csv")},
    content = function(file) {
      validate(
        need(input$queryICES, "Please click the ICES logo to download data"),
        need(input$ecoregion, "Please select ecoregion"),
        need(input$guild, "Please select guild"),
        need(input$stock.code, "Please select stock")
      )
      
      dat <- stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion,
               FISHERIES.GUILD %in% input$guild,
               STOCK.CODE %in% input$stock.code,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
      validate(
        need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
      )
      write.csv(x = dat, file = file, row.names = FALSE)
    })
  
  # The download plot button
  output$downloadPlot <- downloadHandler(
    filename = function() {paste0(input$text, ".png")},
    content = function(file) {
      validate(
        need(input$queryICES, "Please click the ICES logo to download data"),
        need(input$ecoregion, "Please select ecoregion"),
        need(input$guild, "Please select guild"),
        need(input$stock.code, "Please select stock")
      )
      png(file,
          width = 89,
          height = 50.25 * 2,
          units = "mm",
          res = 300)
      dat <- stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion,
               FISHERIES.GUILD %in% input$guild,
               STOCK.CODE %in% input$stock.code,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
      validate(
        need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
      )
      stockSummaryTrends(dat = dat, overallMean = TRUE, legend.cex = 0.5)
      dev.off()
    }
  )} 

# Run the application 
shinyApp(ui = ui, server = server)

