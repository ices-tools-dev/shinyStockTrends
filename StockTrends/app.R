rm(list = ls())
library(dplyr)
library(shiny)


# source("~/git/ices-dk/shinyStockTrends/StockTrends/helpers.R")
source("helpers.R")

ui <- fluidPage(
  titlePanel("Trends in stock status"),
  sidebarLayout(
    sidebarPanel(
      
      h5("1. Click the ICES logo to query data from ICES databases"),
      actionButton(inputId = "queryICES",
                   label = img(src = "ICES_LOGO.png",
                               width = "100px")
      ),
      h6("Be patient, this might take a minute..."),
      h6(textOutput(outputId = "nrows")),
      tags$hr(),
      
      h5( "Select the data you want to explore:"),
      radioButtons(inputId = "dataType", 
                   label = "",
                   list("Fish stocks by guild and ecoregion" = "stoGldEco",
                        "Fisheries guilds by ecoregion" = "gldEco"),
                        # "Fish stocks by name" = "stockName"),
                   selected = "stoGldEco"),
      
      # Fish stocks by guild and ecoregion
      conditionalPanel(
        condition = "input.dataType == 'stoGldEco'",
        h5("Choose ecoregion(s)"),
        actionButton(inputId = "selectEco_stoGldEco",
                     label = "Select all ecoregions"
        ),
        checkboxGroupInput(
          inputId = "ecoregion_stoGldEco",
          label = "",
          choices = ""
        ),
        h5("Choose fishery guild(s)"),
        actionButton(inputId = "selectGuild_stoGldEco",
                     label = "Select all guilds"
        ),
        checkboxGroupInput(
          inputId = "guild_stoGldEco",
          label = "",
          choices = ""
        ),
        h5("Choose fishery stocks(s)"),
        actionButton(inputId = "selectStock_stoGldEco",
                     label = "Select all stocks"
        ),
        checkboxGroupInput(
          inputId = "stock.code_stoGldEco",
          label = "",
          choices = ""
        )
      ),
      
      # Fish stocks by guild and ecoregion
      conditionalPanel(
        condition = "input.dataType == 'gldEco'",
        
        h5("Choose ecoregion(s)"),
        actionButton(inputId = "selectEco_gldEco",
                     label = "Select all ecoregions"
        ),
        checkboxGroupInput(
          inputId = "ecoregion_gldEco",
          label = "",
          choices = ""
        ),
        h5("Choose fishery guild(s)"),
        actionButton(inputId = "selectGuild_gldEco",
                     label = "Select all guilds"
        ),
        checkboxGroupInput(
          inputId = "guild_gldEco",
          label = "",
          choices = ""
        )
      )
      # ,
      #   
      # # Fish stocks by stock code
      # conditionalPanel(
      #   condition = "input.dataType == 'stockName'",
      #   h5("Choose stock(s)"),
      #   textInput(inputId = "stockByName",
      #             label = "Enter fish stock code(s)",
      #             value = "cod-347d",
      #             multiple = TRUE)
      # )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("linePlot", width = "80%", height = "500px"),
                 textInput("textPlot", 
                           label = h5("If you want, type in a new file name:"),
                           value = "stockTrendsPlot"),
                 downloadButton('downloadPlot', 'Download plot (.png)')
        ),
        tabPanel("Data", 
                 dataTableOutput('dataTable'),
                 textInput("textData", 
                           label = h5("If you want, type in a new file name:"),
                           value = "stockTrendsData"),
                 downloadButton('downloadData',
                                'Download data (.csv)')
        )
      )
    )
  )
)



server = function(input, output, session){
  

  ######################
  ### Input handling ###
  ######################
  
  # When the "query ICES databases" button is pushed, run the queryICES() function
  stockTrends <- eventReactive(input$queryICES, {
    # queryICES()
  
      # stockTrends <- 
    readRDS("data/tester.rds")
    })
  
    # dat <- reactive({
  #   req(input$queryICES)
  # 
  #   dat <- switch(input$dataType,
  #                 "Fish stocks by guild and ecoregion" = "stoGldEco",
  #                 "Fisheries guilds by ecoregion" = "gldEco",
  #                 "Fish stocks by name" = "stockName")
  #   # rawData %>%
  # 
  # })
  
  
  # When the ICES databases are called, update ecoregions based on available data
  observe({
    req(input$queryICES,
        input$dataType)
    
    if(req(input$dataType) == "stoGldEco") {
      updateCheckboxGroupInput(session, 
                               "ecoregion_stoGldEco",
                               choices = sort(unique(stockTrends()$ECOREGION)))
      if(req(input$selectEco_stoGldEco)) {
        if(req(input$selectEco_stoGldEco) %% 2 == 0) {
          updateCheckboxGroupInput(session = session,
                                   inputId = "ecoregion_stoGldEco",
                                   choices = sort(unique(stockTrends()$ECOREGION)),
                                   selected = c())
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "ecoregion_stoGldEco",
                                   choices = sort(unique(stockTrends()$ECOREGION)),
                                   selected = c(sort(unique(stockTrends()$ECOREGION))))
        }
      }
    }
    if(req(input$dataType) == "gldEco") {
      updateCheckboxGroupInput(session, 
                               "ecoregion_gldEco",
                               choices = sort(unique(stockTrends()$ECOREGION)))
      if(req(input$selectEco_gldEco)) {
        if(req(input$selectEco_gldEco) %% 2 == 0) {
          updateCheckboxGroupInput(session = session,
                                   inputId = "ecoregion_gldEco",
                                   choices = sort(unique(stockTrends()$ECOREGION)),
                                   selected = c())
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "ecoregion_gldEco",
                                   choices = sort(unique(stockTrends()$ECOREGION)),
                                   selected = c(sort(unique(stockTrends()$ECOREGION))))
        }
      }
    }
  })
  
  # When ecoregions are selected display the available guilds
  observe({
    req(input$queryICES,
        input$dataType)
    
    if(req(input$dataType) == "stoGldEco") {
      req(input$ecoregion_stoGldEco)
      
      updateCheckboxGroupInput(session, 
                               "guild_stoGldEco",
                               choices = sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion_stoGldEco)])))
      if(req(input$selectGuild_stoGldEco)) {
        if(req(input$selectGuild_stoGldEco) %% 2 == 0) {
          updateCheckboxGroupInput(session = session,
                                   inputId = "guild_stoGldEco",
                                   choices = sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion_stoGldEco)])),
                                   selected = c())
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "guild_stoGldEco",
                                   choices = sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion_stoGldEco)])),
                                   selected = c(sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion_stoGldEco)]))))
        }
      }
    }
    
    if(req(input$dataType) == "gldEco") {
      req(input$ecoregion_gldEco)
      
      updateCheckboxGroupInput(session, 
                               "guild_gldEco",
                               choices = sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion_gldEco)])))
      if(req(input$selectGuild_gldEco)) {
        if(req(input$selectGuild_gldEco) %% 2 == 0) {
          updateCheckboxGroupInput(session = session,
                                   inputId = "guild_gldEco",
                                   choices = sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion_gldEco)])),
                                   selected = c())
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "guild_gldEco",
                                   choices = sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion_gldEco)])),
                                   selected = c(sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion_gldEco)]))))
        }
      }
    }
  })
  
  # When ecoregion and guilds are selected display the available stocks
  observe({
    req(input$queryICES,
        input$dataType,
        input$ecoregion_stoGldEco,
        input$guild_stoGldEco)
    
    if(req(input$dataType) == "stoGldEco") {
      updateCheckboxGroupInput(session, 
                               "stock.code_stoGldEco",
                               choices = sort(unique(stockTrends()$STOCK.CODE[which(stockTrends()$ECOREGION %in% input$ecoregion_stoGldEco &
                                                                                    stockTrends()$FISHERIES.GUILD %in% input$guild_stoGldEco)])))
      if(req(input$selectStock_stoGldEco)) {
        if(req(input$selectStock_stoGldEco) %% 2 == 0) {
          updateCheckboxGroupInput(session = session,
                                   inputId = "stock.code_stoGldEco",
                                   choices = sort(unique(stockTrends()$STOCK.CODE[which(stockTrends()$ECOREGION %in% input$ecoregion_stoGldEco &
                                                                                        stockTrends()$FISHERIES.GUILD %in% input$guild_stoGldEco)])),
                                   selected = c())
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "stock.code_stoGldEco",
                                   choices = sort(unique(stockTrends()$STOCK.CODE[which(stockTrends()$ECOREGION %in% input$ecoregion_stoGldEco &
                                                                                        stockTrends()$FISHERIES.GUILD %in% input$guild_stoGldEco)])),
                                   selected = c(sort(unique(stockTrends()$STOCK.CODE[which(stockTrends()$ECOREGION %in% input$ecoregion_stoGldEco &
                                                                                           stockTrends()$FISHERIES.GUILD %in% input$guild_stoGldEco)]))))
        }
      }
    } 
  })
  
  
  
  # 
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
      need(input$dataType, "Please select how you want the data")
    )
    
    if(req(input$dataType) == "stoGldEco") {
      
      validate(    
        need(input$ecoregion_stoGldEco, "Please select ecoregion"),
        need(input$guild_stoGldEco, "Please select guild"),
        need(input$stock.code_stoGldEco, "Please select stock")
      )
      
      dat <- stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion_stoGldEco,
               FISHERIES.GUILD %in% input$guild_stoGldEco,
               STOCK.CODE %in% input$stock.code_stoGldEco,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
      
      validate(
        need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
      )
      
      stockSummaryTrends(dat = dat, LINEGROUP = "STOCK.CODE", overallMean = TRUE, legend.cex = 1)
    }
    
    if(req(input$dataType) == "gldEco") {
      
      validate(    
        need(input$ecoregion_gldEco, "Please select ecoregion"),
        need(input$guild_gldEco, "Please select guild")
      )
      
      dat <- stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion_gldEco,
               FISHERIES.GUILD %in% input$guild_gldEco,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
        select(-STOCK.CODE) %>%
        group_by(Year, FISHERIES.GUILD, METRIC) %>%
        summarize(stockValue = mean(stockValue, na.rm = TRUE))
      
      validate(
        need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
      )
      
      stockSummaryTrends(dat = dat, LINEGROUP = "FISHERIES.GUILD", overallMean = TRUE, legend.cex = 1)
    }
  })
  
  # Print user-defined file name
  output$valuePlot <- renderPrint({ input$textPlot })
  output$valueData <- renderPrint({ input$textData })
  
  # Print the data table
  output$dataTable <- renderDataTable({
    
    validate(
      need(input$queryICES, "Please click the ICES logo to download data"),
      need(input$dataType, "Please select how you want the data")
    )
    
    if(req(input$dataType) == "stoGldEco") {
      
      validate(    
        need(input$ecoregion_stoGldEco, "Please select ecoregion"),
        need(input$guild_stoGldEco, "Please select guild"),
        need(input$stock.code_stoGldEco, "Please select stock")
      )
      
      dat <- stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion_stoGldEco,
               FISHERIES.GUILD %in% input$guild_stoGldEco,
               STOCK.CODE %in% input$stock.code_stoGldEco,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
      
      validate(
        need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
      )
      return(dat)
    }
    
    if(req(input$dataType) == "gldEco") {
      
      validate(    
        need(input$ecoregion_gldEco, "Please select ecoregion"),
        need(input$guild_gldEco, "Please select guild")
      )
      
      dat <- stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion_gldEco,
               FISHERIES.GUILD %in% input$guild_gldEco,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
        select(-STOCK.CODE) %>%
        group_by(Year, FISHERIES.GUILD, METRIC) %>%
        summarize(stockValue = mean(stockValue, na.rm = TRUE))
      
      validate(
        need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
      )
      return(dat)
    }
  })
  
  # The download data button
  output$downloadData <- downloadHandler(
    filename = function() {paste0(input$textData, ".csv")},
    content = function(file) {
   
      validate(
        need(input$queryICES, "Please click the ICES logo to download data"),
        need(input$dataType, "Please select how you want the data")
      )
      
      if(req(input$dataType) == "stoGldEco") {
        
        validate(    
          need(input$ecoregion_stoGldEco, "Please select ecoregion"),
          need(input$guild_stoGldEco, "Please select guild"),
          need(input$stock.code_stoGldEco, "Please select stock")
        )
        
        dat <- stockTrends() %>%
          filter(ECOREGION %in% input$ecoregion_stoGldEco,
                 FISHERIES.GUILD %in% input$guild_stoGldEco,
                 STOCK.CODE %in% input$stock.code_stoGldEco,
                 METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
        
        validate(
          need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
        )
        
        write.csv(x = dat, file = file, row.names = FALSE)
      }
      
      if(req(input$dataType) == "gldEco") {
        
        validate(    
          need(input$ecoregion_gldEco, "Please select ecoregion"),
          need(input$guild_gldEco, "Please select guild")
        )
        
        dat <- stockTrends() %>%
          filter(ECOREGION %in% input$ecoregion_gldEco,
                 FISHERIES.GUILD %in% input$guild_gldEco,
                 METRIC %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
          select(-STOCK.CODE) %>%
          group_by(Year, FISHERIES.GUILD, METRIC) %>%
          summarize(stockValue = mean(stockValue, na.rm = TRUE))
        
        validate(
          need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
        )
        
        write.csv(x = dat, file = file, row.names = FALSE)
      }
    })
  
  # The download plot button
  output$downloadPlot <- downloadHandler(
    filename = function() {paste0(input$textPlot, ".png")},
    content = function(file) {
      
      validate(
        need(input$queryICES, "Please click the ICES logo to download data"),
        need(input$dataType, "Please select how you want the data")
      )
      
      if(req(input$dataType) == "stoGldEco") {
        
        validate(    
          need(input$ecoregion_stoGldEco, "Please select ecoregion"),
          need(input$guild_stoGldEco, "Please select guild"),
          need(input$stock.code_stoGldEco, "Please select stock")
        )
        
        dat <- stockTrends() %>%
          filter(ECOREGION %in% input$ecoregion_stoGldEco,
                 FISHERIES.GUILD %in% input$guild_stoGldEco,
                 STOCK.CODE %in% input$stock.code_stoGldEco,
                 METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
        
        validate(
          need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
        )
        
        png(file,
            width = 89,
            height = 50.25 * 2,
            units = "mm",
            res = 300)
        
        stockSummaryTrends(dat = dat, LINEGROUP = "STOCK.CODE", overallMean = TRUE, legend.cex = 0.5)
        dev.off()
        
      }
      
      if(req(input$dataType) == "gldEco") {
        
        validate(    
          need(input$ecoregion_gldEco, "Please select ecoregion"),
          need(input$guild_gldEco, "Please select guild")
        )
        
        dat <- stockTrends() %>%
          filter(ECOREGION %in% input$ecoregion_gldEco,
                 FISHERIES.GUILD %in% input$guild_gldEco,
                 METRIC %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
          select(-STOCK.CODE) %>%
          group_by(Year, FISHERIES.GUILD, METRIC) %>%
          summarize(stockValue = mean(stockValue, na.rm = TRUE))
        
        validate(
          need(any(!is.na(dat$stockValue)), "This stock does not have necessary data")
        )  
        
        png(file,
            width = 89,
            height = 50.25 * 2,
            units = "mm",
            res = 300)
        
        stockSummaryTrends(dat = dat, LINEGROUP = "FISHERIES.GUILD", overallMean = TRUE, legend.cex = 0.5)
        dev.off()
      }
    }
  )} 



# Run the application 
shinyApp(ui = ui, server = server)

