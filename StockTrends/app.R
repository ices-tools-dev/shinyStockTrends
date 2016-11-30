rm(list = ls())


# source("~/git/ices-dk/shinyStockTrends/StockTrends/helpers.R")
source("helpers.R")

ui <- fluidPage(
  # img(src = "http://www.ices.dk/SiteCollectionImages/ICES%20logos/ICES-logo%20full%20text%20.PNG%20format.png",
  #     width = "300px"),
  titlePanel("Trends in stock status"),
  sidebarLayout(
    sidebarPanel(
      # br(),
      
      h5("1. Click the icon to query data from ICES databases"),
      actionButton(inputId = "queryICES",
                   label = img(src = "http://www.ices.dk/SiteCollectionImages/ICES%20logos/ICES%20logo%20acronym%20PNG%20format.png",
                       width = "100px")
      ),
      h6("Be patient, this might take a minute..."),
      h6(textOutput(outputId = "nrows")),
      tags$hr(),
      #
      h5("2. Choose ecoregion(s)"),
      actionButton(inputId = "selectEco",
                   label = "Select all ecoregions"
      ),
      checkboxGroupInput(
        inputId = "ecoregion",
        label = "",
        # label = h5("Choose ecoregion(s)"),
        choices = ""
      ),
      #
      h5("3. Choose fishery guild(s)"),
      actionButton(inputId = "selectGuild",
                   label = "Select all guilds"
      ),
      checkboxGroupInput(
        inputId = "guild",
        label = "",
        # label = h5("Choose guild(s)"),
        choices = ""
      ),
      #
      # tags$br(),
      h5("4. Choose stock(s)"),
      actionButton(inputId = "selectStock",
                   label = "Select all stocks"
      ),
      checkboxGroupInput(
        inputId = "stock.code",
        label = "",
        # label = h5("Choose stock(s)"),
        choices = ""
      )
    ),
    mainPanel(
      # verbatimTextOutput("value")
      plotOutput("linePlot", width = "80%", height = "500px"),
      hr(),
      br(),
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
  
    # When the "query ICES databases button is pushed, run the queryICES() function
  stockTrends <- eventReactive(input$queryICES, {
    queryICES()
  })
  
  # When the ICES databases are queried, update ecoregions based on available data
  outVarEco = reactive({
    if(input$queryICES == 0) {
      return(NULL)
    }
    sort(unique(stockTrends()$ECOREGION))
  })
  
  observeEvent(input$queryICES, {
    updateCheckboxGroupInput(session, "ecoregion", choices = outVarEco())
  })
  
  # When select all button is toggled, select all available ecoregions
  observe({
    if(input$queryICES == 0) {
      return(NULL)
    } else {
      if (input$selectEco > 0) {
        if (input$selectEco %% 2 == 0){
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
    }
  })
  
  
  # When ecoregions are selected display the available guilds
  outVarGuild = reactive({
    if(input$queryICES == 0) {
      return(NULL)
    }
      sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion)]))
  })
  observeEvent(input$ecoregion, {
    updateCheckboxGroupInput(session, "guild", choices = outVarGuild())
  })
  # When select all button is toggled, select all available guilds
  observe({
    if(input$queryICES == 0) {
      return(NULL)
    } else {
      if (input$selectGuild > 0) {
        if (input$selectGuild %% 2 == 0){
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
    }
  })
  
  

  # When ecoregion and guilds are selected display the available stocks
  outVarStock = reactive({
    if(input$queryICES == 0) {
      return(NULL)
    }
      if(input$ecoregion == "") return(NULL)
      sort(unique(stockTrends()$STOCK.CODE[which(stockTrends()$ECOREGION %in% input$ecoregion &
                                                 stockTrends()$FISHERIES.GUILD %in% input$guild)]))
  })
  observeEvent(input$guild, {
  updateCheckboxGroupInput(session, "stock.code", choices = outVarStock())
  })
  # When select all button is toggled, select all available stocks
  observe({
    if(input$queryICES == 0) {
      return(NULL)
    } else {
      if (input$selectStock > 0) {
        if (input$selectStock %% 2 == 0){
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
    if(is.null(input$queryICES) ||
       is.null(input$ecoregion) ||
       is.null(input$guild) ||
       is.null(input$stock.code)) {
      return(NULL)
    } else {
      
      dat <- stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion,
               FISHERIES.GUILD %in% input$guild,
               STOCK.CODE %in% input$stock.code,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
      
      if(all(is.na(dat$stockValue))) {
        return(NULL)
      } else {
        stockSummaryTrends(dat = dat, overallMean = TRUE, legend.cex = 1)
      }
    }
  }
  )
  
  # Print user-defined file name
  output$value <- renderPrint({ input$text })
  
  # Print the data table
  output$dataTable <- renderDataTable({
    if(is.null(input$queryICES) ||
       is.null(input$ecoregion) ||
       is.null(input$guild) ||
       is.null(input$stock.code)) {
      return(NULL)
    } else {
      
      dat <- stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion,
               FISHERIES.GUILD %in% input$guild,
               STOCK.CODE %in% input$stock.code,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
      
      if(all(is.na(dat$stockValue))) {
        return(NULL)
      } else {
        dat
      }
    }
  }
  )
  
  # The download data button
  output$downloadData <- downloadHandler(
    filename = function() {paste0(input$text, ".csv")},
    content = function(file) {

      dat <- stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion,
               FISHERIES.GUILD %in% input$guild,
               STOCK.CODE %in% input$stock.code,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
      
      if(all(is.na(dat$stockValue))) {
        return(NULL)
      } else {
       write.csv(x = dat, file = file, row.names = FALSE)
      }
    }
  )

  # The download plot button
  output$downloadPlot <- downloadHandler(
    filename = function() {paste0(input$text, ".png")},
    content = function(file) {

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
      
      if(all(is.na(dat$stockValue))) {
        return(NULL)
      } else {
        stockSummaryTrends(dat = dat, overallMean = TRUE, legend.cex = 0.5)
      dev.off()
      }
    }
  )
} 

# Run the application 
shinyApp(ui = ui, server = server)

