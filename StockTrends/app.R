rm(list = ls())


source("helpers.R")

ui <- fluidPage(
  img(src = "http://www.ices.dk/SiteCollectionImages/ICES%20logos/ICES%20logo%20acronym%20PNG%20format.png",
      width = "100px"),
  titlePanel("Trends in stock status"),
  sidebarLayout(
    sidebarPanel(
      br(),
      actionButton(inputId = "queryICES",
                   label = h5("Query data from ICES databases")
      ),
      br(),
      textOutput(outputId = "nrows"),
      checkboxGroupInput(
        inputId = "ecoregion",
        label = h5("Choose ecoregion(s)"),
        choices = ""
      ),
      checkboxGroupInput(
        inputId = "guild",
        label = h5("Choose guild(s)"),
        choices = ""
      ),
      checkboxGroupInput(
        inputId = "stock.code",
        label = h5("Choose stock(s)"),
        choices = ""
      ),
      actionButton(inputId = "selectall",
                   label = "Select/Deselect all stocks"
      )
    ),
    mainPanel(
      # verbatimTextOutput("value")
      plotOutput("linePlot", width = "75%", height = "400px"),
      downloadButton('downloadPlot', 'Download plot'),
      downloadButton('downloadData', 'Download data'),
      textInput("text", label = h5("File name"), value = "shinyPlot"),
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
    sort(unique(stockTrends()$ECOREGION))
  })
  observeEvent(input$queryICES, {
    updateCheckboxGroupInput(session, "ecoregion", choices = outVarEco())
  })
  
  # When ecoregions are selected display the available guilds
  outVarGuild = reactive({
      sort(unique(stockTrends()$FISHERIES.GUILD[which(stockTrends()$ECOREGION %in% input$ecoregion)]))
  })
  observeEvent(input$ecoregion, {
    updateCheckboxGroupInput(session, "guild", choices = outVarGuild())
  })
  

  # When ecoregion and guilds are selected display the available stocks
  outVarStock = reactive({
      if(input$ecoregion %in% "") return(NULL)
      sort(unique(stockTrends()$STOCK.CODE[which(stockTrends()$ECOREGION %in% input$ecoregion &
                                                 stockTrends()$FISHERIES.GUILD %in% input$guild)]))
  })
  observeEvent(input$guild, {
  updateCheckboxGroupInput(session, "stock.code", choices = outVarStock())
  })

  # When select all button is toggled, select all available stocks
  observe({
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0){
        updateCheckboxGroupInput(session = session,
                                 inputId = "stock.code",
                                 choices = outVarStock(),
                                 selected = c())

      } else {
        updateCheckboxGroupInput(session = session,
                                 inputId = "stock.code",
                                 choices = outVarStock(),
                                 selected = c(outVarStock()))

      }}
  })

  ###############
  ### OUTPUTS ###
  ###############
  
  # Print the number of rows downloaded
  output$nrows <- renderText({
    paste0(nrow(stockTrends()), " rows are loaded.")
  })

  # Print the line plot of stock trends
  output$linePlot <- renderPlot({
    if(is.null(input$queryICES) ||
       is.null(input$ecoregion) ||
       is.null(input$guild) ||
       is.null(input$stock.code)) {
      return(NULL)
    } else {

      stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion,
               FISHERIES.GUILD %in% input$guild,
               STOCK.CODE %in% input$stock.code,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
        stockSummaryTrends(overallMean = TRUE, legend.cex = 1)


    }
  })

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
      
      stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion,
               FISHERIES.GUILD %in% input$guild,
               STOCK.CODE %in% input$stock.code,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger"))
    }
  })

  # The download data button
  output$downloadData <- downloadHandler(        #### called from UI
    filename = function() {paste0(input$text, ".csv")},
    content = function(file) {
      
      stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion,
               FISHERIES.GUILD %in% input$guild,
               STOCK.CODE %in% input$stock.code,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
        write.csv(file, row.names = FALSE)
    })

  # The download plot button
  output$downloadPlot <- downloadHandler(
    filename = function() {paste0(input$text, ".png")},
    content = function(file) {
      png(file,
          width = 89,
          height = 50.25 * 2,
          units = "mm",
          res = 300)
      
      stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion,
               FISHERIES.GUILD %in% input$guild,
               STOCK.CODE %in% input$stock.code,
               METRIC %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
        stockSummaryTrends(overallMean = TRUE, legend.cex = 0.5)
      
      dev.off()
    }
  )
} 

# Run the application 
shinyApp(ui = ui, server = server)

