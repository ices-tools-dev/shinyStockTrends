library(dplyr)
library(shiny)

devtools::install_github("ices-tools-prod/fisheryO")
library(fisheryO)
source("helpers.R")

ui <- fluidPage(
  titlePanel("Trends in stock status"),
  sidebarLayout(
    sidebarPanel(
      
      h5( "Select the data you want to explore:"),
      radioButtons(inputId = "dataType", 
                   label = "",
                   list("Fish stocks by guild and ecoregion" = "stoGldEco",
                        "Fisheries guilds by ecoregion" = "gldEco"),
                        # "Fish stocks by name" = "stockName"),
                   selected = ""),
      
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
          inputId = "stock_stoGldEco",
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

###~~~~~~~~~###
#### Server ####
###~~~~~~~~~###

server = function(input, output, session){
  
  stockTrends <- eventReactive(input$dataType, {
    
    if(req(input$dataType) == "stoGldEco") {
      stoGldEco_dat <- fisheryO:::clean_stock_trends(active_year = 2017,
                                                     grouping_var = "EcoGuild",
                                                     plotting_var = "StockCode",
                                                     metric = "MSY")$stock_trends_frmt %>% 
        ungroup %>% 
        mutate(pageGroup = gsub(" stocks", "", pageGroup))
      
      stoGldEco_dat <- tidyr::separate(stoGldEco_dat, col = pageGroup,
                      into = c("ECOREGION", "GUILD"),
                      sep = " - ")
      return(stoGldEco_dat)
    }
    
    if(req(input$dataType) == "gldEco") {
      
      gldEco_dat <- fisheryO:::clean_stock_trends(active_year = 2017,
                                                  grouping_var = "EcoRegion",
                                                  plotting_var = "FisheriesGuild",
                                                  metric = "MSY")$stock_trends_frmt
      
      gldEco_dat %>% 
        dplyr::rename(ECOREGION = pageGroup,
                      GUILD = lineGroup)
      }

  })
   
  ######################
  ### Input handling ###
  ######################
  
  # When the ICES databases are called, update ecoregions based on available data
  observe({
    req(input$dataType)
    
    if(req(input$dataType) == "stoGldEco") {

      # stockTrends <- fisheryO:::clean_stock_trends(active_year = 2017,
      #                                      grouping_var = "EcoGuild",
      #                                      plotting_var = "StockCode",
      #                                      metric = "MSY")$stock_trends_frmt %>% 
      #   ungroup %>% 
      #   mutate(pageGroup = gsub(" stocks", "", pageGroup))
      # 
      # stockTrends <- tidyr::separate(stockTrends, col = pageGroup,
      #                                      into = c("ECOREGION", "GUILD"),
      #                                      sep = " - ")
      stock_data <- stockTrends()
      
      choices <- sort(unique(stock_data$ECOREGION))
      
      updateCheckboxGroupInput(session, 
                               "ecoregion_stoGldEco",
                               choices = choices)
      
      if(req(input$selectEco_stoGldEco)) {
        
        if(req(input$selectEco_stoGldEco) %% 2 == 0) {
      
          updateCheckboxGroupInput(session = session,
                                   inputId = "ecoregion_stoGldEco",
                                   choices = choices,
                                   selected = c())
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "ecoregion_stoGldEco",
                                   choices = choices,
                                   selected = c(choices))
        }
      }
    }
    
    if(req(input$dataType) == "gldEco") {
      
      # stockTrends <- fisheryO:::clean_stock_trends(active_year = 2017,
      #                                      grouping_var = "EcoRegion",
      #                                      plotting_var = "FisheriesGuild",
      #                                      metric = "MSY")$stock_trends_frmt
      # 
      # stockTrends <- stockTrends %>% 
      #   dplyr::rename(ECOREGION = pageGroup,
      #          GUILD = lineGroup)
      # 
      stock_data <- stockTrends()
      
      choices <- sort(unique(stock_data$ECOREGION))
      
      updateCheckboxGroupInput(session, 
                               "ecoregion_gldEco",
                               choices = choices)
      
      if(req(input$selectEco_gldEco)) {
        if(req(input$selectEco_gldEco) %% 2 == 0) {
          updateCheckboxGroupInput(session = session,
                                   inputId = "ecoregion_gldEco",
                                   choices = choices,
                                   selected = c())
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "ecoregion_gldEco",
                                   choices = choices,
                                   selected = c(choices))
        }
      }
    }
  })
  
  # When ecoregions are selected display the available guilds
  observe({
    req(input$dataType)
    
    if(req(input$dataType) == "stoGldEco") {
      req(input$ecoregion_stoGldEco)
      
      stock_data <- stockTrends()
      
      choices_stoGldEco <- sort(unique(stock_data$GUILD[which(stock_data$ECOREGION
                                                                     %in% input$ecoregion_stoGldEco)]))
      
      updateCheckboxGroupInput(session, 
                               "guild_stoGldEco",
                               choices = choices_stoGldEco)
      if(req(input$selectGuild_stoGldEco)) {
        if(req(input$selectGuild_stoGldEco) %% 2 == 0) {
          updateCheckboxGroupInput(session = session,
                                   inputId = "guild_stoGldEco",
                                   choices = choices_stoGldEco,
                                   selected = c())
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "guild_stoGldEco",
                                   choices = choices_stoGldEco,
                                   selected = c(choices_stoGldEco))
        }
      }
    }
    
    if(req(input$dataType) == "gldEco") {
      req(input$ecoregion_gldEco)
      
      stock_data <- stockTrends()
      
      choices_gldEco <- sort(unique(stock_data$GUILD[which(stock_data$ECOREGION %in% 
                                                                          input$ecoregion_gldEco)]))
      
      updateCheckboxGroupInput(session, 
                               "guild_gldEco",
                               choices = choices_gldEco)
      if(req(input$selectGuild_gldEco)) {
        if(req(input$selectGuild_gldEco) %% 2 == 0) {
          updateCheckboxGroupInput(session = session,
                                   inputId = "guild_gldEco",
                                   choices = choices_gldEco,
                                   selected = c())
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "guild_gldEco",
                                   choices = choices_gldEco,
                                   selected = c(choices_gldEco))
        }
      }
    }
  })
  
  # When ecoregion and guilds are selected display the available stocks
  observe({
    req(input$dataType,
        input$ecoregion_stoGldEco,
        input$guild_stoGldEco)
    
    if(req(input$dataType) == "stoGldEco") {
      
      stock_data <- stockTrends()
      
      stocks_stoGldEco <- sort(unique(stock_data$lineGroup[which(stock_data$ECOREGION %in% input$ecoregion_stoGldEco &
                                                                       stock_data$GUILD %in% input$guild_stoGldEco)]))
      updateCheckboxGroupInput(session, 
                               "stock_stoGldEco",
                               choices = stocks_stoGldEco)
      if(req(input$selectStock_stoGldEco)) {
        if(req(input$selectStock_stoGldEco) %% 2 == 0) {
          updateCheckboxGroupInput(session = session,
                                   inputId = "stock_stoGldEco",
                                   choices = stocks_stoGldEco,
                                   selected = c())
        } else {
          updateCheckboxGroupInput(session = session,
                                   inputId = "stock_stoGldEco",
                                   choices = stocks_stoGldEco,
                                   selected = c(stocks_stoGldEco))
        }
      }
    } 
  })
  
  # 
  ###############
  ### OUTPUTS ###
  ###############

  # Print the line plot of stock trends
  output$linePlot <- renderPlot({
    validate(need(input$dataType, "Please select how you want the data")
    )
    
    if(req(input$dataType) == "stoGldEco") {
      
      validate(    
        need(input$ecoregion_stoGldEco, "Please select ecoregion"),
        need(input$guild_stoGldEco, "Please select guild"),
        need(input$stock_stoGldEco, "Please select stock")
      )
      
      dat <- stockTrends() %>%
        filter(ECOREGION %in% input$ecoregion_stoGldEco,
               GUILD %in% input$guild_stoGldEco,
               lineGroup %in% input$stock_stoGldEco,
               plotGroup %in% c("F_FMSY", "SSB_MSYBtrigger")) %>% 
        mutate(pageGroup = sprintf("%s - %s stocks",
                                   ECOREGION,
                                   GUILD)) %>% 
        select(-ECOREGION,
               -GUILD)
      
      validate(
        need(any(!is.na(dat$plotValue)), "This stock does not have necessary data")
      )
      
      object <- unique(dat$pageGroup)
      
      return(stock_trends_fun(object = object,
                       dat = dat,
                       plotting_var = "StockCode",
                       grouping_var = "EcoGuild",
                       metric = "MSY",
                       active_year = 2017,
                       dynamic = FALSE,
                       data_caption = TRUE,
                       file_name = NULL,
                       save_plot = FALSE,
                       return_plot = TRUE,
                       return_data = FALSE,
                       output_path = NULL,
                       stackable = FALSE))
    }
    
    if(req(input$dataType) == "gldEco") {
      
      validate(    
        need(input$ecoregion_gldEco, "Please select ecoregion"),
        need(input$guild_gldEco, "Please select guild")
      )
      
      dat <- stockTrends()%>%
        filter(ECOREGION %in% input$ecoregion_gldEco,
               GUILD %in% input$guild_gldEco,
               plotGroup %in% c("F_FMSY", "SSB_MSYBtrigger")) %>% 
        rename(pageGroup = ECOREGION,
               lineGroup = GUILD)
      
      validate(
        need(any(!is.na(dat$plotValue)), "This stock does not have necessary data")
      )
      
      object <- unique(dat$pageGroup)
      
      return(stock_trends_fun(object = object,
                       dat = dat,
                       plotting_var = "FisheriesGuild",
                       grouping_var = "EcoRegion",
                       metric = "MSY",
                       active_year = 2017,
                       dynamic = FALSE,
                       data_caption = TRUE,
                       file_name = NULL,
                       save_plot = FALSE,
                       return_plot = TRUE,
                       return_data = FALSE,
                       output_path = NULL,
                       stackable = FALSE))
    }
  })
  
  # Print user-defined file name
  output$valuePlot <- renderPrint({ input$textPlot })
  output$valueData <- renderPrint({ input$textData })
  
  # Print the data table
  output$dataTable <- renderDataTable({
    
    validate(need(input$dataType, "Please select how you want the data")
    )
    
    if(req(input$dataType) == "stoGldEco") {
      
      validate(    
        need(input$ecoregion_stoGldEco, "Please select ecoregion"),
        need(input$guild_stoGldEco, "Please select guild"),
        need(input$stock_stoGldEco, "Please select stock")
      )
      
      dat <- stockTrends()%>%
        filter(ECOREGION %in% input$ecoregion_stoGldEco,
               GUILD %in% input$guild_stoGldEco,
               lineGroup %in% input$stock_stoGldEco,
               plotGroup %in% c("F_FMSY", "SSB_MSYBtrigger"))
      
      validate(
        need(any(!is.na(dat$plotValue)), "This stock does not have necessary data")
      )
      return(dat)
    }
    
    if(req(input$dataType) == "gldEco") {
      
      validate(    
        need(input$ecoregion_gldEco, "Please select ecoregion"),
        need(input$guild_gldEco, "Please select guild")
      )
      
      dat <- stockTrends()%>%
        filter(ECOREGION %in% input$ecoregion_gldEco,
               GUILD %in% input$guild_gldEco,
               plotGroup %in% c("F_FMSY", "SSB_MSYBtrigger"))
      
      validate(
        need(any(!is.na(dat$plotValue)), "This stock does not have necessary data")
      )
      return(dat)
    }
  })
  
  # The download data button
  output$downloadData <- downloadHandler(
    filename = function() {paste0(input$textData, ".csv")},
    content = function(file) {
   
      validate(need(input$dataType, "Please select how you want the data")
      )
      
      if(req(input$dataType) == "stoGldEco") {
        
        validate(    
          need(input$ecoregion_stoGldEco, "Please select ecoregion"),
          need(input$guild_stoGldEco, "Please select guild"),
          need(input$stock_stoGldEco, "Please select stock")
        )
        
        dat <- stockTrends()%>%
          filter(ECOREGION %in% input$ecoregion_stoGldEco,
                 GUILD %in% input$guild_stoGldEco,
                 lineGroup %in% input$stock_stoGldEco,
                 plotGroup %in% c("F_FMSY", "SSB_MSYBtrigger"))
        
        validate(
          need(any(!is.na(dat$plotValue)), "This stock does not have necessary data")
        )
        
        write.csv(x = dat, file = file, row.names = FALSE)
      }
      
      if(req(input$dataType) == "gldEco") {
        
        validate(    
          need(input$ecoregion_gldEco, "Please select ecoregion"),
          need(input$guild_gldEco, "Please select guild")
        )
        
        dat <- stockTrends()%>%
          filter(ECOREGION %in% input$ecoregion_gldEco,
                 GUILD %in% input$guild_gldEco,
                 plotGroup %in% c("F_FMSY", "SSB_MSYBtrigger"))
        
        validate(
          need(any(!is.na(dat$plotValue)), "This stock does not have necessary data")
        )
        
        write.csv(x = dat, file = file, row.names = FALSE)
      }
    })
  
  # The download plot button
  output$downloadPlot <- downloadHandler(
    filename = function() {paste0(input$textPlot, ".png")},
    content = function(file) {
      
      validate(need(input$dataType, "Please select how you want the data")
      )
      
      if(req(input$dataType) == "stoGldEco") {
        
        validate(    
          need(input$ecoregion_stoGldEco, "Please select ecoregion"),
          need(input$guild_stoGldEco, "Please select guild"),
          need(input$stock_stoGldEco, "Please select stock")
        )
        
        dat <- stockTrends()%>%
          filter(ECOREGION %in% input$ecoregion_stoGldEco,
                 GUILD %in% input$guild_stoGldEco,
                 lineGroup %in% input$stock_stoGldEco,
                 plotGroup %in% c("F_FMSY", "SSB_MSYBtrigger")) %>% 
          mutate(pageGroup = sprintf("%s - %s stocks", 
                                     ECOREGION, 
                                     GUILD)) %>% 
          select(-ECOREGION,
                 -GUILD)
        
        validate(
          need(any(!is.na(dat$plotValue)), "This stock does not have necessary data")
        )
        
        object <- unique(dat$pageGroup)
        
        stock_trends_fun(object = object,
                         dat = dat,
                         plotting_var = "StockCode",
                         grouping_var = "EcoGuild",
                         metric = "MSY",
                         active_year = 2017,
                         dynamic = FALSE,
                         data_caption = TRUE,
                         file_name = file,
                         save_plot = TRUE,
                         return_plot = FALSE,
                         return_data = FALSE,
                         output_path = NULL,
                         stackable = FALSE)
        
      }
      
      if(req(input$dataType) == "gldEco") {
        
        validate(    
          need(input$ecoregion_gldEco, "Please select ecoregion"),
          need(input$guild_gldEco, "Please select guild")
        )
        
        dat <- stockTrends()%>%
          filter(ECOREGION %in% input$ecoregion_gldEco,
                 GUILD %in% input$guild_gldEco,
                 plotGroup %in% c("F_FMSY", "SSB_MSYBtrigger")) %>%
          rename(pageGroup = ECOREGION,
                 lineGroup = GUILD)
          # select(-STOCK.CODE) %>%
          # group_by(Year, FISHERIES.GUILD, METRIC) %>%
          # summarize(stockValue = mean(stockValue, na.rm = TRUE))
        
        validate(
          need(any(!is.na(dat$plotValue)), "This stock does not have necessary data")
        )  
        
        object <- unique(dat$pageGroup)
        
        stock_trends_fun(object = output,
                         dat = dat,
                         plotting_var = "FisheriesGuild",
                         grouping_var = "EcoRegion",
                         metric = "MSY",
                         active_year = 2017,
                         dynamic = FALSE,
                         data_caption = TRUE,
                         file_name = file,
                         save_plot = TRUE,
                         return_plot = FALSE,
                         return_data = FALSE,
                         output_path = NULL,
                         stackable = FALSE)
        
        # png(file,
        #     width = 89,
        #     height = 50.25 * 2,
        #     units = "mm",
        #     res = 300)
        # 
        # stockSummaryTrends(dat = dat, LINEGROUP = "FISHERIES.GUILD", overallMean = TRUE, legend.cex = 0.5)
        # dev.off()
      }
    }
  )} 



# Run the application 
shinyApp(ui = ui, server = server)

