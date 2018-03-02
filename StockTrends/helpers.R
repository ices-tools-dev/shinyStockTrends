
#~~~~~~~~~~~~~~~~~~~~~~~~#
# Stock Status over time #
#~~~~~~~~~~~~~~~~~~~~~~~~#
stock_trends_fun <- function(object,
                             dat,
                             plotting_var = c("StockCode", "FisheriesGuild")[1],
                             grouping_var = c("EcoGuild", "EcoRegion", "FisheriesGuild")[1],
                             metric = c("MSY", "MEAN")[1],
                             active_year = 2016,
                             dynamic = FALSE,
                             data_caption = TRUE,
                             file_name = NULL,
                             save_plot = FALSE,
                             return_plot = TRUE,
                             return_data = FALSE,
                             output_path = NULL,
                             stackable = FALSE) {
  
  if(!grouping_var %in% c("EcoRegion",
                          "EcoGuild",
                          "FisheriesGuild")) {
    stop(paste0("grouping_var: '", grouping_var, "' is not supported. Please try: EcoRegion, EcoGuild, or FisheriesGuild"))
  }
  if(!plotting_var %in% c("StockCode",
                          "FisheriesGuild")) {
    stop(paste0("plotting_var: '", plotting_var, "' is not supported. Please try: stock or guild"))
  }
  if(plotting_var == "FisheriesGuild" &
     grouping_var %in% c("EcoGuild", "FisheriesGuild")) {
    stop("plotting_var = 'FisheriesGuild' should only be used with grouping_var = 'EcoRegion'.")
  }
  if(!metric %in% c("MSY", "MEAN")) {
    stop(paste0("metric: '", metric, "' is not supported. Please try: 'MSY' or 'MEAN'"))
  }
  
  # 
  # grouping_variable <- rlang::sym(grouping_var)
  # plotting_variable <- rlang::sym(plotting_var)
  # 
  # dat <- fisheryO:::clean_stock_trends(active_year = active_year,
  #                                      grouping_var = grouping_var,
  #                                      plotting_var = plotting_var,
  #                                      metric = metric)
   
  # if(!any(dat$stock_trends_frmt$pageGroup %in% object)) {
  #   stop(paste0("object: '", object, "' is not found. Check your spelling/syntax and try again."))
  # }

  # clicks <- dat$sag_complete_summary %>%
  #   mutate(onclick = sprintf("window.open(\"%s%i/%i/%s.pdf\")",
  #                            "http://ices.dk/sites/pub/Publication%20Reports/Advice/",
  #                            YearOfLastAssessment,
  #                            YearOfLastAssessment,
  #                            StockCode)) %>%
  #   select(StockCode,
  #          Description,
  #          onclick) %>%
  #   distinct(.keep_all = TRUE)
  
  p1_dat <- dat %>%
    ungroup() %>%
    # filter(grepl(object, pageGroup)) %>%
    # left_join(clicks, by = c("lineGroup" = "StockCode")) %>%
    mutate(
      # tooltip_line =   sprintf("<b>%s</b>",
      #                               ifelse(lineGroup == "MEAN",
      #                                      "mean",
      #                                      Description)),
      plotGroup = case_when(plotGroup == "SSB_SSBMEAN"~ "SSB/SSB[mean]",
                            plotGroup == "F_FMEAN"~ "F/F[mean]",
                            plotGroup == "F_FMSY"~ "F/F[MSY]",
                            plotGroup == "SSB_MSYBtrigger"~ "SSB/MSY~B[trigger]"),
      plotGroup = factor(plotGroup)) #%>% 
  # labels = c("F/F[MSY]", "SSB/MSY~B[trigger]"))) #%>%
  # select(-Description)
  
  if(length(unique(p1_dat$lineGroup)) <= 2){
    p1_dat <- p1_dat %>%
      filter(lineGroup != "MEAN")
  }
  
  if(metric == "MEAN"){
    p1_dat <- p1_dat %>%
      filter(lineGroup != "MEAN")
  }
  
  adj_names <- sort(setdiff(unique(p1_dat$lineGroup), "MEAN"))
  if(length(adj_names) > 10) {
    values <- rep("#7F7F7F", length(adj_names))
    legend_pos <- "none"
  }
  if(length(adj_names) <= 10) {
    values <- ggthemes::tableau_color_pal('tableau10')(length(adj_names))
    # values = gg_color_hue(length(adj_names))
    legend_pos <- "bottom"
  }
  
  names(values) <- adj_names
  values <- c(values, c(MEAN = "black"))
  
  if(save_plot) {
    if(is.null(file_name)) {
      file_name <- gsub("\\s", "_", object)
      file_name <- gsub("_-_", "-", file_name)
    }
    
    if(is.null(output_path)) {
      output_path <- "~/"
    }
  }
  
  plot_title <- gsub(".*\\s-\\s", "\\1", object)
  plot_title <- gsub(" stocks", "", plot_title)
  
  if(data_caption) {
    data("date_data", package = "fisheryO")
    
    cap_month <- format(min(date_data$date[grepl("sag", date_data$data_file)]), "%B")
    cap_year <- format(min(date_data$date[grepl("sag", date_data$data_file)]), "%Y")
    
    cap_lab <- labs(title = plot_title, x = "Year", y = "", color = "Stock code",
                    caption = sprintf("ICES Stock Assessment Database, %s/%s. ICES, Copenhagen",
                                      cap_year,
                                      cap_month))
  }
  if(!data_caption) {
    cap_lab <- labs(title = plot_title,
                    x = "Year", y = "",
                    color = "Stock code")
  }
  
  
  p1_plot <- ggplot(p1_dat %>% filter(lineGroup != "MEAN"),
                    aes(x = Year, y = plotValue,
                        color = lineGroup,
                        fill = lineGroup#,
                        # onclick = onclick,
                        # data_id = lineGroup,
                        # tooltip = tooltip_line
                    )) +
    geom_hline(yintercept = 1, col = "grey60") +
    theme_bw(base_size = 9) +
    scale_color_manual(values = values) +
    scale_fill_manual(values = values) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    guides(fill = FALSE) +
    theme(legend.position = legend_pos,
          strip.text = element_text(size = 9, angle = 0, hjust = 0),
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          plot.caption = element_text(size = 6)) +
    cap_lab +
    facet_wrap(~ plotGroup, scales = "free_y", labeller = label_parsed, strip.position = "left", ncol = 1, nrow = 2)
  
  if(dynamic) {
    p1_plot <- p1_plot + ggiraph::geom_line_interactive(alpha = 0.8)
    p1_plot <- p1_plot + ggiraph::geom_line_interactive(data = p1_dat %>% filter(lineGroup == "MEAN"),
                                                        alpha = 0.9, size = 1.15)
    
    if(return_plot){
      if(stackable) {
        return(p1_plot)
      }
      if(!stackable) {
        return(ggiraph::ggiraph(code = print(p1_plot),
                                hover_css = "cursor:pointer;stroke:black;stroke-width:3pt;"))
      }
    }
    
    if(save_plot) {
      suppressWarnings(
        rmarkdown::render(system.file("rmd/stockStatusTrends-dynamic.Rmd", package = "fisheryO"),
                          # "~/git/ices-dk/fisheryO/vignettes/stockStatusTrends-dynamic.rmd",
                          output_file = paste0(output_path, file_name, "_", object, "-dynamic.html"),
                          rmarkdown::html_document(template = NULL),
                          envir = new.env())
      )
    }
  }
  
  if(!dynamic) {
    p1_plot <- p1_plot + geom_line(alpha = 0.8)
    p1_plot <- p1_plot + geom_line(data = p1_dat %>% filter(lineGroup == "MEAN"),
                                   alpha = 0.9, size = 1.15)
    
    if(return_plot) {
      return(p1_plot)
    }
    
    if(return_data) {
      write.csv(x = p1_dat, file = paste0(output_path, file_name, ".csv"), row.names = FALSE)
    }
    
    if(save_plot) {
      ggsave(filename = file_name,
             plot = p1_plot,
             width = 170,
             height = 100.5,
             units = "mm",
             dpi = 300)
    }
  }
}





# 
# queryICES <- function() {
#   library(dplyr)
#   library(icesSAG)
#   library(tidyr)
# 
#   # options(scipen = 5)
#   
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   # DATA SOURCE: FAO codes and ICES stock codes #
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   # (NOTE: current ICES stock codes do not necessarily match with FAO 3-character
#   # codes, in the future this look-up table should not be necessary - SL)
#   # speciesID <- read.csv("~/git/ices-dk/shinyStockTrends/StockTrends/data/ICESspeciesID_v1.csv",
#   #                       stringsAsFactors = FALSE)
#   speciesID <- read.csv("data/ICESspeciesID_v1.csv",
#                         stringsAsFactors = FALSE)
#   
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   # DATA SOURCE: Fishery guilds by ICES stock code #
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   # (NOTE: These guilds should become a part of the RECO database - SL)
#   # fisheryGuild <- read.csv("~/git/ices-dk/shinyStockTrends/StockTrends/data/fisheryGuild.csv",
#   #                          stringsAsFactors = FALSE)
#   fisheryGuild <- read.csv("data/fisheryGuild.csv",
#                            stringsAsFactors = FALSE)
#   
#   speciesGuild <- fisheryGuild %>%
#     mutate(speciesID = toupper(gsub( "-.*$", "", Stock.code)),
#            Stock.code = tolower(Stock.code)) %>%
#     full_join(speciesID, c("speciesID" = "oldCode")) %>%
#     select(STOCK.CODE = Stock.code,
#            FISHERIES.GUILD = Fisheries.Guild,
#            SPECIES.ID = speciesID,
#            SPECIES.NAME = speciesName,
#            -newCode)
#   
#   summaryTbl <- icesSAG::getSummaryTable(2016)
#   
#   
#   keeperF <- c("F", "F/FMSY", "F in winter rings", "Harvest rate", "Harvest rate/FMSY", "Fishing Pressure")
#   relativeF <- c("F/FMSY", "Harvest rate/FMSY")
#   
#   keeperSSB <- c("SSB", "B/BMSY", "SSB & Biomass Age 4+", "UW Tv Index", "Stock Size", "Total biomass/BMSY")
#   relativeSSB <- c("B/BMSY", "Total biomass/BMSY")
#   
#   summaryTblClean <- summaryTbl %>%
#     select(Year,
#            STOCK.CODE = fishstock,
#            F,
#            SSB,
#            fishingPressureDescription,
#            stockSizeDescription,
#            LANDINGS = landings,
#            CATCHES = catches) %>%
#     mutate(STOCK.CODE = tolower(STOCK.CODE),
#            fishingPressureDescription = gsub("Fishing Pressure: " , "", fishingPressureDescription),
#            fishingPressureDescription = gsub("Fishing pressure: " , "", fishingPressureDescription),
#            fishingPressureDescription = gsub("msy" , "MSY", fishingPressureDescription),
#            stockSizeDescription = gsub("Stock Size: ", "", stockSizeDescription),
#            stockSizeDescription = gsub("Stock size: ", "", stockSizeDescription),
#            stockSizeDescription = gsub("msy", "MSY", stockSizeDescription),
#            stockSizeDescription = ifelse(is.na(stockSizeDescription), "Relative",
#                                          stockSizeDescription),
#            FmsyDescription = "FMSY",
#            FmsyDescription = ifelse(fishingPressureDescription %in% relativeF,
#                                     "F/FMSY",
#                                     fishingPressureDescription),
#            BmsyDescription = "MSYBtrigger",
#            BmsyDescription = ifelse(stockSizeDescription %in% relativeSSB,
#                                     "SSB/BMSY",
#                                     stockSizeDescription)) %>%
#     filter(stockSizeDescription %in% keeperSSB |
#              fishingPressureDescription %in% keeperF)
#   
#   refPts <- icesSAG::getFishStockReferencePoints(2016)
#   refPts[refPts == ""] <- NA
#   
#   refPtsClean <- refPts %>%
#     select(-key,
#            -AssessmentYear,
#            -RecruitmentAge,
#            -RecruitmentLength,
#            -MSYBescapement,
#            -Fmanagement,
#            -Bmanagement,
#            Flim = FLim,
#            STOCK.CODE = FishStockName) %>%
#     mutate(STOCK.CODE = tolower(STOCK.CODE))
#   
#   fullSummary <- summaryTblClean %>%
#     left_join(refPtsClean, by = c("STOCK.CODE" = "STOCK.CODE")) %>%
#     mutate(MSYBtrigger = ifelse(stockSizeDescription %in% relativeSSB,
#                                 0.5,
#                                 MSYBtrigger),
#            MSYBtrigger= ifelse(!stockSizeDescription %in% keeperSSB,
#                                NA,
#                                MSYBtrigger),
#            FMSY = ifelse(fishingPressureDescription %in% relativeF,
#                          1,
#                          FMSY),
#            FMSY = ifelse(!fishingPressureDescription %in% keeperF,
#                          NA,
#                          FMSY))
#   charCols <- c("STOCK.CODE", "fishingPressureDescription",
#                 "stockSizeDescription",
#                 "FmsyDescription",
#                 "BmsyDescription")
#   fullSummary[!colnames(fullSummary) %in% charCols] <- lapply(fullSummary[!colnames(fullSummary) %in% charCols],
#                                                               as.numeric)
#   
#   colList <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
#   ltyList <- c(1,3:6)
#   
#   # Get stock list
#   # url <- "http://admin.ices.dk/StockListServices/odata/StockListDWsOData?$filter=ActiveYear%20eq%202016"
#   # url <- "~/git/ices-dk/shinyStockTrends/StockTrends/data/StockListDWsOData.json"
#   url <- "data/StockListDWsOData.json"
#   rawsl <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)$value
#   
#   # Combine Norwegian and Barents Sea Ecoregions
#   # rawsl$NorwegianSeaandBarentsSeaEcoregion[!is.na(rawsl$NorwegianSeaEcoregion) |
#   #                                            !is.na(rawsl$BarentsSeaEcoregion)] <- "x"
#   # Reorganize from a wide data frame to a long data frame with species guild information
#   ecoregions <- colnames(rawsl)[grepl("^.+(Ecoregion)$", colnames(rawsl))]
#   
#   dots <- lapply(c("StockCode", "Description", "DataCategory", "AdviceType", ecoregions),
#                  as.symbol)
#   
#   sl <- rawsl %>%
#     select_(.dots = dots) %>%
#     gather(ECOREGION, value, -StockCode, -Description, -DataCategory, -AdviceType) %>%
#     filter(!is.na(value)
#            # ,
#            # !ECOREGION %in% c("NorwegianSeaEcoregion", "BarentsSeaEcoregion")
#            ) %>%
#     select(STOCK.CODE = StockCode,
#            STOCK.NAME = Description,
#            CAT = DataCategory,
#            ADVICE.TYPE = AdviceType,
#            ECOREGION = ECOREGION,
#            -value) %>%
#     mutate(CAT = floor(as.numeric(CAT)),
#            STOCK.CODE = tolower(STOCK.CODE),
#            ADVICE.TYPE = ifelse(ADVICE.TYPE == "MSY/PA",
#                                 "MSY", ADVICE.TYPE),
#            ECOREGION = as.character(ECOREGION),
#            ECOREGION = recode(ECOREGION, "AzoresEcoregion" = "Azores"),
#            ECOREGION = recode(ECOREGION, "BayofBiscayandtheIberianCoastEcoregion" = "Bay of Biscay and the Iberian Coast"),
#            ECOREGION = recode(ECOREGION, "BarentsSeaEcoregion" = "Barents Sea"), 
#            ECOREGION = recode(ECOREGION, "BalticSeaEcoregion" = "Baltic Sea"),
#            ECOREGION = recode(ECOREGION, "CelticSeasEcoregion" = "Celtic Seas"),
#            ECOREGION = recode(ECOREGION, "FaroesEcoregion" = "Faroes"),
#            ECOREGION = recode(ECOREGION, "GreenlandSeaEcoregion" = "Greenland Sea"),
#            ECOREGION = recode(ECOREGION, "IcelandSeaEcoregion" = "Iceland Sea"),
#            ECOREGION = recode(ECOREGION, "GreaterNorthSeaEcoregion" = "Greater North Sea"),
#            ECOREGION = recode(ECOREGION, "OceanicNortheastAtlanticEcoregion" = "Oceanic north-east Atlantic"),
#            ECOREGION = recode(ECOREGION, "NorwegianSeaEcoregion" = "Norwegian Sea")
#            # ECOREGION = recode(ECOREGION, "NorwegianSeaandBarentsSeaEcoregion" = "Norwegian Sea and Barents Sea")
#            ) %>%
#     left_join(speciesGuild, c("STOCK.CODE" = "STOCK.CODE"))
#   
#   
#   stockTrends <- fullSummary %>%
#     left_join(sl, by = "STOCK.CODE") %>%
#     distinct(.keep_all = TRUE) %>%
#     mutate(F_FMSY = ifelse(!is.na(FMSY),
#                            F / FMSY,
#                            NA),
#            F_Flim = ifelse(!is.na(Flim),
#                            F / Flim,
#                            NA),
#            F_Fpa = ifelse(!is.na(Fpa),
#                           F / Fpa,
#                           NA),
#            SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
#                                     SSB / MSYBtrigger,
#                                     NA),
#            SSB_Blim = ifelse(!is.na(Blim),
#                              SSB / Blim,
#                              NA),
#            SSB_Bpa = ifelse(!is.na(Bpa),
#                             SSB / Bpa,
#                             NA)) %>%
#     select(Year,
#            STOCK.CODE,
#            FISHERIES.GUILD,
#            ECOREGION,
#            F_FMSY,
#            F_Flim,
#            F_Fpa,
#            SSB_MSYBtrigger,
#            SSB_Blim,
#            SSB_Bpa) %>%
#     gather(METRIC, stockValue, -Year, -STOCK.CODE, -FISHERIES.GUILD, -ECOREGION)
#   
#   return(stockTrends)
# }
# 
# stockSummaryTrends <- function(dat, 
#                                LINEGROUP = c(STOCK.CODE, FISHERIES.GUILD),
#                                overallMean = NULL,
#                                legend.cex = 0.5) {
#   
#   colList <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
#   ltyList <- c(1,3:6)
#   
#   # LINEGROUP = "STOCK.CODE"
#   dots <- lapply(c("lineGroup" = LINEGROUP, "Year", "plotGroup" = "METRIC", "plotValue" = "stockValue"),
#                  as.symbol)
#   
#   allDat <- dat %>%
#     # mutate(ECOGUILD = paste0(ECOREGION, ", ", FISHERIES.GUILD)) %>%
#     # ungroup() %>%
#     select_(.dots = dots) %>%
#     # select_(
#     #   #pageGroup = ECOGUILD,
#     #   lineGroup = STOCK.CODE,
#     #   Year,
#     #   plotGroup = METRIC,
#     #   plotValue = stockValue) %>%
#     filter(!is.na(plotValue))
#   #
#   oMean <- dat %>%
#     # mutate(ECOGUILD = paste0(ECOREGION, ", ", FISHERIES.GUILD)) %>%
#     # ungroup() %>%
#     # distinct(METRIC, Year, .keep_all = TRUE) %>%
#     group_by(METRIC, Year) %>%
#     summarize(plotValue = mean(stockValue, na.rm = TRUE)) %>%
#     select(#pageGroup = ECOGUILD,
#       Year,
#       plotGroup = METRIC,
#       plotValue) %>%
#     mutate(lineGroup = "MEAN") %>%
#     filter(!is.na(plotValue))
#   
#   allDat <- bind_rows(allDat, oMean) %>%
#     ungroup()
#   
#   # Set up colors
#   plotList <- allDat %>%
#     # group_by(pageGroup) %>%
#     select(lineGroup) %>%
#     mutate(nLines = n_distinct(lineGroup) - 1,
#            COLOR = NA) %>%
#     distinct(lineGroup, .keep_all = TRUE)
#   # %>%
#   # arrange(pageGroup)
#   #
#   singleList <- plotList %>%
#     filter(nLines == 1) %>%
#     mutate(COLOR = colList[1:length(nLines)])
#   #
#   normList <- plotList %>%
#     filter(nLines <= 9 &
#              nLines > 1 &
#              lineGroup != "MEAN") %>%
#     mutate(COLOR = colList[1:length(nLines)])
#   #
#   longList <- plotList %>%
#     filter(nLines > 9 &
#              lineGroup != "MEAN") %>%
#     mutate(COLOR = "grey80")
#   #
#   meanList <- plotList %>%
#     filter(nLines > 1 &
#              lineGroup == "MEAN") %>%
#     mutate(COLOR = "grey40")
#   
#   colorList <- bind_rows(singleList, normList, longList, meanList)
#   allDat <- left_join(colorList, allDat, by = c( "lineGroup"))
#   
#   all.pg <- allDat %>%
#     # group_by(pageGroup) %>%
#     mutate(nLines = n_distinct(lineGroup)) %>%
#     filter(nLines > 2 | lineGroup != "MEAN") %>%
#     filter(lineGroup != "MEAN" | Year != 2016 | plotGroup != "F_FMSY")
#   
#   
#   # all.pg <- allDat
#   
#   # for(pgGroup in unique(df$pageGroup)) { # Data grouped by PaGe (e.g., by ecoregion)
#   # all.pg <- df[df$pageGroup == pgGroup,]
#   #
#   if(any(names(all.pg) == "plotGroup") == FALSE) {
#     all.pg$plotGroup <- "NONE"
#   }
#   #
#   all.pg$plotGroup <- factor(all.pg$plotGroup)
#   # plotFileName = paste0(plotDir, "figure08_", gsub(", ", "-", pgGroup), ".png")
#   
#   # PLOT AVG
#   # png(filename = plotFileName,
#   #     width = 89,
#   #     height = 50.25 * length(unique(all.pg$plotGroup)),
#   #     units = "mm",
#   #     res = 300)
#   
#   par(mfrow = c(length(unique(all.pg$plotGroup)),1),
#       mar = c(2.15, 2.25, 0.45, 0.25),
#       oma = c(0, 0, 0.75, 0),
#       usr = c(0, 1, 0, 1),
#       mgp = c(0, .35, 0),
#       tck = -0.01)
#   
#   # Order the lineGroup to make sure mean is plotted
#   if(overallMean == TRUE) {
#     if(any(all.pg$lineGroup == "MEAN")) {
#       lineGroupOrd <- relevel(factor(unique(all.pg$lineGroup),
#                                      ordered = F),
#                               ref = "MEAN")
#       if(length(lineGroupOrd) > 10) {
#         lineGroupOrd <- factor(lineGroupOrd, levels = rev(levels(lineGroupOrd)))
#       } # close >= 10
#     } #  reorder lineGroupOrd if overallMean == T
#     else {
#       lineGroupOrd <- factor(unique(all.pg$lineGroup),
#                              ordered = T)
#     } # reorder lineGroupOrd if overallMean == F
#   } # TRUE overallMean
#   if(overallMean == FALSE ) {
#     lineGroupOrd <- factor(unique(all.pg$lineGroup),
#                            ordered = T)
#   } # FALSE overallMean
#   #
#   for(plGroup in unique(levels(all.pg$plotGroup))) { # Data grouped by PLot (e.g., F or SSB)
#     #
#     all.pl <- all.pg[all.pg$plotGroup == plGroup,]
#     yRange <- c(0, max(all.pl$plotValue, na.rm =T) + max(all.pl$plotValue, na.rm = T) * .15)
#     xRange <- c(min(all.pl$Year[!is.na(all.pl$plotValue)]),
#                 max(all.pl$Year[!is.na(all.pl$plotValue)]))
#     #
#     plot(NA,
#          type = "l",
#          ylim = yRange,
#          xlim = xRange,
#          yaxt = "n",
#          xaxt = "n",
#          ann = FALSE)
#     abline(h = 1.0, lty = 2, col = "black", lwd = 1)
#     #
#     # Add lines according to the lnGroup
#     for(lnGroup in levels(lineGroupOrd)) {
#       if(all(is.na(all.pl$plotValue[all.pl$lineGroup == lnGroup]))) {
#         lnGroup <- NA
#         next
#       } # close next if all NA
#       if(!all(is.na(all.pl$plotValue[all.pl$lineGroup == lnGroup]))) {
#         d <- data.frame(all.pl$plotValue[all.pl$lineGroup == lnGroup],
#                         all.pl$Year[all.pl$lineGroup == lnGroup])
#         d <- d[order(d[,2]),]
#         col.d <- as.character(unique(all.pl$COLOR[all.pl$lineGroup == lnGroup]))
#         lin.d <- ifelse(lnGroup == "MEAN", 4, 2)
#         lines(d[,2], d[,1], col = col.d, lwd = lin.d)
#       } # close line plotting
#     } # close lnGroup
#     
#     # Numerate axes
#     axis(1, at = pretty(xRange), cex.axis = .75)
#     axis(2, at = pretty(yRange), cex.axis = .75, las = 1)
#     
#     # Label axes
#     if(plGroup == "F_FMSY") {
#       mtext(expression("F/F"["MSY"]), side = 2, line = 1, cex = .75)
#     } # close F_FMSY
#     if(plGroup == "F_Flim") {
#       mtext(expression("F/F"["lim"]), side = 2, line = 1, cex = .75)
#     } # close F_Flim
#     if(plGroup == "F_Fpa") {
#       mtext(expression("F/F"["pa"]), side = 2, line = 1, cex = .75)
#     } # close F_Fpa
#     if(plGroup == "SSB_MSYBtrigger") {
#       mtext(expression("SSB/MSY B"["trigger"]), side = 2, line = 1, cex= .75)
#     } # close SSB_MSYBtrigger
#     if(plGroup == "SSB_Blim") {
#       mtext(expression("SSB/B"["lim"]), side = 2, line = 1, cex= .75)
#     } # close SSB_Blim
#     if(plGroup == "SSB_Bpa") {
#       mtext(expression("SSB/B"["pa"]), side = 2, line = 1, cex= .75)
#     } # close SSB_Bpa
#     
#     # Title
#     # mtext(gsub(".*, ","", pgGroup), side = 3, outer = T, cex = .75)
#     
#     # Legend
#     if(length(lineGroupOrd) <= 10) {
#       legend("topright",
#              legend = as.character(unique(all.pl$lineGroup)),
#              fill = as.character(unique(all.pl$COLOR)),
#              bty = "n",
#              ncol = 3,
#              cex = legend.cex)
#     } # Close less than 9 lines legend
#     if(length(lineGroupOrd) > 10) {
#       legend("topright",
#              legend = "MEAN",
#              fill = "grey40",
#              bty = "n",
#              ncol = 1,
#              cex = legend.cex)
#     } # close more than 10 lines legend
#   } # Close plGroup
#   # dev.off()
#   # }# Close pgGroup
# } # Close function
# 



