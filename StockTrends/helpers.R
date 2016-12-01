

queryICES <- function() {
  library(dplyr)
  library(icesSAG)
  library(tidyr)

  # options(scipen = 5)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # DATA SOURCE: FAO codes and ICES stock codes #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # (NOTE: current ICES stock codes do not necessarily match with FAO 3-character
  # codes, in the future this look-up table should not be necessary - SL)
  # speciesID <- read.csv("~/git/ices-dk/shinyStockTrends/StockTrends/data/ICESspeciesID_v1.csv",
  #                       stringsAsFactors = FALSE)
  speciesID <- read.csv("data/ICESspeciesID_v1.csv",
                        stringsAsFactors = FALSE)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # DATA SOURCE: Fishery guilds by ICES stock code #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # (NOTE: These guilds should become a part of the RECO database - SL)
  # fisheryGuild <- read.csv("~/git/ices-dk/shinyStockTrends/StockTrends/data/fisheryGuild.csv",
  #                          stringsAsFactors = FALSE)
  fisheryGuild <- read.csv("data/fisheryGuild.csv",
                           stringsAsFactors = FALSE)
  
  speciesGuild <- fisheryGuild %>%
    mutate(speciesID = toupper(gsub( "-.*$", "", Stock.code)),
           Stock.code = tolower(Stock.code)) %>%
    full_join(speciesID, c("speciesID" = "oldCode")) %>%
    select(STOCK.CODE = Stock.code,
           FISHERIES.GUILD = Fisheries.Guild,
           SPECIES.ID = speciesID,
           SPECIES.NAME = speciesName,
           -newCode)
  
  summaryTbl <- icesSAG::getSummaryTable(2016)
  
  
  keeperF <- c("F", "F/FMSY", "F in winter rings", "Harvest rate", "Harvest rate/FMSY", "Fishing Pressure")
  relativeF <- c("F/FMSY", "Harvest rate/FMSY")
  
  keeperSSB <- c("SSB", "B/BMSY", "SSB & Biomass Age 4+", "UW Tv Index", "Stock Size", "Total biomass/BMSY")
  relativeSSB <- c("B/BMSY", "Total biomass/BMSY")
  
  summaryTblClean <- summaryTbl %>%
    select(Year,
           STOCK.CODE = fishstock,
           F,
           SSB,
           fishingPressureDescription,
           stockSizeDescription,
           LANDINGS = landings,
           CATCHES = catches) %>%
    mutate(STOCK.CODE = tolower(STOCK.CODE),
           fishingPressureDescription = gsub("Fishing Pressure: " , "", fishingPressureDescription),
           fishingPressureDescription = gsub("Fishing pressure: " , "", fishingPressureDescription),
           fishingPressureDescription = gsub("msy" , "MSY", fishingPressureDescription),
           stockSizeDescription = gsub("Stock Size: ", "", stockSizeDescription),
           stockSizeDescription = gsub("Stock size: ", "", stockSizeDescription),
           stockSizeDescription = gsub("msy", "MSY", stockSizeDescription),
           stockSizeDescription = ifelse(is.na(stockSizeDescription), "Relative",
                                         stockSizeDescription),
           FmsyDescription = "FMSY",
           FmsyDescription = ifelse(fishingPressureDescription %in% relativeF,
                                    "F/FMSY",
                                    fishingPressureDescription),
           BmsyDescription = "MSYBtrigger",
           BmsyDescription = ifelse(stockSizeDescription %in% relativeSSB,
                                    "SSB/BMSY",
                                    stockSizeDescription)) %>%
    filter(stockSizeDescription %in% keeperSSB |
             fishingPressureDescription %in% keeperF)
  
  refPts <- icesSAG::getFishStockReferencePoints(2016)
  refPts[refPts == ""] <- NA
  
  refPtsClean <- refPts %>%
    select(-key,
           -AssessmentYear,
           -RecruitmentAge,
           -RecruitmentLength,
           -MSYBescapement,
           -Fmanagement,
           -Bmanagement,
           Flim = FLim,
           STOCK.CODE = FishStockName) %>%
    mutate(STOCK.CODE = tolower(STOCK.CODE))
  
  fullSummary <- summaryTblClean %>%
    left_join(refPtsClean, by = c("STOCK.CODE" = "STOCK.CODE")) %>%
    mutate(MSYBtrigger = ifelse(stockSizeDescription %in% relativeSSB,
                                0.5,
                                MSYBtrigger),
           MSYBtrigger= ifelse(!stockSizeDescription %in% keeperSSB,
                               NA,
                               MSYBtrigger),
           FMSY = ifelse(fishingPressureDescription %in% relativeF,
                         1,
                         FMSY),
           FMSY = ifelse(!fishingPressureDescription %in% keeperF,
                         NA,
                         FMSY))
  charCols <- c("STOCK.CODE", "fishingPressureDescription",
                "stockSizeDescription",
                "FmsyDescription",
                "BmsyDescription")
  fullSummary[!colnames(fullSummary) %in% charCols] <- lapply(fullSummary[!colnames(fullSummary) %in% charCols],
                                                              as.numeric)
  
  colList <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
               "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
  ltyList <- c(1,3:6)
  
  # Get stock list
  # url <- "http://admin.ices.dk/StockListServices/odata/StockListDWsOData?$filter=ActiveYear%20eq%202016"
  # url <- "~/git/ices-dk/shinyStockTrends/StockTrends/data/StockListDWsOData.json"
  url <- "data/StockListDWsOData.json"
  rawsl <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)$value
  
  # Combine Norwegian and Barents Sea Ecoregions
  # rawsl$NorwegianSeaandBarentsSeaEcoregion[!is.na(rawsl$NorwegianSeaEcoregion) |
  #                                            !is.na(rawsl$BarentsSeaEcoregion)] <- "x"
  # Reorganize from a wide data frame to a long data frame with species guild information
  ecoregions <- colnames(rawsl)[grepl("^.+(Ecoregion)$", colnames(rawsl))]
  
  dots <- lapply(c("StockCode", "Description", "DataCategory", "AdviceType", ecoregions),
                 as.symbol)
  
  sl <- rawsl %>%
    select_(.dots = dots) %>%
    gather(ECOREGION, value, -StockCode, -Description, -DataCategory, -AdviceType) %>%
    filter(!is.na(value)
           # ,
           # !ECOREGION %in% c("NorwegianSeaEcoregion", "BarentsSeaEcoregion")
           ) %>%
    select(STOCK.CODE = StockCode,
           STOCK.NAME = Description,
           CAT = DataCategory,
           ADVICE.TYPE = AdviceType,
           ECOREGION = ECOREGION,
           -value) %>%
    mutate(CAT = floor(as.numeric(CAT)),
           STOCK.CODE = tolower(STOCK.CODE),
           ADVICE.TYPE = ifelse(ADVICE.TYPE == "MSY/PA",
                                "MSY", ADVICE.TYPE),
           ECOREGION = as.character(ECOREGION),
           ECOREGION = recode(ECOREGION, "AzoresEcoregion" = "Azores"),
           ECOREGION = recode(ECOREGION, "BayofBiscayandtheIberianCoastEcoregion" = "Bay of Biscay and the Iberian Coast"),
           ECOREGION = recode(ECOREGION, "BarentsSeaEcoregion" = "Barents Sea"), 
           ECOREGION = recode(ECOREGION, "BalticSeaEcoregion" = "Baltic Sea"),
           ECOREGION = recode(ECOREGION, "CelticSeasEcoregion" = "Celtic Seas"),
           ECOREGION = recode(ECOREGION, "FaroesEcoregion" = "Faroes"),
           ECOREGION = recode(ECOREGION, "GreenlandSeaEcoregion" = "Greenland Sea"),
           ECOREGION = recode(ECOREGION, "IcelandSeaEcoregion" = "Iceland Sea"),
           ECOREGION = recode(ECOREGION, "GreaterNorthSeaEcoregion" = "Greater North Sea"),
           ECOREGION = recode(ECOREGION, "OceanicNortheastAtlanticEcoregion" = "Oceanic north-east Atlantic"),
           ECOREGION = recode(ECOREGION, "NorwegianSeaEcoregion" = "Norwegian Sea")
           # ECOREGION = recode(ECOREGION, "NorwegianSeaandBarentsSeaEcoregion" = "Norwegian Sea and Barents Sea")
           ) %>%
    left_join(speciesGuild, c("STOCK.CODE" = "STOCK.CODE"))
  
  
  stockTrends <- fullSummary %>%
    left_join(sl, by = "STOCK.CODE") %>%
    distinct(.keep_all = TRUE) %>%
    mutate(F_FMSY = ifelse(!is.na(FMSY),
                           F / FMSY,
                           NA),
           F_Flim = ifelse(!is.na(Flim),
                           F / Flim,
                           NA),
           F_Fpa = ifelse(!is.na(Fpa),
                          F / Fpa,
                          NA),
           SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                    SSB / MSYBtrigger,
                                    NA),
           SSB_Blim = ifelse(!is.na(Blim),
                             SSB / Blim,
                             NA),
           SSB_Bpa = ifelse(!is.na(Bpa),
                            SSB / Bpa,
                            NA)) %>%
    select(Year,
           STOCK.CODE,
           FISHERIES.GUILD,
           ECOREGION,
           F_FMSY,
           F_Flim,
           F_Fpa,
           SSB_MSYBtrigger,
           SSB_Blim,
           SSB_Bpa) %>%
    gather(METRIC, stockValue, -Year, -STOCK.CODE, -FISHERIES.GUILD, -ECOREGION)
  
  return(stockTrends)
}



stockSummaryTrends <- function(dat, 
                               overallMean = NULL,
                               legend.cex = 0.5) {
  
  colList <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
               "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
  ltyList <- c(1,3:6)
  
  
  
  allDat <- dat %>%
    # mutate(ECOGUILD = paste0(ECOREGION, ", ", FISHERIES.GUILD)) %>%
    # ungroup() %>%
    select(#pageGroup = ECOGUILD,
      lineGroup = STOCK.CODE,
      Year,
      plotGroup = METRIC,
      plotValue = stockValue) %>%
    filter(!is.na(plotValue))
  #
  oMean <- dat %>%
    # mutate(ECOGUILD = paste0(ECOREGION, ", ", FISHERIES.GUILD)) %>%
    # ungroup() %>%
    # distinct(METRIC, Year, .keep_all = TRUE) %>%
    group_by(METRIC, Year) %>%
    summarize(plotValue = mean(stockValue, na.rm = TRUE)) %>%
    select(#pageGroup = ECOGUILD,
      Year,
      plotGroup = METRIC,
      plotValue) %>%
    mutate(lineGroup = "MEAN") %>%
    filter(!is.na(plotValue))
  
  allDat <- bind_rows(allDat, oMean)
  
  # Set up colors
  plotList <- allDat %>%
    # group_by(pageGroup) %>%
    select(lineGroup) %>%
    mutate(nLines = n_distinct(lineGroup) - 1,
           COLOR = NA) %>%
    distinct(lineGroup, .keep_all = TRUE)
  # %>%
  # arrange(pageGroup)
  #
  singleList <- plotList %>%
    filter(nLines == 1) %>%
    mutate(COLOR = colList[1:length(nLines)])
  #
  normList <- plotList %>%
    filter(nLines <= 9 &
             nLines > 1 &
             lineGroup != "MEAN") %>%
    mutate(COLOR = colList[1:length(nLines)])
  #
  longList <- plotList %>%
    filter(nLines > 9 &
             lineGroup != "MEAN") %>%
    mutate(COLOR = "grey80")
  #
  meanList <- plotList %>%
    filter(nLines > 1 &
             lineGroup == "MEAN") %>%
    mutate(COLOR = "grey40")
  
  colorList <- bind_rows(singleList, normList, longList, meanList)
  allDat <- left_join(colorList, allDat, by = c( "lineGroup"))
  
  all.pg <- allDat %>%
    # group_by(pageGroup) %>%
    mutate(nLines = n_distinct(lineGroup)) %>%
    filter(nLines > 2 | lineGroup != "MEAN") %>%
    filter(lineGroup != "MEAN" | Year != 2016 | plotGroup != "F_FMSY")
  
  
  # all.pg <- allDat
  
  # for(pgGroup in unique(df$pageGroup)) { # Data grouped by PaGe (e.g., by ecoregion)
  # all.pg <- df[df$pageGroup == pgGroup,]
  #
  if(any(names(all.pg) == "plotGroup") == FALSE) {
    all.pg$plotGroup <- "NONE"
  }
  #
  all.pg$plotGroup <- factor(all.pg$plotGroup)
  # plotFileName = paste0(plotDir, "figure08_", gsub(", ", "-", pgGroup), ".png")
  
  # PLOT AVG
  # png(filename = plotFileName,
  #     width = 89,
  #     height = 50.25 * length(unique(all.pg$plotGroup)),
  #     units = "mm",
  #     res = 300)
  
  par(mfrow = c(length(unique(all.pg$plotGroup)),1),
      mar = c(2.15, 2.25, 0.45, 0.25),
      oma = c(0, 0, 0.75, 0),
      usr = c(0, 1, 0, 1),
      mgp = c(0, .35, 0),
      tck = -0.01)
  
  # Order the lineGroup to make sure mean is plotted
  if(overallMean == TRUE) {
    if(any(all.pg$lineGroup == "MEAN")) {
      lineGroupOrd <- relevel(factor(unique(all.pg$lineGroup),
                                     ordered = F),
                              ref = "MEAN")
      if(length(lineGroupOrd) > 10) {
        lineGroupOrd <- factor(lineGroupOrd, levels = rev(levels(lineGroupOrd)))
      } # close >= 10
    } #  reorder lineGroupOrd if overallMean == T
    else {
      lineGroupOrd <- factor(unique(all.pg$lineGroup),
                             ordered = T)
    } # reorder lineGroupOrd if overallMean == F
  } # TRUE overallMean
  if(overallMean == FALSE ) {
    lineGroupOrd <- factor(unique(all.pg$lineGroup),
                           ordered = T)
  } # FALSE overallMean
  #
  for(plGroup in unique(levels(all.pg$plotGroup))) { # Data grouped by PLot (e.g., F or SSB)
    #
    all.pl <- all.pg[all.pg$plotGroup == plGroup,]
    yRange <- c(0, max(all.pl$plotValue, na.rm =T) + max(all.pl$plotValue, na.rm = T) * .15)
    xRange <- c(min(all.pl$Year[!is.na(all.pl$plotValue)]),
                max(all.pl$Year[!is.na(all.pl$plotValue)]))
    #
    plot(NA,
         type = "l",
         ylim = yRange,
         xlim = xRange,
         yaxt = "n",
         xaxt = "n",
         ann = FALSE)
    abline(h = 1.0, lty = 2, col = "black", lwd = 1)
    #
    # Add lines according to the lnGroup
    for(lnGroup in levels(lineGroupOrd)) {
      if(all(is.na(all.pl$plotValue[all.pl$lineGroup == lnGroup]))) {
        lnGroup <- NA
        next
      } # close next if all NA
      if(!all(is.na(all.pl$plotValue[all.pl$lineGroup == lnGroup]))) {
        d <- data.frame(all.pl$plotValue[all.pl$lineGroup == lnGroup],
                        all.pl$Year[all.pl$lineGroup == lnGroup])
        d <- d[order(d[,2]),]
        col.d <- as.character(unique(all.pl$COLOR[all.pl$lineGroup == lnGroup]))
        lin.d <- ifelse(lnGroup == "MEAN", 4, 2)
        lines(d[,2], d[,1], col = col.d, lwd = lin.d)
      } # close line plotting
    } # close lnGroup
    
    # Numerate axes
    axis(1, at = pretty(xRange), cex.axis = .75)
    axis(2, at = pretty(yRange), cex.axis = .75, las = 1)
    
    # Label axes
    if(plGroup == "F_FMSY") {
      mtext(expression("F/F"["MSY"]), side = 2, line = 1, cex = .75)
    } # close F_FMSY
    if(plGroup == "F_Flim") {
      mtext(expression("F/F"["lim"]), side = 2, line = 1, cex = .75)
    } # close F_Flim
    if(plGroup == "F_Fpa") {
      mtext(expression("F/F"["pa"]), side = 2, line = 1, cex = .75)
    } # close F_Fpa
    if(plGroup == "SSB_MSYBtrigger") {
      mtext(expression("SSB/MSY B"["trigger"]), side = 2, line = 1, cex= .75)
    } # close SSB_MSYBtrigger
    if(plGroup == "SSB_Blim") {
      mtext(expression("SSB/B"["lim"]), side = 2, line = 1, cex= .75)
    } # close SSB_Blim
    if(plGroup == "SSB_Bpa") {
      mtext(expression("SSB/B"["pa"]), side = 2, line = 1, cex= .75)
    } # close SSB_Bpa
    
    # Title
    # mtext(gsub(".*, ","", pgGroup), side = 3, outer = T, cex = .75)
    
    # Legend
    if(length(lineGroupOrd) <= 10) {
      legend("topright",
             legend = as.character(unique(all.pl$lineGroup)),
             fill = as.character(unique(all.pl$COLOR)),
             bty = "n",
             ncol = 3,
             cex = legend.cex)
    } # Close less than 9 lines legend
    if(length(lineGroupOrd) > 10) {
      legend("topright",
             legend = "MEAN",
             fill = "grey40",
             bty = "n",
             ncol = 1,
             cex = legend.cex)
    } # close more than 10 lines legend
  } # Close plGroup
  # dev.off()
  # }# Close pgGroup
} # Close function




