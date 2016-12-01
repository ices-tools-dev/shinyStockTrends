# shinyStockTrends
Shiny web app to produce ICES stock trend plots (.png) and tables (.csv)

## To run locally:

If you have R on your computer, you should be able to run the web app

```{r}
## if you don't have shiny loaded:
# install.packages("shiny")

library(shiny)
runGitHub("ices-tools-dev/shinyStockTrends", subdir = "/StockTrends")
```
