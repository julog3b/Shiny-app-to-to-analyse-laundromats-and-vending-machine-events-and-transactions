

# ------------------
# Required packages
# ------------------

pckg=c("shiny","ggplot2","dplyr","leaflet",
       "lubridate","RColorBrewer","plotly",
       "DT","shinythemes", "highcharter",
       "shinyjs", "shinydashboard", "circlize",
       "RODBC", "gapminder", "tidyr", "xts",
       "stringr", "dygraphs", "bigrquery",
       "skimr", "e1071", "ggalluvial", "alluvial",
       "stats", "rpart.plot", "glmnet", "modeldata",
       "rpart", "caret", "visdat", "recipes",
       "shinycssloaders", "visNetwork", "seriation",
       "textdata", "car", "devtools", "wordcloud2",
       "janeaustenr", "tidyverse", "tidytext",
       "forecast", "corrgram", "datasets",
       "MASS", "vcd", "DBI", "pls", "reshape2", "shinyTime",
       "shinyDatetimePickers", "seasonal", "rclipboard")

for(i in 1:length(pckg)) 
{
  print(pckg[i])
  if (!is.element(pckg[i], installed.packages()[,1]))
    install.packages(pckg[i], dep = TRUE)
  require(pckg[i], character.only = TRUE)
}




# options for print page size
options(max.print = 9999999)  
options(dplyr.print_max = 1e9)


# Options for Highcharts. Choosing a theme and setting decimal places to 2.
options(highcharter.theme = hc_theme_ffx(tooltip = list(valueDecimals = 2)))    

transactions <- read.csv("dec2022Transactions.csv")
 
