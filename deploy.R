library(shiny)
library(reactable)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(hrbrthemes)

# Authenticate
setAccountInfo(name = Sys.getenv("SHINY_ACC_NAME"),
               token = Sys.getenv("TOKEN"),
               secret = Sys.getenv("SECRET"))
# Deploy
deployApp(appFiles = c("ui.R", "server.R", "player_stats_2023.csv", "player_disp_tog.csv"))
