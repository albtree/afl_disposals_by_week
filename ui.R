library(shiny)
library(reactable)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(hrbrthemes)

afl_df <- read.csv("player_stats_2023.csv", stringsAsFactors = FALSE) 
afl_tog <- read.csv("player_disp_tog.csv") %>% filter(season == 2023)
ui <- fluidPage(
  titlePanel("AFL Disposals"),
  mainPanel(
    h5("Data scraped using fitZroy package. ShinyApp by TAlbTree"),
    h6(tags$a(href="https://github.com/albtree/afl_disposals_by_week",
              "albtree/afl_disposals_by_week")),
    reactableOutput("table"),
    h3("Time on Ground Percentage vs Mean Disposals Plot"),
    h4("Season: 2023"),
    h5("Top left = Efficient at attaining disposals when on ground"),
    h5("Bottom right = Inefficient at attaining disposals when on ground"),
    uiOutput("team.nameOutput"),
    br(),
    plotOutput("tog_disp_plot")))