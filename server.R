library(shiny)
library(reactable)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(hrbrthemes)

server <- function(input, output, session) {
  output$table <- renderReactable({reactable(afl_df,
                                             columns = list(
                                               season = colDef(name = "Season"),
                                               round.roundNumber = colDef(name = "Round"),
                                               venue.name = colDef(name = "Venue"),
                                               full_name = colDef(name = "Player"),
                                               team.name = colDef(name = "Team"),
                                               starting_position = colDef(name = "Position"),
                                               is_home = colDef(name = "Home/Away"),
                                               opponent = colDef(name = "Opponent"),
                                               kicks = colDef(name = "Kicks"),
                                               goals = colDef(name = "Goals"),
                                               behinds = colDef(name = "Behinds"),
                                               handballs = colDef(name = "Handballs"),
                                               disposals = colDef(name = "Disposals"),
                                               afl_fantasy_score = colDef(name = "AFL FP"),
                                               timeOnGroundPercentage = colDef(name = "TOG %"),
                                               extendedStats.centreBounceAttendances = colDef(name = "CB Attendances"),
                                               extendedStats.kickins = colDef(name = "Kick Ins"),
                                               extendedStats.kickinsPlayon = colDef(name = "Kick Ins Playons"),
                                               mean_disposals_last_3 = colDef(name = "Avg Disposals Last 3"),
                                               mean_disposals_last_5 = colDef(name = "Avg Disposals Last 5"),
                                               mean_afl_fp_last_3 = colDef(name = "Avg AFL FP Last 3"),
                                               mean_afl_fp_last_5 = colDef(name = "Avg AFL FP Last 5")),
                                             filterable = TRUE, showPageSizeOptions = TRUE, minRows = 10)})
  
  output$team.nameOutput <- renderUI({
    selectInput("team.name", "Team",
                sort(unique(afl_tog$team.name)),
                selected = "Richmond")
  })
  {observeEvent}
  
  filtered <- reactive({if (is.null(input$team.name)) {return(NULL)
  }
    afl_tog %>%
      filter(team.name %in% unique(input$team.name))})
  
  
  output$tog_disp_plot <- renderPlot(ggplot(data = filtered(), aes(x = mean_TOG, y = mean_disposals)) +
                                       geom_point(aes(color = starting_position), size = 4) +
                                       geom_label_repel(aes(label = full_name), size = 4) +
                                       labs(color = "Position",
                                            x = "Mean Time on Ground Percentage",
                                            y = "Mean Disposals") +
                                       theme_ipsum_rc() +
                                       theme(text = element_text(size = 14)))
}
