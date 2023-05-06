library(tidyverse)
library(fitzRoy)

player_stats <- vector("list", length = 30)
for(varx in 2014:2023){player_stats[[varx]] <- try(fetch_player_stats(season = varx)) %>%mutate(season = varx)}
player_stats_all <- do.call(bind_rows, player_stats) %>% as.data.frame() %>%
  mutate(full_name = paste(player.givenName, player.surname)) %>%
  mutate(starting_position = case_when(player.player.position  == "RK" ~ "Ruck",
                                       player.player.position == "R" ~ "Mid",
                                       player.player.position == "RR" ~ "Mid",
                                       player.player.position == "FF" ~ "Forward",
                                       player.player.position == "FB" ~ "Back",
                                       player.player.position == "FPR" ~ "Forward",
                                       player.player.position == "BPR" ~ "Back",
                                       player.player.position == "CHB" ~ "Back",
                                       player.player.position == "CHF" ~ "Forward",
                                       player.player.position == "WR" ~ "Mid",
                                       player.player.position == "C" ~ "Mid",
                                       player.player.position == "HBFR" ~ "Back",
                                       player.player.position == "HBFL" ~ "Back",
                                       player.player.position == "BPL" ~ "Back",
                                       player.player.position == "HFFL" ~ "Forward",
                                       player.player.position == "WL" ~ "Mid",
                                       player.player.position == "FPL" ~ "Forward",
                                       player.player.position == "HFFR" ~ "Forward",
                                       TRUE ~ player.player.position),
         opponent = if_else(team.name == home.team.name, away.team.name, home.team.name),
         is_home = if_else(team.name == home.team.name, "Home", "Away")) %>%
  select(season, round.roundNumber, venue.name, full_name, team.name, starting_position, is_home, opponent, kicks, goals, behinds, handballs, disposals, timeOnGroundPercentage,
         extendedStats.centreBounceAttendances, extendedStats.kickins, extendedStats.kickinsPlayon) %>%
  drop_na(team.name)%>%
  group_by(full_name, team.name, season) %>%
  arrange(round.roundNumber) %>%
  mutate(disposals_lag_1 = lag(disposals),
         disposals_lag_2 = lag(disposals, n = 2),
         disposals_lag_3 = lag(disposals, n = 3),
         disposals_lag_4 = lag(disposals, n = 4),
         disposals_lag_5 = lag(disposals, n = 5)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(disposals_last_3 = mean(c(disposals_lag_1, disposals_lag_2, disposals_lag_3)),
         disposals_last_5 = mean(c(disposals_lag_1, disposals_lag_2, disposals_lag_3, disposals_lag_4, disposals_lag_5))) %>%
  ungroup()

player_stats_by_venue <- player_stats_all %>%
  group_by(full_name, team.name, season, venue.name) %>%
  summarise(mean_disposals = mean(disposals, na.rm = TRUE),
            min_disposals = min(disposals, na.rm = TRUE),
            max_disposals = max(disposals, na.rm = TRUE),
            games_played = n()) %>%
  ungroup() %>%
  mutate(mean_disposals = round(mean_disposals, digits = 2))

player_stats_by_home_away <- player_stats_all %>%
  group_by(full_name, team.name, season, is_home) %>%
  summarise(mean_disposals = mean(disposals, na.rm = TRUE),
            min_disposals = min(disposals, na.rm = TRUE),
            max_disposals = max(disposals, na.rm = TRUE),
            games_played = n()) %>%
  ungroup() %>%
  mutate(mean_disposals = round(mean_disposals, digits = 2))

write.csv(player_stats_all, file = "player_stats_all.csv")
write.csv(player_stats_by_venue, file = "player_stats_by_venue.csv")
write.csv(player_stats_by_home_away, file = "player_stats_by_home_away.csv")
