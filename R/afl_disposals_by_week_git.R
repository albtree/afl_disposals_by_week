library(tidyverse)
library(fitzRoy)

player_afl <- vector("list", length = 30)
for(var_x in 2014:2023){player_afl[[var_x]] <- try(fetch_player_stats_fryzigg(season = var_x)) %>%mutate(season = var_x)}
player_stats_afl_fp <- do.call(bind_rows, player_afl) %>% as.data.frame()%>%
  mutate(full_name = paste(player_first_name, player_last_name),
         match_round = as.integer(match_round)) %>%
  drop_na(match_round) %>%
  select(season, match_round, player_team, full_name, afl_fantasy_score) %>%
  rename(round.roundNumber = match_round,
         team.name = player_team) %>%
  drop_na(team.name) %>%
  group_by(full_name, team.name, season) %>%
  arrange(round.roundNumber) %>%
  mutate(afl_fp_lag_1 = lag(afl_fantasy_score),
         afl_fp_lag_2 = lag(afl_fantasy_score, n = 2),
         afl_fp_lag_3 = lag(afl_fantasy_score, n = 3),
         afl_fp_lag_4 = lag(afl_fantasy_score, n = 4)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(mean_afl_fp_last_3 = mean(c(afl_fantasy_score, afl_fp_lag_1, afl_fp_lag_2)),
         mean_afl_fp_last_5 = mean(c(afl_fantasy_score, afl_fp_lag_1, afl_fp_lag_2, afl_fp_lag_3, afl_fp_lag_4))) %>%
  ungroup()%>%
  mutate(mean_afl_fp_last_3 = round(mean_afl_fp_last_3, digits = 2),
         mean_afl_fp_last_5 = round(mean_afl_fp_last_5, digits = 2))


player_stats <- vector("list", length = 30)
for(varx in 2014:2023){player_stats[[varx]] <- try(fetch_player_stats(season = varx)) %>%mutate(season = varx)}
player_stats_all <- do.call(bind_rows, player_stats) %>% as.data.frame() %>%
  filter(!grepl('Finals|Final', round.name)) %>%
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
         disposals_lag_4 = lag(disposals, n = 4)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(mean_disposals_last_3 = mean(c(disposals, disposals_lag_1, disposals_lag_2)),
         mean_disposals_last_5 = mean(c(disposals, disposals_lag_1, disposals_lag_2, disposals_lag_3, disposals_lag_4))) %>%
  ungroup()%>%
  mutate(mean_disposals_last_3 = round(mean_disposals_last_3, digits = 2),
         mean_disposals_last_5 = round(mean_disposals_last_5, digits = 2)) %>%
  left_join(player_stats_afl_fp, by = c('full_name' = 'full_name',
                                        'team.name' = 'team.name',
                                        'season' = 'season',
                                        'round.roundNumber' = 'round.roundNumber'), na_matches = "never") %>%
  select(season, round.roundNumber, venue.name, full_name, team.name, starting_position, is_home, opponent, kicks, goals, behinds, handballs, disposals, afl_fantasy_score, timeOnGroundPercentage,
         extendedStats.centreBounceAttendances, extendedStats.kickins, extendedStats.kickinsPlayon,mean_disposals_last_3, mean_disposals_last_5,mean_afl_fp_last_3, mean_afl_fp_last_5)



player_stats_2023 <- player_stats_all %>% filter(season == 2023) 
           
player_positions <- player_stats_all %>%
  select(season, full_name, team.name, starting_position) %>%
  group_by(season, full_name, team.name, starting_position) %>%
  summarise(games_played = n()) %>%
  arrange(-games_played) %>%
  slice_max(games_played, n =1, with_ties = FALSE) %>%
  ungroup() %>%
  select(season, full_name, team.name, starting_position) 


player_disp_tog <- player_stats_all %>%
  group_by(full_name, team.name, season) %>%
  summarise(sum_disposals = sum(disposals, na.rm = TRUE),
            sum_TOG = sum(timeOnGroundPercentage, na.rm = TRUE),
            mean_disposals = mean(disposals, na.rm = TRUE),
            mean_TOG = mean(timeOnGroundPercentage, na.rm = TRUE),
            games_played = n()) %>%
  ungroup() %>%
  mutate(disp_per_TOG = sum_disposals/sum_TOG,
         mean_disposals = round(mean_disposals, digits = 2),
         mean_TOG = round(mean_TOG, digits = 2),
         disp_per_TOG = round(disp_per_TOG, digits = 4)) %>%
  left_join(player_positions, by = c('season' = 'season',
                                     'full_name' = 'full_name',
                                     'team.name' = 'team.name'),
            na_matches = "never")
  

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


write.csv(player_stats_all, file = "player_stats_all.csv", row.names = F)
write.csv(player_stats_2023, file = "player_stats_2023.csv", row.names = F)
write.csv(player_stats_by_venue, file = "player_stats_by_venue.csv", row.names = F)
write.csv(player_stats_by_home_away, file = "player_stats_by_home_away.csv", row.names = F)
write.csv(player_disp_tog, file = "player_disp_tog.csv", row.names = F)
