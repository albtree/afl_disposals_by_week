# afl_disposals_by_week
Repo that updates automatically overnight after each Australian Rules Football (AFL) game & pushes to a shiny app:
https://albtree.shinyapps.io/afl_disposals_shiny/

player_stats_all - Has player level stats by round from 2014-2023: Round, Position, Home/Away, Opponent, Kicks, Goals, Behinds, Handballs, 
Disposals, Time on Ground Percentage, Centre Bounce Attendances, Kick Ins, Kick In Playons, Average of last 3 games disposals, Average of last 5 games disposals

player_stats_2023 - as above, but for the Season 2023 only

player_stats_by_home_away - Has player level disposal stats from 2014-2023 grouped by Player, Season, and Home or Away

player_stats_by_venue - Has player level disposal stats from 2014-2023 grouped by Player, Season, and Venue played

Uses the fitzRoy package to pull the data
