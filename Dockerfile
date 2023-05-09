FROM rocker/shiny:4.2.1
RUN install2.r rsconnect shiny dplyr ggrepel reactable nflplotR ggplot2 hrbrthemes
WORKDIR /home/afl_disposals_shiny/
COPY ui.R ui.R 
COPY server.R server.R 
COPY player_stats_2023.csv player_stats_2023.csv
COPY player_disp_tog.csv player_disp_tog.csv
COPY deploy.R deploy.R
CMD Rscript deploy.R
