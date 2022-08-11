# This file initializes the datasets of games with final scores using the
# scrape_game_ids() function
rm(list = ls())
# Install (or update) nflscrapR:
# Make sure devtools is installed with:
install_packages("devtools")
devtools::install_github(repo = "mrcaseb/nflfastR")

# Access nflscrapR:
library(nflfastR)
library(qs)

future::plan("multisession")

#new Games
games_2021 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2021.rds'))
readr::write_csv(games_2021,"/Users/modarresarian/nflproject/static_data/pbp21.csv")

#Old Games
#pbp <- load_pbp(1999:2020, qs = TRUE)
#readr::write_csv(pbp,"/Users/am/Desktop/NFLSets/PBP/NFLPBPNEW.csv")
#ftable(pbp$season)

# Schedule
Seasons<-fast_scraper_schedules(1999:2022)
readr::write_csv(Seasons,"/Users/modarresarian/nflproject/static_data/Schedule.csv")

