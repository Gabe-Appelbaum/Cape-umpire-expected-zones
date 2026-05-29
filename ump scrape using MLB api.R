library(httr)
library(jsonlite)
library(tidyverse)
library(httr2)
library(baseballr)


CCBL_Teams <- list("Hyannis Harbor Hawks", "Falmouth Commodores", "Harwich Mariners", 
                   "Orleans Firebirds", "Yarmouth-Dennis Red Sox", "Bourne Braves",
                   "Cotuit Kettleers", "Chatham Anglers", "Wareham Gatemen",
                   "Brewster Whitecaps")

cape_schedule <- mlb_schedule(season = 2024, level_ids = 22) %>%
      filter(teams_away_team_name %in% CCBL_Teams) %>%
      select(date, game_pk, teams_home_team_name, teams_away_team_name)

game_pks <- unique(cape_schedule$game_pk)

df <- data.frame()



for (game_id in game_pks){
      url <- paste0('http://statsapi.mlb.com/api/v1.1/game/', game_id, '/feed/live')
      response <- GET(url)
      content <- content(response, as = "text")
      json_data <- fromJSON(content)
      
      umps <- json_data$liveData$boxscore$officials$official
      roles <- json_data$liveData$boxscore$officials$officialType
      
      umps <- cbind(umps, roles) %>%
            filter(roles == "Home Plate") %>%
            mutate(game_pk = game_id) %>%
            select(game_pk, fullName)
      
      df <- rbind(df, umps)
}


full_df <- full_join(
      cape_schedule,
      df
)

# now lets create the trackman gameID to merge with trackman file
full_df <- full_df %>%
      mutate(
            trackmanID = paste0(
                        gsub("-", "", date),
                        "-",
                        "CCBL",
                        str_extract(teams_home_team_name, "^[^ ]+")
                        )
            ) %>%
      mutate(
            trackmanID = gsub("-Dennis", "", trackmanID)
      )

write_csv(full_df %>% select(fullName, trackmanID), "data/trackman 2024 umps.csv")
