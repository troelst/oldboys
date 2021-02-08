library(rvest)
library(tidyverse)
library(jsonlite)
library(glue)

teamid <- xxx

teamdata <- function(teamid){
  teamhtml <- glue("https://app.esportligaen.dk/api/team/{teamid}?includeViewInfo=true")
  teamdata <- fromJSON(teamhtml)
}

matchdata <- function(matchid){
matchhtml <- glue("https://app.esportligaen.dk/api/match/details/{matchid}")
matchdata <- fromJSON(matchhtml)
}

kampdata <- map(teamdata(teamid)[["matches"]][["id"]], matchdata)


scorematch <- function(m){
  score <- tibble(date = m$time,
                  hometeam = m$MatchTeam$Team$name[1],
                  awayteam = m$MatchTeam$Team$name[2],
                  homescore = m$MatchTeams$score[1],
                  awayscore = m$MatchTeams$score[2],
                  score = glue("{homescore}-{awayscore}"),
                  res = case_when(homescore > awayscore ~ "1",
                                  homescore < awayscore ~ "2",
                                  TRUE ~ "x"
                    
                  ),
                  vinder = case_when(homescore > awayscore ~ hometeam,
                                     homescore < awayscore ~ awayteam,
                                     TRUE ~ "Uafgjort"
                                     
                  ),
                  map = ifelse(length(m$mapName) == 0, "Ukendt", m$mapName)
  )
}

kampe <- map_dfr(kampdata, scorematch)
