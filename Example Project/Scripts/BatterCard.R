library(dplyr)
source("statsapi_action_get.R")

endpoint <- "people"
url_path <- ""
params = data.frame(key = c("personIds", "hydrate" ),
                    value = c("595281", "awards,education,stats(group=[hitting],type=[yearByYear])"),
                    stringsAsFactors = FALSE
)

api_response <- statsapi_action_get(endpoint = endpoint, params = params)$people

## bio info - red in diagram
bio_info <- api_response %>%
  select(id, lastName, firstName, height, weight, birthDate, birthCity, birthStateProvince, birthCountry)

## handedness - purple 
hand_info <- api_response %>%
  select(id) %>%
  bind_cols(bats = api_response$batSide$description, throws = api_response$pitchHand$description) # get description from nested data frame

## school - green
## use college if available
if("colleges" %in% names(api_response$education)){ 
  school <- api_response %>% 
    select(id) %>%
    bind_cols(
      api_response$education$colleges[[1]] %>% 
      filter(row_number() == n()) %>% ## get the last college attended if multiple
        rename(school_name = name)
    )
} else {
  school <- api_response %>% 
    select(id) %>%
    bind_cols(
      api_response$education$highschools[[1]] %>%
        filter(row_number() == n()) %>%
        rename(school_name = name)
    )
}

## position - blue
position_info <- api_response %>%
  select(id) %>%
  bind_cols(
    position = api_response$primaryPosition$abbreviation
  )

## stats - dark red

# get stats dataframe from response
stats <- data.frame(id = api_response$id, api_response$stats[[1]]$splits[[1]])

# add derived stats (bb_rate, so_rate, iso, woba)
stats_derived <- stats %>% 
  mutate(bb_rate = stats$stat$baseOnBalls / stats$stat$plateAppearances,
         so_rate = stats$stat$strikeOuts / stats$stat$plateAppearances,
         iso = ifelse(stats$stat$avg == ".---", "0", as.numeric(stats$stat$slg) - as.numeric(stats$stat$avg)),
         woba = (
            (.69 * stats$stat$baseOnBalls) + 
            (.72 * stats$stat$hitByPitch) + 
            (.87 * (stats$stat$hits - stats$stat$doubles - stats$stat$triples - stats$stat$homeRuns)) + 
            (1.22 * stats$stat$doubles) +
            (1.53 * stats$stat$triples) +
            (1.94 * stats$stat$homeRuns)) 
            /
            (stats$stat$atBats + stats$stat$baseOnBalls + stats$stat$sacFlies + stats$stat$hitByPitch - stats$stat$intentionalWalks)
         )

## select only needed columns
stats_final <- stats_derived %>%
  transmute(
    mlbamid = id,
    year = season, 
    team = stats$team$name,
    PA = stats$stat$plateAppearances,
    R = stats$stat$runs,
    RBI = stats$stat$rbi,
    HR = stats$stat$homeRuns,
    BB_pct = bb_rate,
    K_pct = so_rate,
    BABIP = stats$stat$babip,
    ISO = iso,
    AVG = stats$stat$avg,
    OBP = stats$stat$obp,
    SLG = stats$stat$slg)

## awards
awards_all <- data.frame(mlbamid = api_response$id, name =  api_response$awards[[1]]$name, season = api_response$awards[[1]]$season, stringsAsFactors = FALSE)

awards <- awards_all %>%
  filter(name %in% c("Rawlings AL Gold Glove",
                     "Rawlings NL Gold Glove",
                     "AL Rookie of the Year",
                     "NL Rookie of the Year",
                     "AL All-Star",
                     "NL All-Star",
                     "AL MVP",
                     "NL MVP",
                     "AL Silver Slugger",
                     "NL Silver Slugger"
                     )
         )

## teams
logo_base_url <- "https://www.mlbstatic.com/team-logos/"
teams <- api_response$stats[[1]]$splits[[1]]$team %>%
  distinct() %>%
  transmute(teamid = id,
            team_name = name,
            logo_url = file.path("https://www.mlbstatic.com/team-logos", paste0(as.character(id), ".svg")),
            current = ifelse(row_number() == 1, TRUE, FALSE)
  )

## draft info
## make new api call to get draft info
## draft API also has headshot links

endpoint = "draft/prospects"
path = api_response$draftYear
params = data.frame(key = c("playerId", "fields" ),
                    value = c("595281", "prospects,pickRound,pickNumber,headshotLink,person,id,team,abbreviation"),
                    stringsAsFactors = FALSE
)

draft_api_response <- statsapi_action_get(endpoint = endpoint, url_path = path, params = params)$prospects

## get draft info
draft_info <- data.frame(mlbamid = draft_api_response$person$id, 
                         headshot = draft_api_response$headshotLink,
                         draftTeam = draft_api_response$team$abbreviation,
                         draftYear = api_response$draftYear,
                         draftRound = draft_api_response$pickRound,
                         draftPick = draft_api_response$pickNumber, 
                         stringsAsFactors = FALSE)


## combine 1 observation per card stats
person <- left_join(bio_info, draft_info, by=c("id"="mlbamid")) %>%
  left_join(hand_info, by=c("id"="id")) %>%
  left_join(school, by=c("id"="id")) %>%
  left_join(position_info, by=c("id"="id"))
  
## write csvs
write.csv(person, file = "person.csv", row.names = FALSE)
write.csv(stats,  file = "stats.csv", row.names = FALSE)
write.csv(awards,  file = "awards.csv", row.names = FALSE)
write.csv(teams,  file = "teams.csv", row.names = FALSE)

