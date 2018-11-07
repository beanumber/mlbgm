##
## Download fWAR as CSV files
##
## Need to manually to to their website and "Save to Excel..."
## This assumes you have those files stored locally already
# https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=6&season=2018&month=0&season1=1871&ind=1&team=0&rost=0&age=0&filter=&players=0

library(tidyverse)

bat <- read_csv(here::here("data-raw", "FanGraphs_1871-2014_batting.csv"))
pitch <- read_csv(here::here("data-raw", "FanGraphs_1871-2014_pitching.csv"))
out <- full_join(bat, pitch, by = c("playerid", "Season"))

fwar <- out %>%
  mutate(Name = ifelse(is.na(Name.x), as.character(Name.y), as.character(Name.x)), 
         yearId = Season, 
         fRAA_bat = ifelse(is.na(Batting), 0, Batting) + ifelse(is.na(Positional), 0, Positional), 
         fRAA_br = ifelse(is.na(`Base Running`), 0, `Base Running`), 
         fRAA_field = ifelse(is.na(Fielding), 0, Fielding), 
         fWAR_pitch = ifelse(is.na(WAR.y), 0, WAR.y), 
         fRepl = ifelse(is.na(Replacement), 0, -Replacement), 
         fRAR = ifelse(is.na(RAR.x), 0, RAR.x) + ifelse(is.na(RAR.y), 0, RAR.y), 
         fRAA = fRAR + fRepl, 
         fWAR = ifelse(is.na(WAR.x), 0, WAR.x) + fWAR_pitch) %>%
  select(playerid, yearId, Name, fWAR, fRAA_bat, fRAA_br, fRAA_field
         , fWAR_pitch, fRAR, fRAA, fRepl)

save(fwar, file = "data/fwar.rda", compress = "xz")
