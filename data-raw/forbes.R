# Forbes franchise values
library(tidyverse)
library(here)

# https://umich.app.box.com/s/41707f0b2619c0107b8b/folder/320021767

forbes <- read_csv(here("data-raw", "forbes.csv")) %>%
  mutate(Team = gsub(" of Anaheim", "", Team),
         Team = gsub("Florida Marlins", "Miami Marlins", Team))

save(forbes, file = "data/forbes.rda", compress = "xz")

# Compare to

require(foreign)
d <- read.dta(here("data-raw", "2011-07-29.dta"))

franchises <- d %>%
  select(t_id, club, team, year = yr, revenue, metropop, percapinc, est500)

