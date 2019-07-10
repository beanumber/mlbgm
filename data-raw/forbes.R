# Forbes franchise values
library(tidyverse)
library(here)

# https://umich.app.box.com/s/41707f0b2619c0107b8b/folder/320021767

forbes <- read_csv(here("data-raw", "forbes.csv")) %>%
  mutate(Team = gsub(" of Anaheim", "", Team),
         Team = gsub("Florida Marlins", "Miami Marlins", Team)) %>%
  left_join(select(lkup_teams(), teamcolors_name, teamID), by = c("Team" = "teamcolors_name")) %>%
  arrange(desc(Year), desc(Value))

save(forbes, file = "data/forbes.rda", compress = "xz")

# Compare to

require(foreign)
d <- read.dta(here("data-raw", "2011-07-29.dta"))

franchises <- d %>%
  select(t_id, club, team, year = yr, revenue, metropop, percapinc, est500)


# Older stuff

rev_old <- readxl::read_excel(here("data-raw/MLBhistorical.xls"),
                        sheet = "MLB - Revenue", range = "B4:O34") %>%
  gather(key = "Year", value = "Revenue", -Team) %>%
  mutate(Year = parse_number(Year),
         Team = gsub("Anaheim Angels", "Los Angeles Angels", Team),
         Team = gsub("Florida Marlins", "Miami Marlins", Team),
         Team = gsub("Montreal Expos", "Washington Nationals", Team))

value_old <- readxl::read_excel(here("data-raw/MLBhistorical.xls"),
                              sheet = "MLB - Valuation", range = "B4:O34") %>%
  gather(key = "Year", value = "Valuation", -Team) %>%
  mutate(Year = parse_number(Year),
         Team = gsub(" of Anaheim", "", Team),
         Team = gsub("Florida Marlins", "Miami Marlins", Team))

forbes_old <- rev_old %>%
  inner_join(value_old, by = c("Team", "Year"))


