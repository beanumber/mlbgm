library(tidyverse)
library(rvest)

url <- "https://www.spotrac.com/mlb/contracts/"


x <- xml2::read_html(url) %>%
  html_nodes("table") %>%
  html_table() %>%
  purrr::pluck(1)

contracts <- x %>%
  rename(End = `Free Agent`) %>%
  mutate(
    Dollars = parse_number(Dollars),
    AAV = parse_number(Average),
    Year_1 = End - Yrs,
    Year_N = End - 1,
    teamID = stringr::str_sub(Team, 1, 3),
    teamID = standardize_team_ids(teamID)
  ) %>%
  select(-Average) %>%
  filter(End > 0) %>%
  as_tibble()

save(contracts, file = "data/contracts.rda", compress = "xz")
