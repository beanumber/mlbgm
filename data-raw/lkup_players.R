library(tidyverse)


# Baseball Prospectus
downloader::download("https://www.baseballprospectus.com/sortable/playerids/playerid_list.csv",
                    destfile = here::here("data-raw", "playerid_list.csv"))
id_bp <- read_csv(here::here("data-raw", "playerid_list.csv"))

names(id_bp) <- c("name_last", "name_first", "bp_id", "davenport_id", "mlbam_id", "retro_id")

# save(id_bp, file = "data/id_bp.rda", compress = "xz")

## Download the Baseball Register from Ted Turocy's site
## http://chadwick-bureau.com/the-register/

url <- "https://github.com/chadwickbureau/register/blob/master/data/people.csv?raw=true"
downloader::download(url, destfile = here::here("data-raw", "people.csv"))

register <- read_csv(here::here("data-raw", "people.csv"))

id_chadwick <- register %>%
  filter(!is.na(key_mlbam)) %>%
  select(name_last, name_first, name_given,
         mlbam_id = key_mlbam,
#         retroId = key_retro,
         lahman_id = key_bbref,
         fg_id = key_fangraphs)

# save(id_chadwick, file = "data/id_chadwick.rda", compress = "xz")

# Crunchtime Baseball
crunchtime <- read_csv("http://crunchtimebaseball.com/master.csv")


# lkup_players

lkup_players <- id_bp %>%
  full_join(id_chadwick, by = "mlbam_id") %>%
  full_join(
    select(crunchtime, contains("_id"), -fg_id, -bp_id, -bref_id, -lahman_id, -retro_id),
    by = c("mlbam_id" = "mlb_id")
  ) %>%
  mutate(
    name_last = ifelse(is.na(name_last.x), name_last.y, name_last.x),
    name_first = ifelse(is.na(name_first.x), name_first.y, name_first.x)
  ) %>%
  select(-matches(".(x|y)")) %>%
  select(name_last, name_first, name_given,
         mlbam_id, lahman_id, retro_id, everything())


lkup_players %>%
  summarize(
    N = n(),
    mlbam = n_distinct(mlbam_id),
    lahman = n_distinct(lahman_id),
    retro = n_distinct(retro_id),
    espn = n_distinct(espn_id)
  )


save(lkup_players, file = "data/lkup_players.rda", compress = "xz")
