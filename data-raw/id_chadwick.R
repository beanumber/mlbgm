##
## Download the Baseball Register from Ted Turocy's site
## http://chadwick-bureau.com/the-register/

library(tidyverse)

url <- "https://github.com/chadwickbureau/register/blob/master/data/people.csv?raw=true"
downloader::download(url, destfile = here::here("data-raw", "people.csv"))

register <- read_csv(here::here("data-raw", "people.csv"))

id_chadwick <- register %>%
  filter(!is.na(key_mlbam)) %>%
  select(key_person, key_mlbam, key_retro, key_bbref,
         key_fangraphs, name_last, name_first, name_given)

# Examine Mike Trout
# filter(idTT, name_last == "Trout")
# save it to the data folder
save(id_chadwick, file = "data/id_chadwick.rda", compress = "xz")
