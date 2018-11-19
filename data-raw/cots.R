
url <- "https://docs.google.com/spreadsheets/d/1m9ap5cOX3j4ZYnmceOZ0oK8GtLg5YGesNsxMZb6GFIs/pubhtml#"

library(tidyverse)
library(googlesheets)

cots <- gs_key("1m9ap5cOX3j4ZYnmceOZ0oK8GtLg5YGesNsxMZb6GFIs")

cots_read <- function(ss, ws = 1) {
  mls_year <- gs_read(ss, ws = ws)
  if ("Pos'n" %in% names(mls_year)) {
    mls_year <- rename(mls_year, Pos = `Pos'n`)
  }
  mls_col <- grep("MLS", names(mls_year))
  names(mls_year)[mls_col] <- "mls"
  mls_year %>%
    mutate(Year = parse_number(ws)) %>%
    select(Player, Pos, mls, Year)
}

mls <- cots %>%
  gs_ws_ls() %>%
  map_df(cots_read, ss = cots)

mls <- mls %>%
  filter(Year == 2017) %>%
  left_join(Lahman::Master %>%
               mutate(Player = paste0(nameLast, ", ", nameFirst)) %>%
               select(Player, playerID),
             by = "Player")

save(mls, file = "data/mls.rda", compress = "xz")

