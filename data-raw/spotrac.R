library(tidyverse)
library(rvest)

url <- "https://www.spotrac.com/mlb/contracts/"


x <- xml2::read_html(url) %>%
  html_nodes("table") %>%
  html_table()

contracts <- x[[1]] %>%
  rename(End = `Free Agent`) %>%
  mutate(Age = parse_number(Age),
         Yrs = parse_number(Yrs),
         Dollars = parse_number(Dollars),
         AAV = parse_number(Average),
         Year_1 = End - Yrs,
         Year_N = End - 1) %>%
  select(-Average) %>%
  filter(End > 0)

save(contracts, file = "data/contracts.rda", compress = "xz")
