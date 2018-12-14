library(tidyverse)
library(rvest)
library(here)
#
# url <- "https://en.wikipedia.org/wiki/List_of_United_States_metropolitan_areas_by_per_capita_income"
# x <- read_html(url) %>%
#   html_nodes("table") %>%
#   purrr::pluck(3) %>%
#   html_table()
#
# msa <- x %>%
#   mutate(msa_pop = parse_number(Population),
#          msa_income = parse_number(`Per capitaincome`))


# Population, 2010-2017

msa_pop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/metro/totals/cbsa-est2017-alldata.csv") %>%
  filter(grepl("Metropolitan Statistical Area", LSAD)) %>%
  select(CBSA, NAME, contains("POPESTIMATE")) %>%
  gather(key = "variable", value = "msa_pop", -CBSA, -NAME) %>%
  mutate(year = parse_number(variable))


# Firm size, 2012-2014

# download.file("https://www.sba.gov/sites/default/files/advocacy/static_stmsa_14.xls")
# xls <- readxl::read_excel(file.path("static_stmsa_14.xls"), sheet = "msa_totals_2012_2014")
xls_new <- read_csv(here("data-raw", "static_stmsa_14.csv")) %>%
  rename(firm_size = 4, msa_code = 3) %>%
  replace_na(list(Year = 9999, msa_code = 0)) %>%
  mutate(the_year = cummin(Year)) %>%
  split(.$the_year) %>%
  map_df(~mutate(.x, MSA = cummax(msa_code)))

firm_sizes_new <- xls_new %>%
  select(the_year, MSA, firm_size, firms) %>%
  group_by(the_year, MSA) %>%
  spread(key = firm_size, value = firms)

msa_lkup_new <- xls_new %>%
  filter(firm_size == "Total") %>%
  select(the_year, MSA, name = MSA_Desc)

# Firm size, 1993-2011

xls_old <- read_csv(here("data-raw", "static_stmsa_14_1993-2011.csv")) %>%
  rename(firm_size = 4, msa_code = 3) %>%
  replace_na(list(Year = 9999, msa_code = 0)) %>%
  mutate(the_year = cummin(Year)) %>%
  split(.$the_year) %>%
  map_df(~mutate(.x, MSA = cummax(msa_code)))

msa_reps <- xls_old %>%
  group_by(the_year) %>%
  summarize(N = n(), msas = n_distinct(msa_code),
            sizes = n_distinct(firm_size))

unique_nonzero <- function(x) {
  y <- unique(x)
  y[y > 0]
}

xls_old$MSA <- xls_old %>%
  split(.$the_year) %>%
  map2(.y = msa_reps$sizes, ~rep(unique_nonzero(.x$msa_code), each = .y)) %>%
  reduce(c)

firm_sizes_old <- xls_old %>%
  select(the_year, MSA, firm_size, firms) %>%
  group_by(the_year, MSA) %>%
  spread(key = firm_size, value = firms)
names(firm_sizes_old) <- names(firm_sizes_old) %>%
  gsub("^E", "", x = .) %>%
  gsub("_", "-", x = .) %>%
  gsub("the-year", "the_year", x = .)


msa_lkup_old <- xls_old %>%
  filter(firm_size == "Total") %>%
  select(the_year, MSA, name = MSA_Desc)

# Combine

msa_firms <- bind_rows(firm_sizes_new, firm_sizes_old)
msa_lkup <- bind_rows(msa_lkup_new, msa_lkup_old)


msa <- msa_firms %>%
  select(the_year, MSA, `100-499`, `500+`) %>%
  inner_join(msa_pop, by = c("the_year" = "year", "MSA" = "CBSA")) %>%
  select(-variable)

save(msa, file = "data/msa.rda", compress = "xz")

msa_mlb <- read_csv(here("data-raw", "msa_mlb.csv"))

save(msa_mlb, file = "data/msa_mlb.rda", compress = "xz")
