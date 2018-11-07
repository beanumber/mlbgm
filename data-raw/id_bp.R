library(tidyverse)

downloader::download("https://www.baseballprospectus.com/sortable/playerids/playerid_list.csv",
                    destfile = here::here("data-raw", "playerid_list.csv"))
id_bp <- read_csv(here::here("data-raw", "playerid_list.csv"))

names(id_bp) <- c("nameLast", "nameFirst", "bpId", "davenportId", "mlbamId", "retroId")

# Merge with Lahman::Master
# ids <- merge(x = Lahman::Master, y = bp.ids, by.x = "retroID", by.y = "retroId", all = TRUE)

# idBP <- as.tbl(bp.ids) %>%
#  filter(!is.na(mlbamId))

save(id_bp, file = "data/id_bp.rda", compress = "xz")
