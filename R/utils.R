globalVariables(c("standardized_name", "lahman_name", "league"))

#' Utilities for dealing with MLB Teams
#' @import dplyr
#' @return a \code{\link[tibble]{tibble}} with one row for each team
#' @export

lkup_teams <- function() {
  out <- Lahman::Teams %>%
    filter(yearID == max(yearID)) %>%
    select(teamID, lahman_name = name, franchID, teamIDBR, teamIDretro) %>%
    mutate(teamID = as.character(teamID),
           franchID = as.character(franchID),
           standardized_name = standardize_team_name(x = lahman_name),
           teamID_alt = case_when(
             teamID == "CHA" ~ "CWS",
             teamID == "KCA" ~ "KC",
             teamID == "MIA" ~ "FLO",
             teamID == "SDN" ~ "SD",
             teamID == "SFN" ~ "SF",
             teamID == "TBA" ~ "TB",
             teamID == "WAS" ~ "WSH",
             TRUE ~ teamID)
           ) %>%
    tidyr::separate(col = standardized_name, into = c("city", "nickname"), sep = " ", remove = FALSE) %>%
    left_join(mutate(teamcolors::teamcolors, standardized_name = standardize_team_name(name)), by = "standardized_name") %>%
    rename(teamcolors_name = name) %>%
    select(-league) %>%
    tibble::as_tibble()
  out
}

#' @rdname lkup_teams
#' @param x character vector of team names
#' @param ... currently ignored
#' @export

standardize_team_name <- function(x, ...) {
  x %>%
    tolower() %>%
    gsub(" of anaheim", "", .) %>%
    gsub("florida", "miami", .) %>%
    gsub("anaheim", "los-angeles", .) %>%
    gsub("montreal", "washington", .) %>%
    gsub("expos", "nationals", .) %>%
    gsub("st\\. ", "st-", .) %>%
    gsub("st ", "st-", .) %>%
    gsub("ny ", "new-york ", .) %>%
    gsub("la ", "los-angeles ", .) %>%
    gsub("chi ", "chicago ", .) %>%
    gsub("san ", "san-", .) %>%
    gsub(" sox", "-sox", .) %>%
    gsub(" jays", "-jays", .) %>%
    gsub("new york", "new-york", .) %>%
    gsub("los angeles", "los-angeles", .) %>%
    gsub(" city", "-city", .) %>%
    gsub(" bay", "-bay", .)
}

#' @rdname lkup_teams
#' @export

standardize_team_ids <- function(x) {
  lkup <- lkup_teams()

  out <- tibble::enframe(trimws(x)) %>%
    left_join(select(lkup, teamID, standardized_name), by = c("value" = "teamID")) %>%
    left_join(select(lkup, teamID, teamIDBR), by = c("value" = "teamIDBR")) %>%
    left_join(select(lkup, teamID, teamIDretro), by = c("value" = "teamIDretro")) %>%
    left_join(select(lkup, teamID, teamID_alt), by = c("value" = "teamID_alt")) %>%
    mutate(teamID = ifelse(is.na(teamID), teamID.x, teamID),
           teamID = ifelse(is.na(teamID), teamID.y, teamID))
  return(out$teamID)
}
