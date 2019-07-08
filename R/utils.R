#' MLB Team colors
#' @export
#' @param ... arguments passed to \code{\link[teamcolors]{league_palette}}
#' @examples
#' mlb_palette() != mlb_palette(2)

mlb_palette <- function(...) {
  teamcolors::league_palette("mlb", ...)
}

#' @export
#' @rdname mlb_palette

mlb_lahman_palette <- function(...) {
  pal <- teamcolors::league_palette("mlb", ...) %>%
    tibble::enframe() %>%
    mutate(name = standardize_team_name(name))
  lkup <- Lahman::Teams %>%
    filter(yearID == max(yearID)) %>%
    select(teamID, name) %>%
    mutate(name = standardize_team_name(name))
  merged <- left_join(pal, lkup)
  out <- merged$value
  names(out) <- merged$teamID
  out
}



#' Match names
#' @import dplyr
#' @export

lahman_teams <- function() {
  out <- Lahman::Teams %>%
    filter(yearID == max(yearID)) %>%
    select(teamID, name) %>%
    mutate(teamID = as.character(teamID),
           team = standardize_team_name(x = name)) %>%
    tidyr::separate(col = team, into = c("city", "nickname"), sep = " ") %>%
    mutate(canonical_name = paste(city, nickname)) %>%
    tibble::as_tibble()
  out
}

#' @rdname lahman_teams
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

#' @rdname lahman_teams
#' @export

mlb_teams <- function() {
  Lahman::Teams %>%
    filter(yearID == 2017) %>%
    mutate(teamID = as.character(teamID)) %>%
    select(name, teamID, franchID, teamIDBR, teamIDretro) %>%
    mutate(teamID_alt = case_when(
      teamID == "CHA" ~ "CWS",
      teamID == "KCA" ~ "KC",
      teamID == "MIA" ~ "FLO",
      teamID == "SDN" ~ "SD",
      teamID == "SFN" ~ "SF",
      teamID == "TBA" ~ "TB",
      teamID == "WAS" ~ "WSH",
      TRUE ~ teamID),
      canonical_name = standardize_team_name(name)
    )
}

#' @rdname lahman_teams
#' @export

standardize_team_ids <- function (x) {
  lkup <- mlb_teams()

  out <- tibble::enframe(trimws(x)) %>%
    left_join(select(lkup, teamID, canonical_name), by = c("value" = "teamID")) %>%
    left_join(select(lkup, teamID, teamIDBR), by = c("value" = "teamIDBR")) %>%
    left_join(select(lkup, teamID, teamIDretro), by = c("value" = "teamIDretro")) %>%
    left_join(select(lkup, teamID, teamID_alt), by = c("value" = "teamID_alt")) %>%
    mutate(teamID = ifelse(is.na(teamID), teamID.x, teamID),
           teamID = ifelse(is.na(teamID), teamID.y, teamID))
  return(out$teamID)
}
