#' MLB Team colors
#' @export
#' @param ... arguments passed to \code{\link[teamcolors]{league_palette}}
#' @examples
#' mlb_palette() != mlb_palette(2)

mlb_palette <- function(...) {
  teamcolors::league_palette("mlb", ...)
}


#' Match names
#' @export

lahman_teams <- function(x, ...) {
  lahman_names <- Lahman::Teams %>%
    filter(yearID == max(yearID)) %>%
    select(teamID, name) %>%
    mutate(team = standardize_team_name(x = name))
}

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
    gsub("ny ", "new-york ", .) %>%
    gsub("la ", "los-angeles ", .) %>%
    gsub("chi ", "chicago ", .) %>%
    gsub("san ", "san-", .) %>%
    gsub(" sox", "-sox", .) %>%
    gsub("new york", "new-york", .) %>%
    gsub("los angeles", "los-angeles", .) %>%
    gsub(" city", "-city", .) %>%
    gsub(" bay", "-bay", .)
}
