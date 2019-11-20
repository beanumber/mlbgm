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

#' Running a linear model on data frame splits
#' @importFrom broom glance
#' @importFrom purrr map map_dfr
#' @param data Dataset for modeling
#' @param form Formula to be passed to the linear model
#' @param i Number of cuts to split the data into
#' @param var_name Variable to base the cuts on
#' @param equal_width If TRUE, cuts the data based on even widths of value; if FALSE, each cut will have an approximately equal number of observations
#' @param ... Passes lm() parameters to allow user to handle NA values
#' @examples lm_split(comps_hypercube, "rWAR ~ cum_rwar",i = 4, "cum_rwar", na.omit = TRUE)
#' @export

lm_split <- function(data, form, i, var_name, equal_width = TRUE, ...) {
  if (equal_width == TRUE) {
    #calculates splits for even variable bin width
    data$splits <- cut(data[[var_name]], i, include.lowest = TRUE)
  } else {
    #calculates splits for even number of obs
    data$splits <- cut_number(data[[var_name]], i)
  }

  #splits data frame based on new split variable
  splitframe <- split(data, data[["splits"]])

  #runs the model on the list of split data frames
  mapframe <- map(splitframe, lm, formula = form)

  #creates model summaries for each data frame and adds a variable to show splits
  glanceframe <- mapframe %>%
    map_dfr(glance, .id = paste(var_name, "splits", sep = "_"))
  return(glanceframe)
}
