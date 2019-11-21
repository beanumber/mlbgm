#' Build a data frame suitable for projecting WAR
#' @export
#' @param year Predict until this year
#' @param ... Can pass logicals for the filter function
#' @examples
#' \dontrun{
#' test <- war_frame(1982)
#' }

war_frame <- function(year = max(Lahman::Batting$yearID), ...) {
  comps_hypercube %>%
    filter(yearID <= year)
}

#' @rdname war_frame
#' @export

war_frame_pitchers <- function(year = max(Lahman::Pitching$yearID), ...) {
  comps_hypercube %>%
    filter(yearID <= year, cum_PA < cum_BFP)
}

#' @rdname war_frame
#' @export

war_frame_batters <- function(year = max(Lahman::Batting$yearID), ...) {
  comps_hypercube %>%
    filter(yearID <= year, cum_PA >= cum_BFP)
}

