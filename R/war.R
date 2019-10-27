#' Build a data frame suitable for projecting WAR
#' @export
#' @examples
#' \dontrun{
#' test <- war_frame(1982)
#' }

war_frame <- function(year = 2017, ...) {
  comps_hypercube %>%
    filter(yearID < year)
}

