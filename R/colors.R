#' MLB Team colors
#' @export
#' @param which Integer giving the set of colors you want. Default is 1 for
#' "primary".
#' @param names The quoted name of the column of the tibble returned by
#' \code{\link{lkup_teams}} that you want to use as the names of the colors.
#' This will need to match the data in the variable you want to map to color.
#' @seealso \code{\link[teamcolors]{team_pal}},
#' \code{\link[teamcolors]{scale_fill_teams}},
#' \code{\link[teamcolors]{scale_color_teams}}
#' @examples
#' mlb_pal() != mlb_pal(2)
#'
#' mlb_pal(names = "lahman_name")
#' mlb_pal(names = "teamcolors_name")
#' mlb_pal(2, names = "city")

mlb_pal <- function(which = 1, names = "teamID") {
  x <- lkup_teams()
  z <- which(names(x) == "primary")
  out <- dplyr::pull(x, z - 1 + which)
  names(out) <- pull(x, names)
  out
}

#' @rdname mlb_pal
#' @inheritDotParams ggplot2::scale_fill_manual
#' @export

scale_fill_mlb <- function(which = 1, names = "teamID", ...) {
  ggplot2::scale_fill_manual(..., values = mlb_pal(which = which, names = names))
}

#' @rdname mlb_pal
#' @export

scale_color_mlb <- function(which = 1, names = "teamID", ...) {
  ggplot2::scale_color_manual(..., values = mlb_pal(which = which, names = names))
}

