#' MLB Team colors
#' @export
#' @param ... arguments passed to \code{\link[teamcolors]{league_palette}}
#' @examples
#' mlb_palette() != mlb_palette(2)

mlb_palette <- function(...) {
  teamcolors::league_palette("mlb", ...)
}
