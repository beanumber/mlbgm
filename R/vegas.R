#' Fetch current Vegas odds
#' @export
#'
current_probs <- function() {
  url <- "http://www.vegasinsider.com/mlb/odds/futures/"
  x <- xml2::read_html(url) %>%
    rvest::html_nodes("table")
  teams <- x[[9]] %>%
    rvest::html_table() %>%
    dplyr::bind_cols(stringr::str_split_fixed(.$Odds, pattern = "/", n = 2) %>%
                       as.data.frame()) %>%
    dplyr::mutate(ws_vig = readr::parse_number(V2) /
                    (readr::parse_number(V1) + readr::parse_number(V2)),
                  ws_prob = ws_vig / sum(ws_vig))
  teams %>%
    dplyr::select(Team, Odds, ws_prob)
}
