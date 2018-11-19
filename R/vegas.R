#' Fetch current Vegas odds
#' @export
#' @examples
#'
#' current_probs()
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

  # fit logistic regression
  historical_teams <- Lahman::Teams %>%
    filter(yearID >= 1998, W + L > 150) %>%
    mutate(wpct = W / (W + L))
  mod <- stats::glm(WSWin == "Y" ~ wpct, data = historical_teams, family = binomial)
  coefs <- coef(mod)

  teams %>%
    dplyr::mutate(logit = gtools::logit(ws_prob),
                  wpct_hat = (logit - coefs["(Intercept)"]) / coefs["wpct"],
                  wpct_hat_rescale = 0.5 + wpct_hat - mean(wpct_hat),
                  wins_pred = 162 * wpct_hat_rescale) %>%
    dplyr::select(Team, Odds, ws_prob, wins_pred)
}
