#' Fetch current Vegas odds
#' @name futures
#' @export

current_probs <- function() {
  url <- "http://www.vegasinsider.com/mlb/odds/futures/"
  x <- xml2::read_html(url) %>%
    rvest::html_nodes("table")
  teams <- x[[9]] %>%
    rvest::html_table() %>%
    dplyr::bind_cols(stringr::str_split_fixed(.$Odds, pattern = "/", n = 2) %>%
                       as_tibble()) %>%
    dplyr::mutate(ws_vig = readr::parse_number(V2) /
                    (readr::parse_number(V1) + readr::parse_number(V2)),
                  ws_prob = ws_vig / sum(ws_vig))

  # fit logistic regression
  mod <- glm_ws()
  preds <- predict_wins(mod, teams$ws_prob) %>%
    dplyr::select(wins_pred)

  dplyr::bind_cols(teams, preds) %>%
    dplyr::select(Team, Odds, ws_prob, wins_pred)
}


#' @rdname futures
#' @param from Year from which to draw historical data
#' @importFrom stats glm binomial
#' @export

glm_ws <- function(from = 1998, ...) {
  historical_teams <- Lahman::Teams %>%
    dplyr::filter(yearID >= 1998, W + L > 150) %>%
    dplyr::mutate(wpct = W / (W + L))
  glm(WSWin == "Y" ~ wpct, data = historical_teams, family = binomial)
}

#' @rdname futures
#' @param mod a \code{\link[stats]{glm}} object
#' @param probs a vector of probabilities
#' @param ... currently ignored
#' @export

predict_wins <- function(mod, probs, ...) {
  coefs <- coef(mod)

  tibble::tibble(ws_prob = probs) %>%
    dplyr::mutate(logit = gtools::logit(ws_prob),
                  wpct_hat = (logit - coefs["(Intercept)"]) / coefs["wpct"],
                  wpct_hat_rescale = 0.5 + wpct_hat - mean(wpct_hat),
                  wins_pred = 162 * wpct_hat_rescale)
}

#' @rdname futures
#' @importFrom rvest html_nodes html_text html_children html_attr
#' @export

read_ws_futures_oddsshark <- function() {
  url <- "https://www.oddsshark.com/mlb/odds/futures"

  num_books <- 9
  idx <- 2:(num_books * 30 + 1)



  x <- xml2::read_html(url)
  y <- x %>%
    html_nodes(css = "#op-future-results") %>%
    html_text() %>%
    stringr::str_split(pattern = "\\+") %>%
    purrr::pluck(1) %>%
    readr::parse_number()

  book_names <- x %>%
    html_nodes(css = "div.op-book-header") %>%
    html_nodes("img") %>%
    html_attr(name = "alt")

  out <- y[idx] %>%
    matrix(ncol = num_books, byrow = TRUE) %>%
    tibble::as_tibble()
  names(out) <- book_names[c(1:5, 7, 9, 11, 12)]

  teams <- x %>%
    html_nodes(css = "div.op-team-data-wrapper:nth-child(2)") %>%
    html_children() %>%
    html_text(trim = TRUE)

  out <- out %>%
    dplyr::mutate(team = teams[2:31]) %>%
    tidyr::gather(key = "sportsbook", value = "future", -team) %>%
    dplyr::mutate(timestamp = Sys.time())

  return(out)
}

#' @rdname futures
#' @importFrom rvest html_nodes html_table
#' @export

read_ws_futures_actionnetwork <- function() {
  url <- "https://www.actionnetwork.com/mlb/futures"

  x <- xml2::read_html(url)
  y <- x %>%
    html_nodes("table") %>%
    html_table() %>%
    purrr::pluck(1)
  y %>%
    mutate(sportsbook = "ActionNetwork",
           timestamp = Sys.time()) %>%
    select(team = Team, sportsbook, future = Odds, timestamp)
}


#' @rdname futures
#' @importFrom rvest html_nodes html_table
#' @export

read_ws_futures_sportsline <- function() {
  url <- "https://www.sportsline.com/mlb/futures/"

  x <- xml2::read_html(url)
  y <- x %>%
    html_nodes("futures-table ember-view") %>%
    html_table() %>%
    purrr::pluck(1)
  y %>%
    mutate(sportsbook = "SportsLine",
           timestamp = Sys.time()) %>%
    select(team = Team, sportsbook, future = BetOnline, timestamp)
}
