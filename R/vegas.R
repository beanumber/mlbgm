#' Fetch current Vegas futures and collect them
#' @name futures
#' @export
#' @examples
#' \dontrun{
#' read_ws_probs()
#' }

read_ws_probs <- function() {
  # collect futures
  a <- read_ws_futures_actionnetwork()
#  b <- read_ws_futures_oddsshark()
  c <- read_ws_futures_sportsoddshistory()
  d <- read_ws_futures_vegasinsider()

  combined <- bind_rows(a, c, d) %>%
    mutate(canonical_name = standardize_team_name(team)) %>%
    left_join(select(lahman_teams(), canonical_name, teamID), by = c("canonical_name")) %>%
    left_join(select(lahman_teams(), city, teamID), by = c("canonical_name" = "city")) %>%
    mutate(teamID = ifelse(is.na(teamID.x), teamID.y, teamID.x)) %>%
    select(-teamID.x, -teamID.y) %>%
    filter(future > 0)

  teams <- combined %>%
    group_by(teamID) %>%
    summarize(
      timestamp = last(timestamp),
      num_futures = n(),
      mean_future = mean(future),
      sd_future = stats::sd(future)
    ) %>%
    arrange(mean_future) %>%
    mutate(ws_prob = 100 / (100 + mean_future),
           ws_prob_normalized = ws_prob / sum(ws_prob))

  # fit logistic regression
  mod <- glm_ws()
  preds <- predict_wins(mod, teams$ws_prob_normalized) %>%
    dplyr::select(wins_pred)

  teams$wins_est <- preds$wins_pred

  return(teams)

}

#' @rdname futures
#' @export
#' @source \url{http://www.vegasinsider.com/mlb/odds/futures/}

read_ws_futures_vegasinsider <- function() {
  url <- "http://www.vegasinsider.com/mlb/odds/futures/"
  x <- xml2::read_html(url) %>%
    rvest::html_nodes("table")
  teams <- x[[9]] %>%
    rvest::html_table() %>%
    bind_cols(stringr::str_split_fixed(.$Odds, pattern = "/", n = 2) %>%
                tibble::as_tibble()) %>%
    mutate(
      decimal_odds = readr::parse_number(V1) / readr::parse_number(V2),
      future = decimal_odds * 100,
      timestamp = Sys.time(),
      sportsbook = "VegasInsider"
    ) %>%
    tibble::as_tibble() %>%
    select(team = Team, sportsbook, future, timestamp)
  return(teams)
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
#' @source \url{https://www.oddsshark.com/mlb/odds/futures}

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
    dplyr::mutate(timestamp = Sys.time()) %>%
    tibble::as_tibble()

  return(out)
}

#' @rdname futures
#' @importFrom rvest html_nodes html_table
#' @export
#' @source \url{https://www.actionnetwork.com/mlb/future}

read_ws_futures_actionnetwork <- function() {
  url <- "https://www.actionnetwork.com/mlb/futures"

  x <- xml2::read_html(url)
  y <- x %>%
    html_nodes("table") %>%
    html_table() %>%
    purrr::pluck(2)
  y %>%
    mutate(sportsbook = "ActionNetwork",
           timestamp = Sys.time()) %>%
    select(team = Team, sportsbook, future = Odds, timestamp) %>%
    tibble::as_tibble()
}


#' @rdname futures
#' @importFrom rvest html_nodes html_table html_node html_attr
#' @export
#' @source \url{https://www.sportsline.com/mlb/futures/}

read_ws_futures_sportsline <- function() {
  url <- "https://www.sportsline.com/mlb/futures/"

  x <- xml2::read_html(url)
  y <- x %>%
    html_nodes("ember842") %>%
    html_table() %>%
    purrr::pluck(1)
  y %>%
    mutate(sportsbook = "SportsLine",
           timestamp = Sys.time()) %>%
    select(team = Team, sportsbook, future = BetOnline, timestamp) %>%
    tibble::as_tibble()
}

#' @rdname futures
#' @export
#' @source \url{https://www.sportsoddshistory.com/mlb-odds/live-world-series-odds/}

read_ws_futures_sportsoddshistory <- function() {
  url <- "https://www.sportsoddshistory.com/mlb-odds/live-world-series-odds/"

  x <- xml2::read_html(url)
  y <- x %>%
    html_nodes(xpath = "/html/body/div/div/div[5]/div/table")

  header <- y %>%
    html_node("thead") %>%
    html_nodes("img") %>%
    html_attr("src")


    # as.character() %>%
    # stringr::str_extract_all("<!-- .+ column header -->") %>%
    # purrr::pluck(1) %>%
    # gsub("<!--", "", .) %>%
    # gsub(" column header -->", "", .) %>%
    # trimws()

  data <- y %>%
    html_table() %>%
    purrr::pluck(1)

  if (ncol(data) - 1 != length(header)) {
    stop("number of columns does not match number of sportsbooks")
  } else {
    names(data)[2:ncol(data)] <- header
  }

  data %>%
    tidyr::gather(key = "sportsbook", value = "future", -Team) %>%
    filter(!is.na(future)) %>%
    tibble::as_tibble() %>%
    mutate(timestamp = Sys.time()) %>%
    select(team = Team, sportsbook, future, timestamp)
}
