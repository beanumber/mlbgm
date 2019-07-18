globalVariables(c("AB", "BB", "BFP", "HBP", "IBB", "InnOuts", "PA", "POS",
                  "RAA_bat", "RAA_field", "RAA_pitch", "SF", "SH", "salary",
                  "WAR_PA", "age", "birthYear", "comps_hypercube", "cum_BFP",
                  "cum_PA", "decile", "dist", "head", "nameFirst", "nameLast",
                  "playerID", "playerId", "quantile", "rRAA_bat", "rRAA_field",
                  "rRAA_pitch", "rWAR", "rwar", "tTPA", "tWAR", "this_player", "yearId",
                  "value", "age.x", "age.y", "rWAR_hat", "value_hat", "salary_hat",
                  "type", "predicted", "tRAA_bat", "tRAA_field", "mlb_exp"))

#' Modeling future performance
#' @param lahman_id Lahman ID of the player
#' @param horizon Numeric description of the year from which you want to project
#' @param max_dist Maximum distance allowed for nearest neighbors
#' @param max_num Maximum number of neighbors returned
#' @export
#' @examples
#' comps("kiescbr01")
#' comps("kiescbr01", 2000)
#' comps("piazzmi01")
#' comps("piazzmi01", 1995)
#' comps("wrighda03")
#' comps("wrighda03", 2009)


comps <- function(lahman_id,
                  horizon = as.numeric(format(Sys.Date(), "%Y")),
                  max_dist = 500,
                  max_num = 1e4) {

  x <- comps_hypercube %>%
    filter(yearID <= horizon)
  q <- x %>%
    filter(playerID == lahman_id, yearID < horizon) %>%
    arrange(desc(yearID)) %>%
    utils::head(1) %>%
    select(-age) %>%
    left_join(select(Lahman::Master, playerID, birthYear, nameLast, nameFirst), by = "playerID") %>%
    mutate(age = yearID - birthYear) %>%
    select(-birthYear)

  if (nrow(q) < 1) {
    stop(paste(nrow(q), "is too few query points."))
  }

  if (nrow(q) > 1) {
    stop(paste(nrow(q), "is too many query points."))
  }

  comps_df <- x %>%
    # weights are arbitrary!
    mutate(dist = sqrt(10 * (age - q$age)^2 + (cum_PA - q$cum_PA)^2 + (cum_BFP - q$cum_BFP)^2)) %>%
    arrange(dist) %>%
    filter(dist <= max_dist) %>%
    head(max_num)


  out <- list(
    lahman_id = lahman_id,
    horizon = horizon,
    horizon_age = horizon -
      Lahman::Master %>%
      filter(playerID == lahman_id) %>%
      pull(birthYear),
#    hypercube = x,
    query = q,
    comps_df = comps_df,
    comps_universe = comps_df %>%
      pull(playerID) %>%
      unique() %>%
      comps_universe()
  )
  out$mod_rwar <- comps_mod_rwar(out$comps_universe)
  out$mod_salary <- comps_mod_salary(out$comps_universe)
  class(out) <- append("comps", class(out))
  out
}

comps_hypercube_build <- function() {

  bat <- Lahman::Batting %>%
    group_by(playerID, yearID) %>%
    summarize(PA = sum(AB + BB + IBB + HBP + SF + SH))

  pitch <- Lahman::Pitching %>%
    group_by(playerID, yearID) %>%
    summarize(BFP = sum(BFP))

  field <- Lahman::Fielding %>%
    group_by(playerID, yearID, POS) %>%
    summarize(InnOuts = sum(InnOuts)) %>%
    tidyr::spread(key = "POS", value = "InnOuts")

  bat %>%
    left_join(pitch, by = c("playerID", "yearID")) %>%
    left_join(field, by = c("playerID", "yearID")) %>%
    left_join(select(Lahman::Master, playerID, birthYear), by = "playerID") %>%
    mutate(age = yearID - birthYear) %>%
    select(-birthYear) %>%
    tidyr::replace_na(list(PA = 0, BFP = 0)) %>%
    mutate(cum_PA = cumsum(PA),
           cum_BFP = cumsum(BFP)) %>%
    # simplify for now
    select(playerID, yearID, age, cum_PA, cum_BFP)
}

#' @rdname comps
#' @export
#' @examples
#' comps_universe(c("piazzmi01", "wrighda03", "mcgwima01"))

comps_universe <- function(lahman_id) {
  x <- lahman_id %>%
    tibble::enframe(value = "playerId") %>%
    inner_join(rwar, by = "playerId") %>%
    group_by(playerId, yearId) %>%
    summarize(num_stints = n(),
              PA = sum(PA), BFP = sum(BFP, na.rm = TRUE),
              RAA_bat = sum(rRAA_bat), RAA_field = sum(rRAA_field),
              RAA_pitch = sum(rRAA_pitch),
              rWAR = sum(rWAR)) %>%
    group_by(playerId) %>%
    mutate(mlb_exp = rank(yearId)) %>%
    ungroup() %>%
    left_join(
      select(Lahman::Salaries, playerId = playerID, yearId = yearID, salary),
      by = c("playerId", "yearId")
    ) %>%
    left_join(
      select(Lahman::Master, playerId = playerID, birthYear),
      by = "playerId"
    ) %>%
    mutate(age = yearId - birthYear)

  n <- 10
  y <- x %>%
    ungroup() %>%
    group_by(playerId) %>%
    summarize(num_years = n_distinct(yearId), last_year = max(yearId),
              tTPA = sum(PA), tBFP = sum(BFP, na.rm = TRUE),
              tRAA_bat = sum(RAA_bat), tRAA_field = sum(RAA_field),
              tRAA_pitch = sum(RAA_pitch),
              max_age = max(age), tWAR = sum(rWAR)) %>%
    mutate(WAR_PA = tWAR / tTPA,
           quantile = cut(tWAR, include.lowest = TRUE,
                          breaks = stats::quantile(tWAR, probs = 0:n / n))) %>%
    arrange(desc(WAR_PA))

  x %>%
    left_join(select(y, playerId, quantile), by = "playerId")
}

#' @rdname comps
#' @param .data Passed to the data argument of \code{\link[stats]{lm}}
#' @export

comps_mod_rwar <- function(.data) {

  lm(rWAR ~ poly(age, 7) + quantile, data = .data)

}

#' @rdname comps
#' @export

comps_mod_salary <- function(.data) {

  .data %>%
    filter(salary > 0) %>%
    lm(log(salary) ~ factor(mlb_exp) + rWAR + poly(age, 2) + factor(yearId), data = .)

}

#' @rdname comps
#' @param object A \code{\link{comps}} object
#' @param n number of years in advanced to predict. Capped by 24 years of MLB
#' experience.
#' @export
#' @examples
#' predict(comps("beltrca01", 2005))

predict.comps <- function(object, n = 20, ...) {
  max_mlb_exp <- object$mod_salary$xlevels %>%
    purrr::pluck("factor(mlb_exp)") %>%
    readr::parse_integer() %>%
    max()

  tmp <- object$comps_universe %>%
    filter(playerId == object$lahman_id)

  hypothetical <- tibble::tibble(
    playerId = object$lahman_id,
    yearId = object$horizon:(object$horizon + n),
    age = object$horizon_age:(object$horizon_age + n),
    mlb_exp = age - max(tmp$age - tmp$mlb_exp),
    quantile = unique(tmp$quantile)
  ) %>%
    filter(mlb_exp <= max_mlb_exp)

  pred_rwar <- hypothetical %>%
    broom::augment(object$mod_rwar, newdata = .) %>%
    rename(rWAR = .fitted) %>%
    select(-.se.fit)

  pred_salary <- pred_rwar %>%
    augment_future(object$mod_salary, newdata = ., col_name = "yearId") %>%
    mutate(salary_hat = exp(.fitted)) %>%
    rename(rWAR_hat = rWAR) %>%
    select(-.fitted)

  pred_salary
}

#' @rdname comps
#' @param x A \code{comps} object
#' @param horizon_years How many years in advance do you want to forecast?
#' @param ... currently ignored
#' @export
#' @import ggplot2
#' @examples
#' plot_rwar(comps("wrighda03", 2009))

plot_rwar <- function(x, horizon_years = 5, ...) {

  z <- x$comps_universe %>%
    pull(age) %>%
    range()

  ys <- seq(z[1], z[2], by = 1) %>%
    tibble::enframe(value = "age") %>%
    mutate(yearId = age + x$horizon - x$horizon_age) %>%
    filter(yearId %% 5 == 0)

  base_plot <- ggplot(x$comps_universe, aes(x = age, y = rWAR)) +
    geom_jitter(width = 0.1, alpha = 0.1) +
    geom_vline(xintercept = x$horizon_age, color = "dodgerblue", linetype = 2) +
    geom_point(data = filter(x$comps_universe, playerId == x$lahman_id), color = "dodgerblue") +
    geom_line(data = filter(x$comps_universe, playerId == x$lahman_id), color = "dodgerblue") +
    geom_text(data = ys, aes(label = yearId, y = max(x$comps_universe$rWAR)), color = "darkgray") +
    scale_x_continuous("Age") +
    labs(title = paste0(horizon_years, "-year rWAR projection for ", x$q$nameFirst, " ", x$q$nameLast, ", starting in ", x$horizon),
         subtitle = paste("Based on", length(unique(x$comps_universe$playerId)), "comparable players since", min(x$comps_universe$yearId)),
         caption = "Source: Baseball-Reference.com")

  this_decile <- x$comps_universe %>%
    filter(playerId == x$lahman_id) %>%
    pull(quantile) %>%
    unique()

  deciles <- levels(x$comps_universe$quantile) %>%
    tibble::enframe(value = "decile") %>%
    mutate(this_player = decile == this_decile,
           color = ifelse(this_player, "red", "gray"))

  grid <- x$comps_universe %>%
    ungroup() %>%
    modelr::data_grid(age = modelr::seq_range(age, by = 1),
                      quantile = levels(quantile)) %>%
    mutate(.fitted = predict(x$mod_rwar, newdata = .))

  max_quantile <- tail(levels(x$comps_universe$quantile), 1)

  next_contract <- grid %>%
    filter(age > x$horizon_age,
           age <= x$horizon_age + horizon_years,
           quantile == this_decile)

  base_plot +
    geom_line(data = grid, aes(y = .fitted, color = stats::reorder(quantile, .fitted))) +
    scale_color_manual("rWAR decile", values = deciles$color, "red") +
    geom_area(data = next_contract, aes(y = .fitted), alpha = 0.5, fill = "pink") +
    annotate("text", x = mean(next_contract$age), y = mean(next_contract$.fitted) / 2,
             label = paste(round(sum(next_contract$.fitted), 1), "rWAR"))

}


#' @rdname comps
#' @export
#' @examples
#' plot_comps_universe(comps("wrighda03", 2009))

plot_comps_universe <- function(x, ...) {
  this_decile <- x$comps_universe %>%
    filter(playerId == x$lahman_id) %>%
    pull(quantile) %>%
    unique()

  these_comps <- x$comps_universe %>%
    filter(quantile == this_decile) %>%
    group_by(playerId) %>%
    summarize(N = n(),
              tRAA_bat = sum(RAA_bat),
              tRAA_field = sum(RAA_field)
    )
  ggplot(data = these_comps, aes(x = tRAA_bat, y = tRAA_field)) +
    geom_point() +
    ggrepel::geom_label_repel(
      data = sample_n(these_comps, 20),
      aes(label = playerId)) +
    ggrepel::geom_label_repel(
      data = filter(these_comps, playerId == x$lahman_id),
      aes(label = playerId), color = "dodgerblue") +
    scale_y_continuous("Runs Above Average, Fielding") +
    scale_x_continuous("Runs Above Average, Batting") +
    labs(title = "Quantile Comparable Players",
         subtitle = paste(nrow(these_comps),
                          "players. By Batting and Fielding contributions to rWAR"),
         caption = "Source: Baseball-Reference.com")
}

#' @rdname comps
#' @export
#' @examples
#' plot_salary(comps("wrighda03", 2009))

plot_salary <- function(x, horizon_years = 5, ...) {
  this_player <- x$comps_universe %>%
    filter(playerId == x$lahman_id)

  newdata <- predict(x) %>%
    rename(salary = salary_hat)

  ggplot(data = x$comps_universe, aes(x = mlb_exp, y = salary)) +
    geom_jitter(width = 0.1, alpha = 0.1) +
    geom_point(data = this_player, color = "dodgerblue") +
    geom_line(data = this_player, color = "dodgerblue") +
    geom_line(data = newdata, color = "red") +
    scale_y_continuous("USD", labels = scales::dollar) +
    scale_x_continuous("Years of Major League Experience") +
    labs(title = paste0(horizon_years, "-year salary projection for ", x$q$nameFirst, " ", x$q$nameLast, ", starting in ", x$horizon),
         subtitle = paste("Based on", length(unique(x$comps_universe$playerId)), "comparable players since", min(x$comps_universe$yearId)),
         caption = "Source: Baseball-Reference.com")
}

#' @rdname comps
#' @export
#' @examples
#' dollars_per_rwar()

dollars_per_rwar <- function() {
  past <- rwar %>%
    group_by(playerId, yearId) %>%
    summarize(rWAR = sum(rWAR)) %>%
    full_join(
      Lahman::Salaries %>%
        group_by(playerId = playerID, yearId = yearID) %>%
        summarize(salary = sum(salary, na.rm = TRUE)),
      by = c("playerId", "yearId")) %>%
    group_by(yearId) %>%
    summarize(num_players = n_distinct(playerId),
              rWAR = sum(rWAR, na.rm = TRUE),
              salary = sum(as.numeric(salary), na.rm = TRUE)) %>%
    mutate(dollars_per_rwar = salary / rWAR)

  mod <- lm(dollars_per_rwar ~ yearId, data = filter(past, salary > 0))

  out <- broom::augment(mod, newdata = data.frame(yearId = 2017:2040)) %>%
    select(-.se.fit) %>%
    full_join(past, by = "yearId") %>%
    mutate(dollars_per_rwar = ifelse(is.na(dollars_per_rwar), .fitted, dollars_per_rwar),
           dollars_per_rwar = ifelse(dollars_per_rwar == 0, .fitted, dollars_per_rwar)) %>%
    select(-.fitted) %>%
    arrange(yearId)
  out
}

#' @rdname comps
#' @export
#' @examples
#' plot(comps("wrighda03", 2009))

plot.comps <- function(x, horizon_years = 5, ...) {

  plot_data <- x$comps_universe %>%
    left_join(
      select(dollars_per_rwar(), yearId, dollars_per_rwar),
      by = "yearId"
    ) %>%
    mutate(value = rWAR * dollars_per_rwar)

  z <- x$comps_universe %>%
    filter(age >= 15, age <= 50) %>%
    pull(age) %>%
    range()

  ys <- seq(z[1], z[2], by = 1) %>%
    tibble::enframe(value = "age") %>%
    mutate(yearId = age + x$horizon - x$horizon_age) %>%
    filter(yearId %% 5 == 0)

  this_player <- plot_data %>%
    filter(playerId == x$lahman_id) %>%
    select(playerId, yearId, age, value, salary, value) %>%
    full_join(predict(x), by = c("playerId", "yearId")) %>%
    left_join(
      select(dollars_per_rwar(), yearId, dollars_per_rwar),
      by = "yearId"
    ) %>%
    mutate(age = ifelse(is.na(age.x), age.y, age.x),
           value_hat = rWAR_hat * dollars_per_rwar,
           net_value = value - salary,
           net_value_hat = value_hat - salary_hat,
           salary = -salary,
           salary_hat = -salary_hat) %>%
    select(playerId, yearId, age, matches("(value|salary)")) %>%
    tidyr::gather(key = "type", value = "value", -playerId, -yearId, -age) %>%
    mutate(predicted = grepl("_hat", type),
           type = gsub("_hat", "", type))


  ggplot(plot_data, aes(x = age, y = value)) +
    geom_jitter(width = 0.1, alpha = 0.1, color = "darkgray") +
    geom_jitter(aes(y = -salary), width = 0.1, alpha = 0.1, color = "pink") +
    geom_vline(xintercept = x$horizon_age, color = "black", linetype = 2) +
    geom_point(data = filter(this_player, predicted == FALSE), aes(color = type)) +
    geom_line(data = this_player, aes(color = type, linetype = predicted)) +
    geom_text(data = ys, aes(label = yearId, y = max(plot_data$value)), color = "darkgray") +
    scale_x_continuous("Age") +
    scale_y_continuous("Value (USD)", labels = scales::dollar) +
    scale_color_manual(values = c("net_value" = "purple", "salary" = "red", value = "blue")) +
    labs(title = paste0("Value projection for ", x$q$nameFirst, " ", x$q$nameLast, ", starting in ", x$horizon),
         subtitle = paste("Based on", length(unique(plot_data$playerId)), "comparable players since", min(plot_data$yearId)),
         caption = "Source: Baseball-Reference.com")
}
