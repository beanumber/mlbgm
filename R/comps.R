globalVariables(c("AB", "BB", "BFP", "HBP", "IBB", "InnOuts", "PA", "POS",
                  "RAA_bat", "RAA_field", "RAA_pitch", "SF", "SH", "salary",
                  "WAR_PA", "age", "birthYear", "comps_hypercube", "cum_BFP",
                  "cum_PA", "decile", "dist", "head", "nameFirst", "nameLast",
                  "playerID", "playerId", "quantile", "rRAA_bat", "rRAA_field",
                  "rRAA_pitch", "rWAR", "rwar", "tTPA", "tWAR", "this_player", "yearId",
                  "value", "age.x", "age.y", "rWAR_hat", "value_hat", "salary_hat",
                  "type", "predicted", "tRAA_bat", "tRAA_field", "mlb_exp",
                  "BB_b", "BB_p", "SO", "SO_b", "SO_p", "SB",
                  "CS", "cum_so_b", "cum_so_p", "cum_bb_b", "cum_bb_p"))

#' Modeling future performance
#' @param lahman_id Lahman ID of the player
#' @param horizon Numeric description of the year from which you want to project
#' @param max_dist Maximum distance allowed for nearest neighbors
#' @param max_num Maximum number of neighbors returned
#' @export
#' @examples
#' \dontrun{
#' comps("kiescbr01")
#' comps("kiescbr01", 2000)
#' comps("piazzmi01")
#' comps("piazzmi01", 1995)
#' comps("wrighda03")
#' comps("wrighda03", 2009)
#' comps("harpebr03")
#' }


comps <- function(lahman_id,
                  horizon = as.numeric(format(Sys.Date(), "%Y")),
                  max_dist = 1e4,
                  max_num = 1e2) {

  x <- mlbgm::comps_hypercube %>%
    filter(yearID <= as.numeric(horizon))
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

  space <- tibble::tribble(
    ~var_name, ~weight,
    "age", 100,
    "cum_PA", 0.001,
    "cum_BFP", 0.001,
    "speed_score_1", 100,
    "cum_so_b", 0.01,
    "cum_bb_b", 0.01,
    "cum_so_p", 0.01,
    "cum_bb_p", 0.01,
    "cum_rwar_max", 100,
    "cum_rwar_avg", 500,
    "cum_rwar", 10,
  )

  q_long <- q %>%
    select(space$var_name) %>%
    tidyr::pivot_longer(-matches("ID$"), names_to = "var_name", values_to = "value")
  comps_space <- x %>%
    select(playerID, yearID, space$var_name)
  comps_df_var <- comps_space %>%
    tidyr::pivot_longer(-matches("ID$"), names_to = "var_name", values_to = "value") %>%
    left_join(space, by = "var_name") %>%
    left_join(q_long, by = "var_name") %>%
    mutate(
      sq_diff = (value.x - value.y)^2,
      wgt_sq_diff = weight * sq_diff
    ) %>%
    group_by(playerID, yearID) %>%
    mutate(
      total_wgt_sq_diff = sum(wgt_sq_diff, na.rm = TRUE),
      pct_wgt_sq_diff = wgt_sq_diff / total_wgt_sq_diff
    )

#  comps_df_var %>%
#    group_by(var_name) %>%
#    skimr::skim(pct_wgt_sq_diff)

  comps_df <- comps_df_var %>%
    summarize(
      dist = sqrt(min(total_wgt_sq_diff))
    ) %>%
    arrange(dist) %>%
    filter(dist <= max_dist) %>%
    distinct(playerID, .keep_all = TRUE) %>%
    head(max_num) %>%
    left_join(comps_space, by = c("playerID", "yearID"))

  out <- list(
    lahman_id = lahman_id,
    horizon = horizon,
    horizon_age = horizon -
      Lahman::Master %>%
      filter(playerID == lahman_id) %>%
      pull(birthYear),
#    hypercube = x,
    query = q,
    comps_df = comps_df %>%
      select(-matches("dist_")),
    comps_universe = x %>%
      select(playerID, yearID, rWAR, age) %>%
      inner_join(select(comps_df, playerID, dist), by = "playerID")
  )
  out$mod_rwar <- comps_mod_rwar(out$comps_universe, lahman_id)
#  out$mod_salary <- comps_mod_salary(out$comps_universe)
  out$predict_rwar <- predict_rwar(out)
  class(out) <- append("comps", class(out))
  out
}

comps_hypercube_build <- function() {

  bat <- Lahman::Batting %>%
    tidyr::replace_na(list(IBB = 0, BB = 0, SB = 0, CS = 0, SF = 0, SH = 0)) %>%
    group_by(playerID, yearID) %>%
    summarize(
      PA = sum(AB + BB + IBB + HBP + SF + SH, na.rm = TRUE),
      R = sum(R, na.rm = TRUE),
      H = sum(H, na.rm = TRUE),
      singles = sum(H - X2B - X3B - HR, na.rm = TRUE),
      triples = sum(X3B, na.rm = TRUE),
      HR = sum(HR, na.rm = TRUE),
      BIP = sum(AB - HR - SO, na.rm = TRUE),
      GIDP = sum(GIDP, na.rm = TRUE),
      BB_b = sum(BB + HBP, na.rm = TRUE),
      SO_b = sum(SO, na.rm = TRUE),
      SB = sum(SB, na.rm = TRUE),
      CS = sum(CS, na.rm = TRUE),
    ) %>%
    mutate(
      # https://en.wikipedia.org/wiki/Speed_Score
#      speed_score = mean(c(
      SS1 = 20 * ((SB + 3) / (SB + CS + 7) - 0.4),
      SS2 = sqrt((SB + CS) / (singles + BB_b)) / 0.07,
      SS3 = 625 * triples / BIP,
      SS4 = 25 * ((R - HR) / (H + BB_b - HR) - 0.1),
      SS5 = (0.063 - (GIDP / BIP)) / 0.007,
#      ), na.rm = TRUE
    )

  pitch <- Lahman::Pitching %>%
    group_by(playerID, yearID) %>%
    summarize(BFP = sum(BFP),
              BB_p = sum(BB, na.rm = TRUE),
              SO_p = sum(SO, na.rm = TRUE))

  field_long <- Lahman::Fielding %>%
    group_by(playerID, yearID, POS) %>%
    summarize(InnOuts = sum(InnOuts), PO = sum(PO), A = sum(A), G = sum(G)) %>%
    ungroup() %>%
    mutate(
      RF = (PO + A) / G,
      SS6 = case_when(
      POS == "P" ~ 1,
      POS == "C" ~ 0,
      POS == "1B" ~ 2,
      POS == "2B" ~ 1.25 * RF,
      POS == "3B" ~ (4/2.65) * RF,
      POS == "SS" ~ (7/4.6) * RF,
      POS == "OF" ~ 3 * RF,
      TRUE ~ 0
    ))

  speed_score_6 <- field_long %>%
    tidyr::replace_na(list(InnOuts = 1)) %>%
    group_by(playerID, yearID) %>%
    summarize(SS6 = stats::weighted.mean(SS6, InnOuts, na.rm = TRUE))

  field <- field_long %>%
    select(playerID, yearID, POS, InnOuts) %>%
    tidyr::spread(key = "POS", value = "InnOuts")

  value <- mlbgm::rwar %>%
    group_by(playerID = playerId, yearID = yearId) %>%
    summarise(rWAR = sum(rWAR, na.rm = TRUE),
              rRAA_bat = sum(rRAA_bat, na.rm = TRUE),
              rRAA_field = sum(rRAA_field, na.rm = TRUE),
              rRAA_pitch = sum(rRAA_pitch, na.rm = TRUE))

  x <- bat %>%
    left_join(pitch, by = c("playerID", "yearID")) %>%
    left_join(field, by = c("playerID", "yearID")) %>%
    left_join(value, by = c("playerID", "yearID")) %>%
    left_join(speed_score_6, by = c("playerID", "yearID")) %>%
    left_join(select(Lahman::Master, playerID, birthYear), by = "playerID") %>%
    mutate(age = yearID - birthYear,
           speed_score = (SS1 + SS2 + SS3 + SS4 + SS5 + SS6) / 6) %>%
    select(-birthYear) %>%
    tidyr::replace_na(
      list(PA = 0, BFP = 0, BB_b = 0, BB_p = 0)
    ) %>%
    group_by(playerID) %>%
    arrange(playerID, yearID) %>%
    mutate(
      # need to fix this if they miss a year
      rWAR_1 = dplyr::lag(rWAR, 1),
      rWAR_2 = dplyr::lag(rWAR, 2),
      rWAR_3 = dplyr::lag(rWAR, 3),
      cum_PA = cumsum(tidyr::replace_na(dplyr::lag(PA, 1), 0)),
      cum_BFP = cumsum(tidyr::replace_na(dplyr::lag(BFP, 1), 0)),
      cum_so_b = cumsum(tidyr::replace_na(dplyr::lag(SO_b, 1), 0)),
      cum_bb_b = cumsum(tidyr::replace_na(dplyr::lag(BB_b, 1), 0)),
      cum_so_p = cumsum(tidyr::replace_na(dplyr::lag(SO_p, 1), 0)),
      cum_bb_p = cumsum(tidyr::replace_na(dplyr::lag(BB_p, 1), 0)),
      cum_rwar = cumsum(tidyr::replace_na(dplyr::lag(rWAR, 1), 0)),
      cum_raa_b = cumsum(tidyr::replace_na(dplyr::lag(rRAA_bat, 1), 0)),
      cum_raa_p = cumsum(tidyr::replace_na(dplyr::lag(rRAA_pitch, 1), 0)),
      cum_raa_f = cumsum(tidyr::replace_na(dplyr::lag(rRAA_field, 1), 0)),
      cum_so_bb_b = cum_so_b / cum_bb_b,
      cum_so_bb_p = cum_so_p / cum_bb_p,
      cum_rwar_max = cummax(tidyr::replace_na(dplyr::lag(rWAR, 1), 0)),
      cum_rwar_avg = cummean(tidyr::replace_na(dplyr::lag(rWAR, 1), 0)),
      cum_p_vec = cumsum(tidyr::replace_na(dplyr::lag(P, 1), 0)),
      cum_c_vec = cumsum(tidyr::replace_na(dplyr::lag(C, 1), 0)),
      cum_1b_vec = cumsum(tidyr::replace_na(dplyr::lag(`1B`, 1), 0)),
      cum_2b_vec = cumsum(tidyr::replace_na(dplyr::lag(`2B`, 1), 0)),
      cum_3b_vec = cumsum(tidyr::replace_na(dplyr::lag(`3B`, 1), 0)),
      cum_ss_vec = cumsum(tidyr::replace_na(dplyr::lag(SS, 1), 0)),
      cum_of_vec = cumsum(tidyr::replace_na(dplyr::lag(OF, 1), 0)),
      cum_ip = cum_p_vec + cum_c_vec + cum_1b_vec + cum_2b_vec + cum_3b_vec + cum_ss_vec + cum_of_vec,
      speed_score_1 = dplyr::lag(speed_score, 1),
    ) %>%
    mutate_at(vars(matches("_vec")), ~ . / cum_ip) %>%
    ungroup()
}

#' @rdname comps
#' @param .data Passed to the data argument of \code{\link[stats]{lm}}
#' @export

comps_mod_rwar <- function(.data, lahman_id) {
  .data %>%
    filter(playerID != lahman_id) %>%
    stats::loess(rWAR ~ age, data = ., weights = 1/dist)
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
#' \dontrun{
#' predict(comps("beltrca01", 2005))
#' }

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
#' @export

predict_rwar <- function(x, ...) {
  model <- x$comps_universe %>%
    ungroup() %>%
    modelr::data_grid(age = modelr::seq_range(age, by = 1)) %>%
    broom::augment(x$mod_rwar, newdata = .) %>%
    mutate(
      upper = .fitted + 1.96 * .se.fit,
      lower = .fitted - 1.96 * .se.fit,
      yearID = age + (x$horizon - x$horizon_age),
      playerID = x$lahman_id
    )

  x$comps_universe %>%
    filter(playerID == x$lahman_id)

  tmp <- x$comps_universe %>%
    group_by(age) %>%
    mutate(
      rWAR_ntile = percent_rank(rWAR)
    )
  ntile <- tmp %>%
    filter(playerID == x$lahman_id) %>%
    pull(rWAR_ntile) %>%
    mean(na.rm = TRUE)

  ntiles <- c(rWAR_25 = 0.25, rWAR_50 = 0.50, rWAR_75 = 0.75, rWAR_player = ntile)

  empirical <- tmp %>%
    summarize(
      rWAR_pct = list(stats::quantile(rWAR, probs = ntiles))
    ) %>%
    mutate(ntile = list(names(ntiles))) %>%
    tidyr::unnest(c(rWAR_pct, ntile)) %>%
    tidyr::pivot_wider(names_from = ntile,
  #                     values_fn = list(rWAR_pct = length),
                       values_from = rWAR_pct)

  model %>%
    left_join(empirical, by = "age")
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
    geom_point(data = filter(x$comps_universe, playerID == x$lahman_id), color = "dodgerblue") +
    geom_line(data = filter(x$comps_universe, playerID == x$lahman_id), color = "dodgerblue") +
    geom_text(data = ys, aes(label = yearId, y = max(x$comps_universe$rWAR)), color = "darkgray") +
    scale_x_continuous("Age") +
    labs(title = paste0(horizon_years, "-year rWAR projection for ", x$q$nameFirst, " ", x$q$nameLast, ", starting in ", x$horizon),
         subtitle = paste("Based on", length(unique(x$comps_universe$playerID)), "most comparable players since", min(x$comps_universe$yearID)),
         caption = "Source: Baseball-Reference.com")

  grid <- predict_rwar(x)

  next_contract <- grid %>%
    filter(age >= x$horizon_age,
           age < x$horizon_age + horizon_years)

  base_plot +
    geom_line(data = grid, aes(y = .fitted), color = "red") +
#    geom_line(data = grid, aes(y = lower), color = "red", lty = 2, alpha = 0.5) +
#    geom_line(data = grid, aes(y = upper), color = "red", lty = 2, alpha = 0.5) +
    geom_line(data = grid, aes(y = rWAR_25), color = "red", lty = 2, alpha = 0.5) +
    geom_line(data = grid, aes(y = rWAR_75), color = "red", lty = 2, alpha = 0.5) +
    geom_line(data = grid, aes(y = rWAR_player), color = "blue", lty = 2, alpha = 0.5) +
    geom_area(data = next_contract, aes(y = rWAR_player), alpha = 0.5, fill = "pink") +
    annotate("text", x = mean(next_contract$age), y = mean(next_contract$rWAR_player) / 2,
             label = paste(round(sum(next_contract$rWAR_player), 1), "rWAR"))

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
