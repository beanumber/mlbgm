#' Tools for extrapolating effects from models
#' @description This function will extrapolate fixed effects for the future,
#'  assuming linear growth.
#' @param mod a model that has a \code{\link[broom]{tidy}} method
#' @param col_name name to match the name of the effect
#' @param ... currently ignored
#' @importFrom stats lm
#' @return an \code{\link{lm}} object
#' @export

mod_effect <- function(mod, col_name, ...) {
  fixed_effects <- filter_effect(mod, col_name, ...)
  lm(paste("estimate ~ ", col_name), data = fixed_effects)
}

#' @rdname mod_effect
#' @export

filter_effect <- function(mod, col_name, ...) {
  out <- mod %>%
    broom::tidy() %>%
    dplyr::filter(grepl(col_name, term))
  out[, col_name] <- readr::parse_number(out$term)
  out
}

#' @rdname mod_effect
#' @param newdata passed to \code{\link{predict}}
#' @importFrom stats predict coef
#' @importFrom utils tail
#' @export

augment_future <- function(mod, newdata, col_name, ...) {

  effects <- filter_effect(mod, col_name)

  last_effect <- effects %>%
    arrange(desc(term)) %>%
    head(1)

  max_effect <- tail(effects, 1)
  max_effect_name <- grep(col_name, names(coef(mod)), value = TRUE) %>%
    max() %>%
    readr::parse_number()

  mod_effects <- mod_effect(mod, col_name)

  future_effects <- mod_effects %>%
    broom::augment(newdata = newdata) %>%
    dplyr::rename(effect = .fitted) %>%
    dplyr::select(-.se.fit)

  newdata$temp <- newdata[, col_name] %>%
    purrr::as_vector()
  newdata[, col_name] <- factor(as_vector(last_effect[, col_name]))
  newdata <- newdata %>%
    dplyr::mutate(y_hat_old = predict(mod, newdata = .)) %>%
    dplyr::left_join(
      future_effects[, c(col_name, "effect")],
      by = c("temp" = col_name)) %>%
    dplyr::mutate(.fitted = y_hat_old + effect -  last_effect$estimate)
  newdata[, col_name] <- newdata$temp
  newdata %>%
    dplyr::select(-temp, -y_hat_old, -effect)
}
