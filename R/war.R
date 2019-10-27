#' Build a data frame suitable for projecting WAR
#' @export
#' @examples
#' \dontrun{
#' test <- model_frame()
#' }

model_frame <- function() {
  comps_hypercube %>%
    group_by(playerID) %>%
    arrange(playerID, yearID) %>%
    mutate(
      # need to fix this if they miss a year
      rWAR_1 = dplyr::lag(rWAR),
      rWAR_2 = dplyr::lag(rWAR, 2),
      rWAR_3 = dplyr::lag(rWAR, 3)
    ) %>%
    ungroup()
}

