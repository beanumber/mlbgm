#' @title read_rwar
#'
#' @description Retrieves rWAR from Baseball-Reference.com
#'
#' @details Retrieves daily rWAR figures from Baseball-Reference.com and stores it as a data.frame
#'
#' @return a data.frame consisting of rWAR
#'
#' @importFrom magrittr %>%
#' @export
#' @examples
#'
#' # Get data from yesterday
#' \dontrun{
#' rWAR <- read_rwar()
#' }
#' data(rWAR)
#' hist(rWAR$rWAR)
#'
#' # Leaders since 1954
#' library(dplyr)
#' modern = filter(rWAR, yearId >= 1954)
#' head(arrange(modern, desc(rWAR)), 20)
#'
#' # Relationship between batting and fielding
#' plot(modern$rRAA_bat, modern$rRAA_field, cex=0.5)

read_rwar <- function() {
    bat <- "http://www.baseball-reference.com/data/war_daily_bat.txt"
    pitch <- "http://www.baseball-reference.com/data/war_daily_pitch.txt"
    rWAR_b <- readr::read_csv(bat, na = c("NULL", "NA", ""))
    rWAR_p <- readr::read_csv(pitch, na = c("NULL", "NA", ""))

    # Grab the number of batters faced for pitchers
    bf_lkup <- Lahman::Pitching %>%
      dplyr::select("playerID", "yearID", "teamID", "stint", "BFP")
    rWAR_p <- rWAR_p %>%
      dplyr::left_join(bf_lkup, by = c("player_ID" = "playerID",
                                "year_ID" = "yearID",
                                "stint_ID" = "stint"))

    bat_fields <- c(
      "player_ID", "year_ID", "stint_ID", "team_ID", "PA",
      "runs_position", "runs_above_rep", "runs_above_avg",
      "runs_above_avg_off", "runs_above_avg_def", "WAR")
    pitch_fields <- c(
      "player_ID", "year_ID", "stint_ID", "team_ID", "BFP",
      "runs_above_rep", "runs_above_avg", "WAR")
    rWAR <- dplyr::full_join(
      x = rWAR_b[, bat_fields],
      y = rWAR_p[, pitch_fields],
      by = c("player_ID", "year_ID", "stint_ID"))
    rWAR <- rWAR %>%
      dplyr::rename_(rRAA_bat = ~runs_above_avg_off
              , rRAA_field = ~runs_above_avg_def
              , rRAA_pitch = ~runs_above_avg.y
              , playerId = ~player_ID
              , yearId = ~year_ID
              , stintId = ~stint_ID) %>%
      dplyr::mutate_(TPA = ~ifelse(is.na(PA), 0, PA) + ifelse(is.na(BFP), 0, BFP),
              rRAR = ~ifelse(is.na(runs_above_rep.x), 0, runs_above_rep.x) +
                ifelse(is.na(runs_above_rep.y), 0, runs_above_rep.y),
              rRAA = ~ifelse(is.na(runs_above_avg.x), 0, runs_above_avg.x) +
                ifelse(is.na(rRAA_pitch), 0, rRAA_pitch),
              rRepl = ~rRAA - rRAR,
              rWAR = ~ifelse(is.na(WAR.x), 0, WAR.x) +
                ifelse(is.na(WAR.y), 0, WAR.y),
              teamId = ~ifelse(is.na(team_ID.x), as.character(team_ID.y), as.character(team_ID.x)))
    out <- rWAR %>%
      dplyr::select(-dplyr::matches("\\.(x|y)"))
    return(out)
}
