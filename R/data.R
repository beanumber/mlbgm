#' Cross-reference Player IDs across systems
#'
#' A data frame containing player ID mappings
#'
#' @docType data
#' @format A data frame with one row for each player and 15 columns:
#' \describe{
#'    \item{name_last}{the person's last name}
#'    \item{name_first}{the person's first name}
#'    \item{name_given}{the person's given name}
#'    \item{mlbam_id}{the person's MLBAM id}
#'    \item{lahman_id}{the person's Lahman id}
#'    \item{retro_id}{the person's Retrosheet id}
#'    \item{bp_id}{the person's Baseball Prospectus id}
#'    \item{davenport_id}{the person's Davenport id}
#'    \item{fg_id}{the person's FanGraphs id}
#'    \item{cbs_id}{the person's CBS id}
#'    \item{espn_id}{the person's ESPN id}
#'    \item{nfbc_id}{the person's NFBC id}
#'    \item{yahoo_id}{the person's Yahoo id}
#'    \item{ottoneu_id}{the person's Ottoneu id}
#'    \item{rotowire_id}{the person's Rotowire id}
#' }
#'
#' @source \url{http://chadwick-bureau.com/the-register/}
#' @source \url{http://www.baseballprospectus.com/sortable/playerids/playerid_list.csv}
#' @source \url{http://crunchtimebaseball.com/master.csv}
#' @seealso \code{\link[Lahman]{Master}}
#'
"lkup_players"

#' fWAR values downloaded from FanGraphs.
#'
#' A dataset containing fWAR values
#'
#' @docType data
#' @format A data frame with one row for each player-season and 11 columns:
#' \describe{
#'    \item{playerid}{the FanGraphs id of the player}
#'    \item{yearId}{the season}
#'    \item{Name}{the player's name}
#'    \item{fWAR}{the player's fWAR in that season}
#'    \item{fRAA_bat}{the Runs Above Average earned while batting}
#'    \item{fRAA_br}{the Runs Above Average earned while running the bases}
#'    \item{fRAA_field}{the Runs Above Average earned while fielding}
#'    \item{fWAR_pitch}{the Runs Above Average earned while pitching}
#'    \item{fRAR}{the Runs Above Replacement}
#'    \item{fRAA}{the Runs Above Average}
#'    \item{fRepl}{the replacement level, in runs}
#' }
#'
#' @source \url{http://fangraphs.com/}
#'
"fwar"

#' rWAR values downloaded from Baseball-Reference.com
#'
#' A dataset containing rWAR values
#' @docType data
#' @format A data frame with one row for each player-season and 15 columns:
#' \describe{
#'    \item{playerId}{the Lahman id of the player}
#'    \item{yearId}{the season}
#'    \item{stintId}{the stint}
#'    \item{PA}{the number of plate appearances}
#'    \item{runs_position}{?}
#'    \item{rRAA_bat}{the Runs Above Average earned while batting and running the bases}
#'    \item{rRAA_field}{the Runs Above Average earned while fielding}
#'    \item{BFP}{the number of batters faced}
#'    \item{rRAA_pitch}{the Runs Above Average earned while pitching}
#'    \item{TPA}{the sum of PA and BFP}
#'    \item{rRAR}{the Runs Above Replacement}
#'    \item{rRAA}{the Runs Above Average}
#'    \item{rRepl}{the replacement level, in runs}
#'    \item{rWAR}{the WAR according to Baseball-Reference}
#'    \item{teamId}{the team's ID}
#' }
#'
#' @source \url{http://baseball-reference.com/}
#'
"rwar"

#' Franchise values
#' @docType data
"forbes"

#' MSA statistics
#' @docType data
"msa"

#' MSA-MLB map
#' @docType data
"msa_mlb"

#' Major League Service Time
#' @docType data
#' @source \url{https://legacy.baseballprospectus.com/compensation/cots/}
"mls"

#' Contracts
#' @docType data
#' @source \url{https://www.spotrac.com/mlb/contracts/}
"contracts"

#' Comparable Players
#' @docType data
"comps_hypercube"
