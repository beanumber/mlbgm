library(mlbgm)
library(tidyverse)
library(Lahman)
library(broom)
library(modelr)

#PRED_DF_GENERATOR FUNCTION

#produces data frame that is then fed into the baseline prediction function
#useful for viewing and understanding the data being fed into the function
#also allows you to see which player and year combos can be predicted
pred_df_generator <- function(year) {
  #converts the year argument to a numeric
  year <- as.numeric(year)
  #renaming for ease
  stats <- comps_hypercube
  
  #creates a variable for strikeout to walk ratio
  batting <- Batting %>%
    group_by(playerID, yearID) %>%
    summarise(BB = sum(BB, na.rm = TRUE), SO = sum(SO, na.rm = TRUE))
  batting <- batting %>%
    mutate(bb_so = SO/BB)
  
  #remove NaNs and Inf from strikeout to walk ratio
  batting <- batting %>%
    filter(bb_so != "Inf" & bb_so != "NaN")
  
  #removes unnecessary data from the rwar dataframe
  rwar <- rwar %>%
    select(playerId, yearId, stintId, PA, BFP, TPA, rWAR, rRAA_bat,
           rRAA_field, rRAA_pitch)
  
  #removing stints for rwar
  #including na.rm will fix NAs for stints
  rwar <- rwar %>%
    group_by(playerId, yearId) %>%
    summarise(PA = sum(PA, na.rm = TRUE), BFP = sum(BFP, 
                                                    na.rm = TRUE),
              TPA = sum(TPA, na.rm = TRUE),
              rWAR = sum(rWAR, na.rm = TRUE), 
              rRAA_bat = sum(rRAA_bat, na.rm = TRUE),
              rRAA_field = sum(rRAA_field, na.rm = TRUE), 
              rRAA_pitch = sum(rRAA_pitch, na.rm = TRUE))
  
  #identifying the variables DO NOT to be lagged in the data
  nolag <- rwar %>%
    select(playerId, yearId, rWAR)
  nolag2 <- stats %>%
    select(playerID, yearID, age)
  nolag <- left_join(nolag, nolag2, by = c("playerId" = "playerID", 
                                           "yearId" = "yearID"))
  
  #identifying variables that will be modeled based on the performance up to the previous year
  tolag <- rwar %>%
    select(playerId, yearId, rRAA_bat, rRAA_field, rRAA_pitch)
  tolag2 <- stats %>%
    select(playerID, yearID, cum_PA, cum_BFP)
  tolag3 <- batting %>%
    select(playerID, yearID, bb_so)
  tolag <- left_join(tolag, tolag2, by = c("playerId" = "playerID", 
                                           "yearId" = "yearID"))
  tolag <- left_join(tolag, tolag3, by = c("playerId" = "playerID", 
                                           "yearId" = "yearID"))
  #activating the lag
  tolag$prev_yearId <- tolag$yearId + 1
  tolag <- tolag %>%
    rename(previousyear = yearId)
  
  #creating our final data frame 
  finaldata <- left_join(nolag, tolag, by = c("playerId", "yearId" = "prev_yearId"))
  finaldata <- finaldata %>%
    ungroup()
  #dealing with NA values
  finaldata <- finaldata %>%
    mutate(bb_so = ifelse(is.na(bb_so), 0, bb_so),
           rRAA_bat = ifelse(is.na(rRAA_bat), 0, rRAA_bat),
           rRAA_field = ifelse(is.na(rRAA_field), 0, rRAA_field),
           rRAA_pitch = ifelse(is.na(rRAA_pitch), 0, rRAA_pitch))
  
  #filtering for the desired year
  finaldata <- finaldata %>%
    ungroup()
  finaldata <- finaldata %>%
    filter(yearId %in% year)
  finaldata <- finaldata %>%
    filter(!is.na(previousyear))
  
}
test <- pred_df_generator(2008:2018)

#BASELINE_PREDICTION FUNCTION

#set seed for reproducibility before running this function
#takes in a data set, player(s), and year(s) and produces predicted rWAR
#"data" argument must be the result of the pred_df_generator
baseline_prediction <- function(data, playerID, year) {
  basemod <- lm(rWAR ~ age + rRAA_bat + rRAA_field + rRAA_pitch + cum_PA +
                  cum_BFP + bb_so, data, na.action = na.exclude)
  withpreds <- add_predictions(data, basemod, var = "pred", 
                               type = NULL)
  withpreds <- withpreds %>%
    filter(playerId %in% playerID, yearId %in% year)
  final <- withpreds %>%
    rename(actual_rWAR = rWAR, predicted_rWAR = pred)
  final <- final %>%
    select(playerId, yearId, actual_rWAR, predicted_rWAR)
  
}
test2 <- baseline_prediction(test, c("aardsda01", "aceveal01"), 2008:2015)

