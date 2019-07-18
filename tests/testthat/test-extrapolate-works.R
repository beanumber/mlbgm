context("test-extrapolate")

test_that("extrapolate works", {
  salaries <- Lahman::Salaries %>%
    dplyr::group_by(playerID, yearID) %>%
    dplyr::summarize(salary = max(salary))

  mod <- lm(salary ~ factor(yearID), data = salaries)

  efx <- filter_effect(mod, "yearID")
  expect_gt(nrow(efx), 0)
  expect_true("yearID" %in% names(efx))
  expect_s3_class(mod_efx <- mod_effect(mod, "yearID"), "lm")
  expect_true(any(grepl("yearID", names(coef(mod_efx)))))

  X <- data.frame(yearID = 2019)
  expect_equal(nrow(df <- augment_future(mod, newdata = X, "yearID")), 1)
  expect_true("yearID" %in% names(df))

})

test_that("futures work", {
  futures <- read_ws_futures() %>%
    summarize_futures()
  expect_equal(nrow(futures), 30)
  expect_equal(sum(futures$ws_prob_normalized), 1)

  x <- unique(trimws(stringr::str_sub(contracts$Team, 1, 3)))
  standardize_team_ids(x)
})

test_that("teams work", {
  x <- lkup_teams()
  expect_is(x, "tbl_df")
  expect_equal(nrow(x), 30)

  expect_length(mlb_pal(), 30)
  expect_named(mlb_pal())
  expect_is(class(names(mlb_pal())), "character")

  any(mlb_pal(1) == mlb_pal(2))

  expect_error(mlb_pal("lahman_name"), "non-numeric")
  expect_equal(mlb_pal(names = "lahman_name"), mlb_pal(1, names = "lahman_name"))
  expect_equivalent(mlb_pal(names = "lahman_name"), mlb_pal(1, names = "teamcolors_name"))
})
