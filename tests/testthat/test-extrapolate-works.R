context("test-extrapolate")

test_that("extrapolate works", {
  salaries <- Lahman::Salaries %>%
    dplyr::group_by(playerID, yearID) %>%
    dplyr::summarize(salary = max(salary))

  mod <- lm(salary ~ factor(yearID), data = salaries)

  efx <- pull_effects(mod, "yearID")
  expect_gt(nrow(pull_effects(mod, "yearID")), 0)
  expect_true("yearID" %in% names(efx))
  expect_s3_class(mod_efx <- extrapolate_effects(mod, "yearID"), "lm")
  expect_true(any(grepl("yearID", names(coef(mod_efx)))))

  X <- data.frame(yearID = 2019)
  expect_equal(nrow(df <- augment_future(mod, newdata = X, "yearID")), 1)
  expect_true("yearID" %in% names(df))

})

test_that("futures work", {
  futures <- read_ws_probs()
  expect_equal(nrow(futures), 30)
  expect_equal(sum(futures$ws_prob_normalized), 1)

  x <- unique(trimws(stringr::str_sub(contracts$Team, 1, 3)))
  standardize_team_ids(x)
})
