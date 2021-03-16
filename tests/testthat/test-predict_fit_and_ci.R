lm_outcome <- comp_model(
  type = "linear",
  outcome = "BMI",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
)

log_outcome <- comp_model(
  type = "logistic",
  outcome = "disease",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
)
cox_outcome <- comp_model(
  type = "cox",
  event = "event",
  follow_up_time = "follow_up_time",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
)

old_comp <- comp_mean(
  simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
  units = "hr/day"
)
new_comp <-
  change_composition(
    composition = old_comp,
    main_part = "moderate",
    main_change = +0.5,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
  )

test_that("expected order of confidence interval limits", {
  expect_gt(
    predict_fit_and_ci(
      model = cox_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$upper_CI, predict_fit_and_ci(
    model = cox_outcome,
    dataset = simdata,
    terms = FALSE,
    new_data = new_comp,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
  )$fit )
})

test_that("expected order of confidence interval limits", {
  expect_lt(
    predict_fit_and_ci(
      model = cox_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$lower_CI, predict_fit_and_ci(
      model = cox_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})

test_that("expected order of confidence interval limits", {
  expect_gt(
    predict_fit_and_ci(
      model = lm_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$upper_CI, predict_fit_and_ci(
      model = lm_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})

test_that("expected order of confidence interval limits", {
  expect_lt(
    predict_fit_and_ci(
      model = lm_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$lower_CI, predict_fit_and_ci(
      model = lm_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})

test_that("expected order of confidence interval limits", {
  expect_gt(
    predict_fit_and_ci(
      model = log_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$upper_CI, predict_fit_and_ci(
      model = log_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})

test_that("expected order of confidence interval limits", {
  expect_lt(
    predict_fit_and_ci(
      model = log_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$lower_CI, predict_fit_and_ci(
      model = log_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})


test_that("input scale of new composition doesn't matter", {
  expect_equal(
    predict_fit_and_ci(
      model = log_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp*24,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit, predict_fit_and_ci(
      model = log_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})


test_that("wrong labels for composition throws error", {
  expect_error(
    predict_fit_and_ci(
      model = log_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary")
    ))
})

test_that("wrong labels for composition throws error", {
  expect_error(
    predict_fit_and_ci(
      model = log_outcome,
      dataset = simdata,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep", "extra")
    ))
})

install.packages("devtools", repos = "https://www.stats.bris.ac.uk/R/")
library(devtools)
devtools::install_github('tystan/deltacomp', repos = "https://www.stats.bris.ac.uk/R/")
library(deltacomp)

comp_labels <- c("sl", "sb", "lpa", "mvpa")
dcs <- predict_delta_comps(
  dataf = fat_data,
  y = "fat",
  comps = c("sl", "sb", "lpa", "mvpa"),
  covars = c("sibs", "parents", "ed"),
  deltas = seq(-60, 60, by = 5) / (24 * 60),
  comparisons = "one-v-one",
  alpha = 0.05
)

epic_linear <- comp_model(type = "linear", data = fat_data, rounded_zeroes = FALSE, outcome = "fat",comp_labels = c("sl", "sb", "lpa", "mvpa"),covariates = c("sibs", "parents", "ed"))
newdata <- data.frame(matrix(ncol = 0, nrow = 2) )
newdata$sb <- c(comp_mean(fat_data, comp_labels = comp_labels)$sb - (-1/24), comp_mean(fat_data, comp_labels = comp_labels)$sb)
newdata$sl <-  c(comp_mean(fat_data, comp_labels = comp_labels)$sl + (-1/24), comp_mean(fat_data, comp_labels = comp_labels)$sl + (-1/24))
newdata$lpa <- c(comp_mean(fat_data, comp_labels = comp_labels)$lpa, comp_mean(fat_data, comp_labels = comp_labels)$lpa + 1/24)
newdata$mvpa <- c(comp_mean(fat_data, comp_labels = comp_labels)$mvpa, comp_mean(fat_data, comp_labels = comp_labels)$mvpa)


ps <- predict_fit_and_ci(epic_linear, fat_data, newdata, comp_labels = c("sl", "sb", "lpa", "mvpa"))

test_that("equals results from deltacomp package", {
  expect_equal(ps$fit[1:2], dcs$delta_pred[1:2])
})
test_that("equals lower ci from deltacomp package", {
  expect_equal(ps$lower_CI[1:2], dcs$ci_lo[1:2])
})
test_that("equals upper ci from deltacomp package", {
  expect_equal(ps$upper_CI[1:2], dcs$ci_up[1:2])
})
