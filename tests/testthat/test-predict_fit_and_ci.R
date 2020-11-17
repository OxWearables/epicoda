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
? comp_model
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
