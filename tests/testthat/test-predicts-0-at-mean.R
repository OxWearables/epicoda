comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
m <- comp_model(
  type = "linear",
  data = simdata,
  outcome = "BMI",
  covariates = c("agegroup", "sex"),
  comp_labels = comp_labels,
  rounded_zeroes = TRUE
)
m2 <- comp_model(
  type = "logistic",
  data = simdata,
  outcome = "disease",
  covariates = c("agegroup", "sex"),
  comp_labels = comp_labels,
  rounded_zeroes = TRUE
)
m3 <- comp_model(
  type = "cox",
  data = simdata,
  outcome = survival::Surv(simdata$follow_up_time, simdata$event),
  covariates = c("agegroup", "sex"),
  comp_labels = comp_labels,
  rounded_zeroes = TRUE
)
nd <- comp_mean(
  data = simdata,
  rounded_zeroes = TRUE,
  comp_labels = comp_labels
)
test_that("prediction at the comp mean is 0", {
  expect_equal(0,
               predict_fit_and_ci(
                 model = m,
                 new_data = nd, comp_labels = comp_labels,terms = TRUE)[1, "fit"])
})

test_that("prediction at the comp mean is 0", {
  expect_equal(0,
               as.numeric(predict_fit_and_ci(
                 model = m,
                 new_data = nd, comp_labels = comp_labels,terms = TRUE)[1, "upper_CI"]))
})


test_that("prediction at the comp mean is 1 - logistic", {
  expect_equal(1,
               predict_fit_and_ci(
                 model = m2,
                 new_data = nd, comp_labels = comp_labels,terms = TRUE)[1, "fit"])
})


test_that("prediction at the comp mean is 1 - logistic", {
  expect_equal(1,
               as.numeric(predict_fit_and_ci(
                 model = m2,
                 new_data = nd, comp_labels = comp_labels,terms = TRUE)[1, "lower_CI"]))
})

test_that("prediction at the comp mean is 1 - cox", {
  expect_equal(1,
               predict_fit_and_ci(
                 model = m3,
                 new_data = nd, comp_labels = comp_labels,terms = TRUE)[1, "fit"])
})

test_that("prediction at the comp mean is 1 - cox", {
  expect_equal(1,
               as.numeric(predict_fit_and_ci(
                 model = m3,
                 new_data = nd, comp_labels = comp_labels,terms = TRUE)[1, "upper_CI"]))
})
