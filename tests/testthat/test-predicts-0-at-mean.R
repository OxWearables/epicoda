comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
m <- comp_model(
  type = "linear",
  data = simdata,
  outcome = "BMI",
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
