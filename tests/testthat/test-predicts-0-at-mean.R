comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
test_that("prediction at the comp mean is 0", {
  expect_equal(0,
               predict_fit_and_ci(
                 model = comp_model(
                   type = "linear",
                   data = simdata,
                   outcome = "BMI",
                   covariates = c("agegroup", "sex"),
                   comp_labels = comp_labels,
                   rounded_zeroes = TRUE
                 ),
                 dataset = simdata,
                 new_data = comp_mean(
                   data = simdata,
                   rounded_zeroes = TRUE,
                   comp_labels = comp_labels
                 ), comp_labels = comp_labels, rounded_zeroes = TRUE, terms = TRUE, transformation_type = "ilr")[1, "fit"])
})
