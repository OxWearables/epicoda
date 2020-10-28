comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "linear",
           outcome = "BMI",
           covariates = c("agegroup", "sex"),
           follow_up_time = NULL,
           event = NULL,
           data = simdata,
           comp_labels = comp_labels,
           transformation_type = "ilr"), file = "example_model.rds")
})

