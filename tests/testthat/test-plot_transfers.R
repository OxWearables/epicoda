min_val_in_data <- min(simdata$vigorous[simdata$vigorous > 0])
lm_outcome <- comp_model(type = "linear",
outcome = "BMI",
covariates = c("agegroup", "sex"),
data = simdata,
comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = min_val_in_data)

test_that("error if try to use parts not appearing in comp_labels", {
  expect_error(epicoda::plot_transfers(from_part = "sedentary",
                                       to_part = "BMI",
                                       model = lm_outcome ,
                                       comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                       y_label = "Model-predicted difference in BMI",
                                       units = "hr/day",
                                       terms = TRUE))
})

test_that("error if try to use wrong comp_labels", {
  expect_error(epicoda::plot_transfers(from_part = "sedentary",
                                       to_part = "moderate",
                                       model = lm_outcome ,
                                       comp_labels =c("vigorous", "light", "sedentary", "sleep"),
                                       y_label = "Model-predicted difference in BMI",
                                       units = "hr/day",
                                       terms = TRUE))
})



test_that("result unchanged ", {
  expect_equal_to_reference(epicoda::plot_transfers(from_part = "sedentary",
                                       to_part = "moderate",
                                       model = lm_outcome ,
                                       comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                       y_label = "Model-predicted difference in BMI",
                                       units = "hr/day",
                                       terms = TRUE), "../test_data/plot_example.RDS")
})

test_that("error if compositional parts in different order ", {
  expect_error(epicoda::plot_transfers(from_part = "sedentary",
                                                    to_part = "moderate",
                                                    model = lm_outcome ,
                                                    comp_labels =c("vigorous", "moderate",  "sedentary","light", "sleep"),
                                                    y_label = "Model-predicted difference in BMI",
                                                    units = "hr/day",
                                                    terms = TRUE))
})
