lm_outcome <- comp_model(type = "linear",
outcome = "BMI",
covariates = c("agegroup", "sex"),
data = simdata,
comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))

test_that("error if try to use parts not appearing in comp_labels", {
  expect_error(epicoda::plot_transfers(from_part = "sedentary",
                                       to_part = "BMI",
                                       model = lm_outcome ,
                                       dataset = simdata,
                                       transformation_type = "ilr",
                                       comp_labels =c("vigorous", "light", "sedentary", "sleep"),
                                       y_label = "Model-predicted difference in BMI",
                                       units = "hr/day",
                                       terms = TRUE))
})

test_that("error if try to use wrong comp_labels", {
  expect_error(epicoda::plot_transfers(from_part = "sedentary",
                                       to_part = "moderate",
                                       model = lm_outcome ,
                                       dataset = simdata,
                                       transformation_type = "ilr",
                                       comp_labels =c("vigorous", "light", "sedentary", "sleep"),
                                       y_label = "Model-predicted difference in BMI",
                                       units = "hr/day",
                                       terms = TRUE))
})

simdata_add <- simdata
for (lab in c("vigorous", "moderate", "light", "sedentary", "sleep")){
  simdata_add[, lab] <- 24*simdata[, lab]
}

test_that("result unchanged if different input data scale for compositional columns", {
  expect_equal(epicoda::plot_transfers(from_part = "sedentary",
                                       to_part = "moderate",
                                       model = lm_outcome ,
                                       dataset = simdata_add,
                                       transformation_type = "ilr",
                                       comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                       y_label = "Model-predicted difference in BMI",
                                       units = "hr/day",
                                       terms = TRUE), epicoda::plot_transfers(from_part = "sedentary",
                                                                              to_part = "moderate",
                                                                              model = lm_outcome ,
                                                                              dataset = simdata,
                                                                              transformation_type = "ilr",
                                                                              comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                                                              y_label = "Model-predicted difference in BMI",
                                                                              units = "hr/day",
                                                                              terms = TRUE))
})

test_that("result unchanged ", {
  expect_equal_to_reference(epicoda::plot_transfers(from_part = "sedentary",
                                       to_part = "moderate",
                                       model = lm_outcome ,
                                       dataset = simdata,
                                       transformation_type = "ilr",
                                       comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                       y_label = "Model-predicted difference in BMI",
                                       units = "hr/day",
                                       terms = TRUE), "../test_data/plot_example.RDS")
})

