comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "linear",
           outcome = "BMI",
           covariates = c("agegroup", "sex"),
           data = simdata,
           comp_labels = comp_labels,
           transformation_type = "ilr"), file = "example_model.rds")
})


test_that("Error if use clr coordinates.", {
  expect_error(epicoda::comp_model(type = "linear",
                                                outcome = "BMI",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata,
                                                comp_labels = comp_labels,
                                                transformation_type = "clr"))
})

test_that("Error if use transformation which does not exist.", {
  expect_error(epicoda::comp_model(type = "linear",
                                    outcome = "BMI",
                                    covariates = c("agegroup", "sex"),
                                    data = simdata,
                                    comp_labels = comp_labels,
                                    transformation_type = "made-up transformation"))
})

test_that("Error if no outcome.", {
  expect_error(epicoda::comp_model(type = "linear",
                                    covariates = c("agegroup", "sex"),
                                    data = simdata,
                                    comp_labels = comp_labels,
                                    transformation_type = "ilr"))
})

test_that("Error if composition not specified correctly.", {
  expect_error(epicoda::comp_model(type = "linear",
                                    outcome = "BMI",
                                    covariates = c("agegroup", "sex"),
                                    data = simdata,
                                    comp_labels = "sleep",
                                    transformation_type = "ilr"))
})
