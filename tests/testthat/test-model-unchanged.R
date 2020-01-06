test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "linear",
           outcome = "linear_outcome",
           covariates = c("agegroup", "sex"),
           follow_up_time = NULL,
           event = NULL,
           data = simdataplain,
           comp_labels = comp_labels,
           transformation_type = "ilr",
           rounded_zeroes = TRUE,
           det_limit = 0.0083,
           comparison_part = NULL,
           part_1 = NULL))
})

