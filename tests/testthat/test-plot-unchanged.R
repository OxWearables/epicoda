comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
test_that("Plot unchanged from cached plot.", {
  expect_equal_to_reference(epicoda::plot_transfers(from_part = "sleep",
                                                             to_part = "moderate",
                                                             model =
                                                      epicoda::comp_model(type = "linear",
                                                                          outcome = "BMI",
                                                                          covariates = c("agegroup", "sex"),
                                                                          follow_up_time = NULL,
                                                                          event = NULL,
                                                                          data = simdata,
                                                                          comp_labels = comp_labels,
                                                                          transformation_type = "ilr"),
                                                             dataset = simdata,
                                                             transformation_type = "ilr",
                                                             comp_labels =  c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                                             y_label = NULL,
                                                             units = "hr/day",
                                                             rounded_zeroes = TRUE,
                                                             det_limit = 0.0083,
                                                             terms = TRUE,
                                                            plot_log = FALSE), file = "example_plot.rds")
})
