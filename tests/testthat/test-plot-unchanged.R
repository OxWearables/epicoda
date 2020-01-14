comp_labels <- c("partA", "partB", "partC", "partD", "partE")
test_that("Plot unchanged from cached plot.", {
  expect_equal_to_reference(epicoda::plot_transfers(from_part = "partD",
                                                             to_part = "partB",
                                                             model =
                                                      epicoda::comp_model(type = "linear",
                                                                          outcome = "linear_outcome",
                                                                          covariates = c("agegroup", "sex"),
                                                                          follow_up_time = NULL,
                                                                          event = NULL,
                                                                          data = simdata,
                                                                          comp_labels = comp_labels,
                                                                          transformation_type = "ilr",
                                                                          rounded_zeroes = TRUE,
                                                                          det_limit = 0.0083,
                                                                          comparison_part = NULL,
                                                                          part_1 = NULL),
                                                             dataset = simdata,
                                                             transformation_type = "ilr",
                                                             comp_labels = c("partA", "partB", "partC", "partD", "partE"),
                                                             y_label = NULL,
                                                             units = "hr/day",
                                                             rounded_zeroes = TRUE,
                                                             det_limit = 0.0083,
                                                             terms = TRUE,
                                                            plot_log = FALSE), file = "example_plot.rds")
})
