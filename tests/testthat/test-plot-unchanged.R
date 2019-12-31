test_that("Plot unchanged from cached plot.", {
  expect_equal_to_reference(epicoda::plot_transfers(from_part = "partD",
                                                             to_part = "partB",
                                                             model =
                                                             lm(linear_outcome ~ , simdata),
                                                             dataset = simdata,
                                                             transformation_type = "ilr",
                                                             comp_labels = c("partA", "partB", "partC", "partD", "partE"),
                                                             y_label = NULL,
                                                             units = "hr/day",
                                                             rounded_zeroes = TRUE,
                                                             det_limit = 0.0083,
                                                             terms = TRUE,
                                                            plot_log = TRUE))
})
