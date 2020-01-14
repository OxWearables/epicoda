test_that("multiplication works", {
  expect_equal(0, predict_fit_and_ci(
    model = comp_model(
      type = "linear",
      data = simdata,
      outcome = "linear_outcome",
      covariates = c("agegroup", "sex"),
      comp_labels = c("partA", "partB", "partC", "partD", "partE"),
      comp_mean
    ),

  ))
})
