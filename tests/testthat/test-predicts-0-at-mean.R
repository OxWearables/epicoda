comp_labels <- c("partA", "partB", "partC", "partD", "partE")
test_that("prediction at the comp mean is 0", {
  expect_equal(0,
               predict_fit_and_ci(
                 model = comp_model(
                   type = "linear",
                   data = simdata,
                   outcome = "linear_outcome",
                   covariates = c("agegroup", "sex"),
                   comp_labels = c("partA", "partB", "partC", "partD", "partE"),
                   rounded_zeroes = TRUE
                 ),
                 dataset = simdata,
                 new_data = comp_mean(
                   data = simdata,
                   rounded_zeroes = TRUE,
                   comp_labels = comp_labels
                 ), comp_labels = comp_labels, rounded_zeroes = TRUE, terms = TRUE, transformation_type = "ilr")[1, "fit"])
})
