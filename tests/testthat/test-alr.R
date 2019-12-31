test_that("alr returns error messages if reference component not specified", {
  expect_error(epicoda::alr_trans(data = simdata, comp_labels = c("partA", "partB", "partC", "partD", "partE")))
})

