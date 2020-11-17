test_that("output same as input in relevant way", {
  expect_equal(transform_comp(data = simdata,
                              comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
                              transformation_type = "ilr", units = "hr/day")[, c("vigorous", "moderate", "light", "sedentary", "sleep")],
               simdata[, c("vigorous", "moderate", "light", "sedentary", "sleep")])
})


test_that("error if specify comparison part with ilr", {
  expect_error(transform_comp(data = simdata,
                              comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
                              transformation_type = "ilr", comparison_part = "moderate"))
})
