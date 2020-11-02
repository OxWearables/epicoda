test_that("scaling works for compositional mean", {
  expect_equal(comp_mean(data = simdata, comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                         rounded_zeroes = TRUE, units = "hr/day"), 24*comp_mean(data = simdata, comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                                                                rounded_zeroes = TRUE))
})

test_that("user-specified scaling works for compositional mean", {
  expect_equal(comp_mean(data = simdata, comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                         rounded_zeroes = TRUE, units = "specified", specified_units = c("test", 29)), 29*comp_mean(data = simdata, comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                                                                rounded_zeroes = TRUE))
  })

test_that("error thrown if units specified wrongly", {
  expect_error(comp_mean(data = simdata, comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                         rounded_zeroes = TRUE, units = "min/month"))
})
