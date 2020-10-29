test_that("clr transformation is correctly inverted", {
  expect_equal(clr_trans_inv(clr_trans(simdata[, c("vigorous", "moderate", "light", "sedentary", "sleep")])), (1/24)*simdata[, c("vigorous", "moderate", "light", "sedentary", "sleep")])
})
