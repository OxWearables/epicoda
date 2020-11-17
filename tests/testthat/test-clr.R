sd_zf <- simdata[simdata$vigorous != 0,]
test_that("clr transformation is correctly inverted", {
  expect_equal(clr_trans_inv(clr_trans(sd_zf[, c("vigorous", "moderate", "light", "sedentary", "sleep")])), (1/24)*sd_zf[, c("vigorous", "moderate", "light", "sedentary", "sleep")])
})
