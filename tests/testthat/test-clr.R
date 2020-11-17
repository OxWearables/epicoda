comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
sd_zf <- simdata[simdata$vigorous != 0,]
for (label in comp_labels){
  sd_zf <- sd_zf[sd_zf[, label] != 0, ]
}
rownames(sd_zf) <- NULL
rownames(simdata) <- NULL
test_that("clr transformation is correctly inverted", {
  expect_equal(clr_trans_inv(clr_trans(sd_zf[, c("vigorous", "moderate", "light", "sedentary", "sleep")])), (1/24)*sd_zf[, c("vigorous", "moderate", "light", "sedentary", "sleep")])
})
