comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
sd_zf <- simdata[simdata$vigorous != 0,]
for (label in comp_labels){
  sd_zf <- sd_zf[sd_zf[, label] != 0, ]
}
d <- 24*ilr_trans_inv(ilr_trans(sd_zf[, comp_labels]))
colnames(d) <- comp_labels
rownames(d) <- NULL
rownames(sd_zf) <- NULL
test_that("ilr inversion inverts ilr", {
  expect_equal(d, sd_zf[, comp_labels])
})

test_that("ilr fails if data not just compositional columns", {
  expect_error(ilr_trans(sd_zf))
})
