comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
d <- 24*ilr_trans_inv(ilr_trans(simdata[, comp_labels]))
colnames(d) <- comp_labels

test_that("ilr inversion inverts ilr", {
  expect_equal(d, simdata[, comp_labels])
})

test_that("ilr fails if data not just compositional columns", {
  expect_error(ilr_trans(simdata))
})
