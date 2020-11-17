comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
sd_zf <- simdata[simdata$vigorous != 0,]

for (label in comp_labels){
  sd_zf <- sd_zf[sd_zf[, label] != 0, ]
}
rownames(sd_zf) <- NULL

test_that("result of alr transformation unchanged from reference", {
  expect_equal_to_reference(alr_trans(sd_zf[1 ,comp_labels], comp_labels, "sleep"), "../test_data/alr_output.RDS")
})


test_that("Throws error if comparison part incorrectly specified", {
  expect_error(alr_trans(sd_zf[1 ,comp_labels], comp_labels, "not_a_column"))
})

test_that("Throws error if comp_labels incorrectly specified", {
  expect_error(alr_trans(sd_zf[1 ,comp_labels], c("not_the_right_columns")))
})

test_that("Throws error if comparison_part not specified", {
  expect_error(epicoda::alr_trans(data = sd_zf, comp_labels = comp_labels))
})


