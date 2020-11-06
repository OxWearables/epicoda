comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
d <- readRDS("../test_data/alr_output.RDS")

test_that("Output of alr transformation agrees with reference", {
  expect_equal(alr_trans(simdata[1 ,comp_labels], comp_labels, "sleep"),d )
})
test_that("Throws error if comparison part incorrectly specified", {
  expect_error(alr_trans(simdata[1 ,comp_labels], comp_labels, "not_a_column"))
})

test_that("Throws error if comp_labels incorrectly specified", {
  expect_error(alr_trans(simdata[1 ,comp_labels], c("not_the_right_columns")))
})

test_that("Throws error if comparison_part not specified", {
  expect_error(epicoda::alr_trans(data = simdata, comp_labels = comp_labels))
})


