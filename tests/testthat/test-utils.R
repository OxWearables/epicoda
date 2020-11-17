test_that("mode returns mode", {
  expect_equal(epicoda:::Mode(c(2,2,3)), 2)
})


test_that("gm behaves as expected", {
  expect_equal(epicoda:::gm(c(2,1,4)), 2)
})

df <- data.frame(matrix(nrow = 2, ncol = 0))
df$newcol <- c(1/sqrt(2), -1/sqrt(2))
m <- as.matrix(df)

test_that("create_transformation_matrix behaves as expected in two dimensions", {
  expect_equal(epicoda:::create_transformation_matrix(2), m)
})


test_that("error if part_1 not in specified comp_labels", {
  expect_error(epicoda:::alter_order_comp_labels(c("label1", "label2"), "label3"))
})


test_that("error if specified_units is incorrect", {
  expect_error(epicoda:::process_units(units = "specified", specified_units = "in wrong form"))
})

test_that("error if units not one of permitted descriptions", {
  expect_error(epicoda:::process_units(units = "made up name"))
})

data_with_zero <- simdata[1,]
data_with_zero[1, "vigorous"] <- 0

test_that("when rounded_zeroes is FALSE, zeroes are removed", {
  expect_equal(nrow(epicoda:::process_zeroes(data_with_zero, comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), rounded_zeroes = FALSE)), 0)
})
