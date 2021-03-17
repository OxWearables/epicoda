test_that("mode returns mode", {
  expect_equal(epicoda:::Mode(c(2,2,3)), 2)
})

test_that("mode returns mode", {
  expect_equal(epicoda:::Mode(c(2,2,3, 3)), c(2, 3))
})

test_that("gm behaves as expected", {
  expect_equal(epicoda:::gm(c(2,1,4)), 2)
})

test_that("gm behaves as expected", {
  expect_equal(epicoda:::gm(c(12, 3, 6, 6)), 6)
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


test_that("error when a different kind of model is presented to process_model_type", {
  expect_error(process_model_type(glm(event ~ agegroup, family="poisson", data=simdata)))
})

test_that("error when incorrect arguments to process_axis_label", {
  expect_error(process_axis_label("hello"))
})


comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
sd_zf <- simdata[simdata$vigorous != 0,]
for (label in comp_labels){
  sd_zf <- sd_zf[sd_zf[, label] != 0, ]
}
rownames(sd_zf) <- NULL
nc <- normalise_comp(sd_zf[,comp_labels], comp_labels = comp_labels)
test_that("normalised data should sum to 1", {
  expect_equal(rep(1, times = nrow(nc)), apply(nc, 1, sum))
})

nc_messed_up <- nc
nc_messed_up[1:1000, comp_labels] <- 22*nc_messed_up[1:1000, comp_labels]
rc <- rescale_comp(nc, comp_labels, 24)
rownames(rc) <- NULL

test_that("rescaling normalised composition can get back to input", {
  expect_equal(rc, sd_zf[, comp_labels])
})


test_that("warning if normalise data not on a single scale", {
  expect_warning(rescale_comp(nc_messed_up, comp_labels, 24))
})

test_that("rescale_det_limit throws error if no clear scale", {
  expect_message(rescale_det_limit(nc_messed_up, comp_labels, 0.001))
})






















