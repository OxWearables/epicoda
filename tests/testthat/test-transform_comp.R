install.packages("robCompositions")
comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
sd_zf <- simdata[simdata$vigorous != 0,]
for (label in comp_labels){
  sd_zf <- sd_zf[sd_zf[, label] != 0, ]
}
rownames(sd_zf) <- NULL
d <- transform_comp(data = simdata,
                   comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
                   transformation_type = "ilr", rounded_zeroes = FALSE)[, c("vigorous", "moderate", "light", "sedentary", "sleep")]
rownames(d) <- NULL
d[, comp_labels] <- 24*d[, comp_labels]
test_that("output same as input in relevant way", {
  expect_equal(d,
               sd_zf[, comp_labels])
})


test_that("error if specify comparison part with ilr", {
  expect_error(transform_comp(data = simdata,
                              comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
                              transformation_type = "ilr", comparison_part = "moderate"))
})

test_that("output same as input in relevant way", {
  expect_equal(d,
               sd_zf[, comp_labels])
})

test_that("output matches that of pivotCoord", {
  expect_equal(transform_comp(data = simdata[1:5, c("vigorous", "moderate", "light", "sedentary", "sleep")],
                              comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
                              transformation_type = "ilr", part_1 = "moderate", rounded_zeroes = FALSE)[1:5, "ilr_1_moderate_vs_parts_2_to_5"], robCompositions::pivotCoord(simdata[1:5, c("vigorous", "moderate", "light", "sedentary", "sleep")],pivotvar = 2)[1:5, 1])
})

test_that("output matches that of pivotCoord 2", {
  expect_equal(transform_comp(data = simdata[1:5, c("light", "sedentary", "sleep")],
                              comp_labels = c("light", "sedentary", "sleep"),
                              transformation_type = "ilr", part_1 = "sleep", rounded_zeroes = FALSE)[1:5, "ilr_1_sleep_vs_parts_2_to_3"], robCompositions::pivotCoord(simdata[1:5, c("light", "sedentary", "sleep")],pivotvar = 3)[1:5, 1])
})

test_that("output matches that of pivotCoord complete", {
  t1 <- transform_comp(data = simdata[1:5, c("light", "sedentary", "sleep")],
                       comp_labels = c("light", "sedentary", "sleep"),
                       transformation_type = "ilr", rounded_zeroes = FALSE)[1:5, ]
  t1 <- t1[, (ncol(t1) -1):ncol(t1)]
  t2 <- robCompositions::pivotCoord(simdata[1:5, c("light", "sedentary", "sleep")])[1:5,]
  colnames(t2) <- colnames(t1)
  expect_equal(t1, t2)
})

x <- exp(rnorm(5))
y <- exp(rnorm(5, x))
z <- rep(3, by = 5)
ddum <- data.frame(cbind(x = x, y = y, z = z))
test_that("output matches that of pivotCoord dummy input", {
  t1 <- transform_comp(data = ddum,
                       comp_labels = c("x", "y", "z"),
                       transformation_type = "ilr", rounded_zeroes = FALSE)[2:5, 4:5]
  t2 <- robCompositions::pivotCoord(ddum)[2:5,1:2]
  colnames(t1) <- colnames(t2)

  expect_equal(t1, t2)
})
