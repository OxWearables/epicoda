comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
sd_zf <- simdata[simdata$vigorous != 0,]
for (label in comp_labels){
  sd_zf <- sd_zf[sd_zf[, label] != 0, ]
}
rownames(sd_zf) <- NULL
d <- transform_comp(data = simdata,
                   comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
                   transformation_type = "ilr", units = "hr/day", rounded_zeroes = FALSE)[, c("vigorous", "moderate", "light", "sedentary", "sleep")]
rownames(d) <- NULL
test_that("output same as input in relevant way", {
  expect_equal(d,
               sd_zf[, comp_labels])
})


test_that("error if specify comparison part with ilr", {
  expect_error(transform_comp(data = simdata,
                              comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
                              transformation_type = "ilr", comparison_part = "moderate"))
})
