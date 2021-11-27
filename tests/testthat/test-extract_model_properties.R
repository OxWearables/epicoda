lm_outcome <- comp_model(
  type = "linear",
  outcome = "BMI",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = 0.00119
)

log_outcome <- comp_model(
  type = "logistic",
  outcome = "disease",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = 0.00119
)
cox_outcome <- comp_model(
  type = "cox",
  event = "event",
  follow_up_time = "follow_up_time",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = 0.00119
)

comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
tl <- transf_labels(comp_labels = comp_labels, transformation_type = "ilr")

sd <- transform_comp(simdata, comp_labels = comp_labels, rounded_zeroes = TRUE, det_limit = 0.00119)
d <- get_dataset_from_model(model = lm_outcome, comp_labels = comp_labels,
                            transf_labels = tl, type = "linear")

test_that("dataset is same as if generate by hand", {
  expect_gt(length(colnames(d)), 0)
  expect_equal(d, sd[, colnames(d)] )
})
d <- get_dataset_from_model(model = log_outcome, comp_labels = comp_labels,
                            transf_labels = tl, type = "logistic")

test_that("dataset is same as if generate by hand", {
  expect_gt(length(colnames(d)), 0)
  expect_equal(d, sd[, colnames(d)] )
})

d <- get_dataset_from_model(model = cox_outcome, comp_labels = comp_labels,
                            transf_labels = tl, type = "cox")

test_that("dataset is same as if generate by hand", {
  expect_gt(length(colnames(d)), 0)
  expect_equal(d, sd[, colnames(d)] )
})

cm <- comp_mean(simdata, comp_labels = comp_labels, det_limit = 0.00119)
d <- get_cm_from_model(model = lm_outcome, comp_labels = comp_labels,
                            transf_labels = tl)$cm

test_that("comp mean is same as if generate by hand", {
  expect_equal(d, cm)
})

d <- get_cm_from_model(model = log_outcome, comp_labels = comp_labels,
                       transf_labels = tl)$cm

test_that("comp mean is same as if generate by hand", {
  expect_equal(d, cm)
})

d <- get_cm_from_model(model = cox_outcome, comp_labels = comp_labels,
                       transf_labels = tl)$cm

test_that("comp mean is same as if generate by hand", {
  expect_equal(d, cm)
})
