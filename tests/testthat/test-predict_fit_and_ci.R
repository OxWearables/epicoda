min_val_in_data <- min(simdata$vigorous[simdata$vigorous >0 ])
lm_outcome <- comp_model(
  type = "linear",
  outcome = "BMI",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = min_val_in_data
)

log_outcome <- comp_model(
  type = "logistic",
  outcome = "disease",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = min_val_in_data
)
cox_outcome <- comp_model(
  type = "cox",
  event = "event",
  follow_up_time = "follow_up_time",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = min_val_in_data
)

old_comp <- comp_mean(
  simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
  units = "hr/day", det_limit = min_val_in_data
)
new_comp <-
  change_composition(
    composition = old_comp,
    main_part = "moderate",
    main_change = +0.5,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
  )

test_that("expected order of confidence interval limits", {
  expect_gt(
    predict_fit_and_ci(
      model = cox_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$upper_CI, predict_fit_and_ci(
    model = cox_outcome,
    terms = FALSE,
    new_data = new_comp,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
  )$fit )
})

test_that("expected order of confidence interval limits", {
  expect_lt(
    predict_fit_and_ci(
      model = cox_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$lower_CI, predict_fit_and_ci(
      model = cox_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})

test_that("expected order of confidence interval limits", {
  expect_gt(
    predict_fit_and_ci(
      model = lm_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$upper_CI, predict_fit_and_ci(
      model = lm_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})

test_that("expected order of confidence interval limits", {
  expect_lt(
    predict_fit_and_ci(
      model = lm_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$lower_CI, predict_fit_and_ci(
      model = lm_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})

test_that("expected order of confidence interval limits", {
  expect_gt(
    predict_fit_and_ci(
      model = log_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$upper_CI, predict_fit_and_ci(
      model = log_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})

test_that("expected order of confidence interval limits", {
  expect_lt(
    predict_fit_and_ci(
      model = log_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$lower_CI, predict_fit_and_ci(
      model = log_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})


test_that("input scale of new composition doesn't matter", {
  expect_equal(
    predict_fit_and_ci(
      model = log_outcome,
      terms = FALSE,
      new_data = new_comp*24,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit, predict_fit_and_ci(
      model = log_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
})


test_that("wrong labels for composition throws error", {
  expect_error(
    predict_fit_and_ci(
      model = log_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary")
    ))
})

test_that("wrong labels for composition throws error", {
  expect_error(
    predict_fit_and_ci(
      model = log_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep", "extra")
    ))
})
test_that("wrong labels for composition throws error", {
  expect_error(
    predict_fit_and_ci(
      model = log_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("viggy", "moderate", "light", "sedentary", "sleep", "extra")
    ))
})

#==============
# Test against deltacomp package

# setup
comp_labels <- c("sl", "sb", "lpa", "mvpa")
fat_data <- deltacomp::fat_data

## deltacomp predictions
dcs <- deltacomp::predict_delta_comps(
  dataf = fat_data,
  y = "fat",
  comps = comp_labels,
  covars = c("sibs", "parents", "ed"),
  deltas = seq(-60, 60, by = 5) / (24 * 60),
  comparisons = "one-v-one",
  alpha = 0.05
)

## epicoda model
epic_linear <- comp_model(type = "linear", data = fat_data, rounded_zeroes = FALSE, outcome = "fat",comp_labels = comp_labels ,covariates = c("sibs", "parents", "ed"))

## set up new data
newdata <- data.frame(matrix(ncol = 0, nrow = 2))
newdata$sb <- c(comp_mean(fat_data, comp_labels = comp_labels)$sb - (-1/24), comp_mean(fat_data, comp_labels = comp_labels)$sb)
newdata$sl <-  c(comp_mean(fat_data, comp_labels = comp_labels)$sl + (-1/24), comp_mean(fat_data, comp_labels = comp_labels)$sl + (-1/24))
newdata$lpa <- c(comp_mean(fat_data, comp_labels = comp_labels)$lpa, comp_mean(fat_data, comp_labels = comp_labels)$lpa + 1/24)
newdata$mvpa <- c(comp_mean(fat_data, comp_labels = comp_labels)$mvpa, comp_mean(fat_data, comp_labels = comp_labels)$mvpa)

## epicoda predictions
ps <- predict_fit_and_ci(epic_linear, newdata, comp_labels = comp_labels)

## tests
test_that("equals results from deltacomp package", {
  expect_equal(ps$fit[1:2], dcs$delta_pred[1:2])
})
test_that("equals lower ci from deltacomp package", {
  expect_equal(ps$lower_CI[1:2], dcs$ci_lo[1:2])
})
test_that("equals upper ci from deltacomp package", {
  expect_equal(ps$upper_CI[1:2], dcs$ci_up[1:2])
})

## new set up, new data
comp_labels <- c("sed", "sleep", "lpa", "mvpa")
fat_data <- deltacomp::fairclough

## deltacomp predictions, one one reallocations
dcs <- deltacomp::predict_delta_comps(
  dataf = fat_data,
  y = "z_bmi",
  comps = comp_labels,
  covars = c("shuttles_20m", "height"),
  deltas = seq(-10, 10, by = 5) / (24 * 60),
  comparisons = "one-v-one",
  alpha = 0.05
)

## deltacomp predictions, proportional reallocations
dcs2 <- deltacomp::predict_delta_comps(
  dataf = fat_data,
  y = "z_bmi",
  comps = comp_labels,
  covars = c("shuttles_20m", "height"),
  deltas = seq(-10, 10, by = 5) / (24 * 60),
  comparisons = "prop-realloc",
  alpha = 0.05
)

## epicoda model
epic_linear <- comp_model(type = "linear", data = fat_data, rounded_zeroes = FALSE, outcome = "z_bmi",comp_labels = comp_labels,covariates = c("shuttles_20m", "height"))

## set up new data NB this also tests change_composition function
cd <- as.data.frame(comp_mean(fat_data, comp_labels = comp_labels))
newdata <- change_composition(cd, main_change = -1/(24*6), main_part = "sed", at_expense_of = "sleep", comp_labels = comp_labels)
newdata[2, comp_labels] <- change_composition(cd, main_change = -1/(24*6), main_part = "sed", at_expense_of = "lpa", comp_labels = comp_labels)
newdata[3, comp_labels] <- change_composition(cd, main_change = -1/(24*6), main_part = "sed", comp_labels = comp_labels) # no at_expense_of argument defaults to all

## epicoda predictions
ps <- predict_fit_and_ci(epic_linear, newdata, comp_labels = comp_labels)

## tests
test_that("equals results from deltacomp package", {
  expect_equal(ps$fit[1:2], dcs$delta_pred[1:2])
})
test_that("equals lower ci from deltacomp package", {
  expect_equal(ps$lower_CI[1:2], dcs$ci_lo[1:2])
})
test_that("equals upper ci from deltacomp package", {
  expect_equal(ps$upper_CI[1:2], dcs$ci_up[1:2])
})
test_that("equals results from deltacomp package", {
  expect_equal(ps$fit[3], dcs2$delta_pred[1])
})
test_that("equals lower ci from deltacomp package", {
  expect_equal(ps$lower_CI[3], dcs2$ci_lo[1])
})
test_that("equals upper ci from deltacomp package", {
  expect_equal(ps$upper_CI[3], dcs2$ci_up[1])
})

#==============
# Test various aspects of expected behaviour

fv <- epicoda:::generate_fixed_values(simdata, comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
cm_df <- as.data.frame(comp_mean(simdata, c("vigorous", "moderate", "light", "sedentary", "sleep"), units = "hr/day", det_limit = min_val_in_data))
fv[, c("vigorous", "moderate", "light", "sedentary", "sleep")] <- cm_df
md <- epicoda:::make_new_data(from_part = "sedentary",
                        to_part = "moderate",
                        dataset = simdata,
                        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day")

comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
m3 <- comp_model(
  type = "cox",
  data = simdata,
  follow_up_time = "follow_up_time",
  event = "event",
  comp_labels = comp_labels,
  rounded_zeroes = FALSE
)

m4 <- comp_model(
  type = "cox",
  data = epicoda::simdata,
  follow_up_time = "follow_up_time",
  event = "event",
  comp_labels = comp_labels,
  covariates = c("agegroup"),
  rounded_zeroes = FALSE
)

t_yes <- predict_fit_and_ci(m3, md, comp_labels = comp_labels)
t_no <- predict_fit_and_ci(m3, md, comp_labels = comp_labels , terms = FALSE)

t_yes_cov <- predict_fit_and_ci(m4, md, comp_labels = comp_labels)
t_no_cov <- predict_fit_and_ci(m4, md, comp_labels = comp_labels , terms = FALSE)

test_that("when no covariates, two CI types for Cox are same ", {
  expect_equal(t_yes$fit, t_no$fit)
})

test_that("when no covariates, two CI types for Cox are same ", {
  expect_equal(as.vector(t_yes$lower_CI), as.vector(t_no$lower_CI))
})

test_that("when no covariates, two CI types for Cox are same ", {
  expect_equal(as.vector(t_yes$upper_CI), as.vector(t_no$upper_CI))
})

test_that("when covariates, two CI types for Cox are same", {
  expect_equal(as.vector((log(t_yes$upper_CI) - log(t_yes$fit))/1.96), as.vector(t_no$se.fit), tolerance = 0.00001 )
  })

test_that("when covariates, two CI types for Cox are different", {
  expect_false(isTRUE(all.equal(as.vector(t_yes_cov$upper_CI), as.vector(t_no_cov$upper_CI))))
})

test_that("when covariates, two CI types for Cox are different", {
  expect_false(isTRUE(all.equal(as.vector((log(t_yes_cov$upper_CI) - log(t_yes_cov$fit))/1.96), as.vector(t_no_cov$se.fit),  tolerance = 0.00001 )))
})

#==============
# Test known invariance properties
m5 <- comp_model(
  type = "cox",
  data = epicoda::simdata,
  follow_up_time = "follow_up_time",
  event = "event",
  comp_labels = comp_labels,
  covariates = c("agegroup"),
  part_1 = "sedentary",
  rounded_zeroes = FALSE
)

m6 <- comp_model(
  type = "cox",
  data = epicoda::simdata,
  follow_up_time = "follow_up_time",
  event = "event",
  comp_labels = comp_labels,
  covariates = c("agegroup"),
  part_1 = "sleep",
  rounded_zeroes = FALSE
)

t_p1_sed <- predict_fit_and_ci(m5, md, comp_labels = comp_labels, part_1 = "sedentary")
t_p1_sleep <- predict_fit_and_ci(m6, md, comp_labels = comp_labels, part_1 = "sleep")

test_that("order of ilr parts doesn't matter for global compositional predictions", {
  expect_equal(t_p1_sed$fit,t_p1_sleep$fit)
})

test_that("order of ilr parts doesn't matter - CI ", {
  expect_equal(as.vector(t_p1_sed$lower_CI), as.vector(t_p1_sleep$lower_CI))
})

test_that("order of ilr parts doesn't matter - CI ", {
  expect_equal(as.vector(t_p1_sed$upper_CI), as.vector(t_p1_sleep$upper_CI))
})
