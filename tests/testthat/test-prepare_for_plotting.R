test_fv <- data.frame("test_cat" = rep("hello", by = 100), "test_cont" = rep(1, by = 100))

test_that("fixed value output_as_expected", {
  expect_equal(epicoda:::generate_fixed_values(test_fv, comp_labels = NA)$test_cat, "hello")
})
test_that("fixed value output_as_expected", {
  expect_equal(epicoda:::generate_fixed_values(test_fv, comp_labels = NA)$test_cont, 1)
})

fv <- epicoda:::generate_fixed_values(simdata, comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
test_that("error if from part not in comp_labels", {
  expect_error(epicoda:::make_new_data(from_part = "BMI",
                                        to_part = "sedentary",
                                        dataset = simdata,
                                        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv))
})

# CHECK MAKE NEW DATA BEHAVING AS EXPECTED ===========================================================
dat_for_test <- data.frame(a = (1:11)/30, b = seq(21/30, 10/30, length.out = 11), c = 1 - seq(21/30, 10/30, length.out = 11) - (1:11)/30 )
cm <-  comp_mean(dat_for_test, comp_labels = c("a", "b", "c"))
new_dat <- epicoda:::make_new_data(from_part = "a",
                                   to_part = "b",
                                   dataset = dat_for_test,
                                   lower_quantile = 0.2,
                                   upper_quantile = 0.8,
                                   comp_labels = c("a", "b", "c"), fixed_values = cm, granularity = 5, units = "unitless")
hm_new_dat <- data.frame(a = seq(3/30, 9/30, length.out = 5), b = cm[["a"]] + cm[["b"]] - seq(3/30, 9/30, length.out = 5), c = cm[["c"]])
rownames(new_dat) <- NULL
rownames(hm_new_dat) <- NULL
test_that("new_data behaves as expected", {
  expect_equal(hm_new_dat, new_dat)
})
new_dat_flipped <- epicoda:::make_new_data(from_part = "b",
                                   to_part = "a",
                                   dataset = dat_for_test,
                                   lower_quantile = 0.2,
                                   upper_quantile = 0.8,
                                   comp_labels = c("a", "b", "c"), fixed_values = cm, granularity = 5, units = "unitless")
rownames(new_dat_flipped) <- NULL
test_that("new_data behaves as expected", {
  expect_equal(hm_new_dat, new_dat_flipped)
})

# New data sums to appropriate scale
cm_df <- as.data.frame(epicoda::comp_mean(epicoda::simdata, comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"), units = "hr/day", det_limit = 0.00119))
n <- epicoda:::make_new_data(from_part = "moderate",
                        to_part = "sedentary",
                        dataset = simdata,
                        lower_quantile = 0.2,
                        upper_quantile = 0.8,
                        comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"), fixed_values = cm_df, units = "min/day", granularity = 5)
n2 <- epicoda:::make_new_data(from_part = "moderate",
                             to_part = "sedentary",
                             dataset = epicoda::simdata,
                             lower_quantile = 0.2,
                             upper_quantile = 0.8,
                             comp_labels =c("vigorous", "moderate", "light", "sedentary"), fixed_values = cm_df, units = "min/wk", granularity = 5)
rownames(n) <- NULL
rownames(n2) <- NULL
test_that("rowsums of new data as expected", {
  expect_equal(rowSums(n[, c("vigorous", "moderate", "light", "sedentary", "sleep")]) , rep(24*60, 5))
})

test_that("rowsums of new data as expected", {
  expect_equal(rowSums(n2[, c("vigorous", "moderate", "light", "sedentary")]) , rep(24*60*7, 5))
})

# TEST THE RANGE OF OUTPUT ========================================================================================================================
fv[, c("vigorous", "moderate", "light", "sedentary", "sleep")] <- cm_df
q1 <- quantile(simdata$moderate, 0.05)[[1]]
q2 <- quantile(simdata$moderate, 0.95)[[1]]
q3 <- quantile(simdata$vigorous, 0.05)[[1]]
q4 <- quantile(simdata$vigorous, 0.95)[[1]]
q5 <- cm_df$sedentary - (q1 - cm_df$moderate)
q6 <- cm_df$sedentary - (q2 - cm_df$moderate)
q7 <- cm_df$moderate - (q3- cm_df$vigorous)
q8 <- cm_df$moderate - (q4 - cm_df$vigorous)
test_that("correct range of min data", {
  expect_equal(min(epicoda:::make_new_data(from_part = "sedentary",
                                        to_part = "moderate",
                                        dataset = simdata,
                                        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day")$moderate) , q1 )
})

test_that("correct range of min data", {
  expect_equal(max(epicoda:::make_new_data( from_part = "sedentary",
                                            to_part = "moderate",
                                            dataset = simdata,
                                            comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day")$moderate) , q2 )
})

test_that("correct range of min data", {
  expect_equal(min(epicoda:::make_new_data(from_part = "vigorous",
                                           to_part = "moderate",
                                           dataset = simdata,
                                           comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day")$vigorous) , q3 )
})

test_that("correct range of min data", {
  expect_equal(max(epicoda:::make_new_data( from_part = "vigorous",
                                            to_part = "moderate",
                                            dataset = simdata,
                                            comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day")$vigorous) , q4 )
})

test_that("correct range of min data", {
  expect_equal(min(epicoda:::make_new_data(from_part = "sedentary",
                                           to_part = "moderate",
                                           dataset = simdata,
                                           comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day")$sedentary) , q6 )
})

test_that("correct range of min data", {
  expect_equal(max(epicoda:::make_new_data( from_part = "sedentary",
                                            to_part = "moderate",
                                            dataset = simdata,
                                            comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day")$sedentary) , q5 )
})

test_that("correct range of min data", {
  expect_equal(min(epicoda:::make_new_data(from_part = "vigorous",
                                           to_part = "moderate",
                                           dataset = simdata,
                                           comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day")$moderate) , q8 )
})

test_that("correct range of min data", {
  expect_equal(max(epicoda:::make_new_data( from_part = "vigorous",
                                            to_part = "moderate",
                                            dataset = simdata,
                                            comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day")$moderate) , q7 )
})


cm_df <- as.data.frame(comp_mean(simdata, c("vigorous", "moderate", "light", "sedentary", "sleep"), units = "hr/day", det_limit = 0.00119))
fv[, c("vigorous", "moderate", "light", "sedentary", "sleep")] <- cm_df
q1 <- quantile(simdata$moderate, 0.1)[[1]]
q2 <- quantile(simdata$moderate, 0.9)[[1]]
q3 <- quantile(simdata$vigorous, 0.1)[[1]]
q4 <- quantile(simdata$vigorous, 0.9)[[1]]
q5 <- cm_df$sedentary - (q1 - cm_df$moderate)
q6 <- cm_df$sedentary - (q2 - cm_df$moderate)
q7 <- cm_df$moderate - (q3- cm_df$vigorous)
q8 <- cm_df$moderate - (q4 - cm_df$vigorous)
test_that("correct range of min data", {
  expect_equal(min(epicoda:::make_new_data(from_part = "sedentary",
                                           to_part = "moderate",
                                           dataset = simdata,
                                           comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day", lower_quantile = 0.1, upper_quantile = 0.9)$moderate) , q1 )
})

test_that("correct range of min data", {
  expect_equal(max(epicoda:::make_new_data( from_part = "sedentary",
                                            to_part = "moderate",
                                            dataset = simdata,
                                            comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day", lower_quantile = 0.1, upper_quantile = 0.9)$moderate) , q2 )
})

test_that("correct range of min data", {
  expect_equal(min(epicoda:::make_new_data(from_part = "vigorous",
                                           to_part = "moderate",
                                           dataset = simdata,
                                           comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day", lower_quantile = 0.1, upper_quantile = 0.9)$vigorous) , q3 )
})

test_that("correct range of min data", {
  expect_equal(max(epicoda:::make_new_data( from_part = "vigorous",
                                            to_part = "moderate",
                                            dataset = simdata,
                                            comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day", lower_quantile = 0.1, upper_quantile = 0.9)$vigorous) , q4 )
})

test_that("correct range of min data", {
  expect_equal(min(epicoda:::make_new_data(from_part = "sedentary",
                                           to_part = "moderate",
                                           dataset = simdata,
                                           comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day", lower_quantile = 0.1, upper_quantile = 0.9)$sedentary) , q6 )
})

test_that("correct range of min data", {
  expect_equal(max(epicoda:::make_new_data( from_part = "sedentary",
                                            to_part = "moderate",
                                            dataset = simdata,
                                            comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day", lower_quantile = 0.1, upper_quantile = 0.9)$sedentary) , q5 )
})

test_that("correct range of min data", {
  expect_equal(min(epicoda:::make_new_data(from_part = "vigorous",
                                           to_part = "moderate",
                                           dataset = simdata,
                                           comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day", lower_quantile = 0.1, upper_quantile = 0.9)$moderate) , q8 )
})

test_that("correct range of min data", {
  expect_equal(max(epicoda:::make_new_data( from_part = "vigorous",
                                            to_part = "moderate",
                                            dataset = simdata,
                                            comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day", lower_quantile = 0.1, upper_quantile = 0.9)$moderate) , q7 )
})

