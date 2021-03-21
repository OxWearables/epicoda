test_fv <- data.frame("test_cat" = rep("hello", by = 100), "test_cont" = rep(1, by = 100))

test_that("fixed value output_as_expected", {
  expect_equal(epicoda:::generate_fixed_values(test_fv, comp_labels = NA)$test_cat, "hello")
})
test_that("fixed value output_as_expected", {
  expect_equal(epicoda:::generate_fixed_values(test_fv, comp_labels = NA)$test_cont, 1)
})

fv <- epicoda:::generate_fixed_values(simdata, comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
test_that("error if from part not in comp_labels", {
  expect_error(epicoda:::make_new_data( from_part = "BMI",
                                        to_part = "sedentary",
                                        dataset = simdata,
                                        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv))
})

cm_df <- as.data.frame(comp_mean(simdata, c("vigorous", "moderate", "light", "sedentary", "sleep"), units = "hr/day"))
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

