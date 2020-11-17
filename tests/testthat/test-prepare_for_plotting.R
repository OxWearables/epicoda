test_fv <- data.frame("test_cat" = rep("hello", by = 100), "test_cont" = rep(1, by = 100))

test_that("fixed value output_as_expected", {
  expect_equal(epicoda:::generate_fixed_values(test_fv, comp_labels = NA)$test_cat, "hello")
})
test_that("fixed value output_as_expected", {
  expect_equal(epicoda:::generate_fixed_values(test_fv, comp_labels = NA)$test_cont, 1)
})

test_that("error if from part not in comp_labels", {
  expect_error(epicoda:::make_new_data(  from_part = "wrong",
                                        to_part = "sedentary",
                                        dataset = simdata,
                                        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")))
})

