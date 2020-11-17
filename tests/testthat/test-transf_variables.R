test_that("error if fail to specify transformation type", {
  expect_error(epicoda::transf_variables(comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")))
})

test_that("vector_to_sum gives expected output", {
  expect_equal(vector_to_sum(c("a", "b")), "a+b")
})
