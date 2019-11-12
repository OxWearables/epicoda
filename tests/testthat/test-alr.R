test_that("alr returns error messages if reference component not specifified", {
  expect_error(alr(data = example_data, comp_labels = c("1st_comp", "2nd_comp", "3rd_comp")))
})
