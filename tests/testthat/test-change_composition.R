comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
sd1mod <- simdata[1, comp_labels]
sd1mod[, "light"] <- simdata[1, "light"] -1
sd1mod[, "sedentary"] <- simdata[1, "sedentary"] + 1

test_that("change_composition reallocates as expected", {
  expect_equal(change_composition(simdata[1, comp_labels], comp_labels = comp_labels, main_change = +1 , main_part = "sedentary", at_expense_of = "light"), sd1mod)
})


test_that("error if change results in compositional parts < 0", {
  expect_error(change_composition(simdata[1, comp_labels], comp_labels = comp_labels, main_change = -20 , main_part = "sedentary", at_expense_of = "light"))
})
