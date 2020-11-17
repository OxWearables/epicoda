test_that("warning if try to exponentiate coefficients from linear model", {
  expect_warning(tab_coefs(scale_type = "exp", # This argument can be "lp" or "exp" and determines whether
                          # coefficients are presented on the scale of the linear predictors ("lp")
                          # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
                          # logistic regression models and the Hazard Ratio for Cox regression models.
                         level = 0.95,
                         type = "linear",
                         outcome = "BMI",
                         covariates = c("agegroup", "sex"),
                         data = simdata,
                         comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep" )))
})

