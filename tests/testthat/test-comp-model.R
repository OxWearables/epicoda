comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")

# Cached models --------------------------------------------------------
test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "linear",
           outcome = "BMI",
           covariates = c("agegroup", "sex"),
           data = simdata,
           comp_labels = comp_labels), file = "../test_data/example_model.rds")
})

test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "linear",
                                                outcome = "BMI",
                                                data = simdata,
                                                comp_labels = comp_labels), file = "../test_data/example_model_null.rds")
})

test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "logistic",
                                                outcome = "disease",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata,
                                                comp_labels = comp_labels), file = "../test_data/example_model_logistic.rds")
})

test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "logistic",
                                                outcome = "disease",
                                                data = simdata,
                                                comp_labels = comp_labels), file = "../test_data/example_model_logistic_null.rds")
})

test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "cox",
                                                follow_up_time = "follow_up_time",
                                                event = "event",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata,
                                                comp_labels = comp_labels), file = "../test_data/example_model_cox.rds")
})

test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "cox",
                                                follow_up_time = "follow_up_time",
                                                event = "event",
                                                data = simdata,
                                                comp_labels = comp_labels), file = "../test_data/example_model_cox_null.rds")
})

# Errors when expected ---------------------------------------------------------------------------------------------
test_that("Error if no outcome.", {
  expect_error(epicoda::comp_model(type = "linear",
                                    covariates = c("agegroup", "sex"),
                                    data = simdata,
                                    comp_labels = comp_labels))
})

test_that("Error if composition not specified correctly.", {
  expect_error(epicoda::comp_model(type = "linear",
                                    outcome = "BMI",
                                    covariates = c("agegroup", "sex"),
                                    data = simdata,
                                    comp_labels = "sleep"))
})

# Behaviours as expected ------------------------------------------------------------------------------------------
for (cov_vec in list(NULL, c("agegroup"), c("agegroup", "sex"))){{
  for (comp_labels in list(c("light", "sleep"), c("light", "moderate", "sleep"), c("light", "moderate", "sleep", "sedentary", "vigorous"))){
    model1 <- epicoda::comp_model(type = "linear",
                                  outcome = "BMI",
                                  covariates = cov_vec,
                                  data = simdata,
                                  comp_labels = comp_labels)
    model2 <- epicoda::comp_model(type = "logistic",
                                  outcome = "disease",
                                  covariates = cov_vec,
                                  data = simdata,
                                  comp_labels = comp_labels)
    model3 <- epicoda::comp_model(type = "cox",
                                  event = "event",
                                  follow_up_time = "follow_up_time",
                                  covariates = cov_vec,
                                  data = simdata,
                                  comp_labels = comp_labels)
    n_coef <- length(comp_labels) - 1
    if ("agegroup" %in% cov_vec){
      n_coef <- n_coef + 4
    }
    if ("sex" %in% cov_vec){
      n_coef <- n_coef + 1
    }
    test_that("Expected number of coefficients", {
      expect_equal(length(coef(model1)), n_coef + 1)
      expect_equal(length(coef(model2)), n_coef + 1)
      expect_equal(length(coef(model3)), n_coef)
      })  }
}}

# Two ways of producing same model are equivalent -------------------------------------------------
m1 <- epicoda::comp_model(type = "cox",
                    follow_up_time = "follow_up_time",
                    event = "event",
                    covariates = c("agegroup", "sex"),
                    data = simdata,
                    comp_labels = comp_labels)
m2 <- epicoda::comp_model(type = "cox",
                          outcome  = survival::Surv(simdata$follow_up_time, simdata$event),
                          covariates = c("agegroup", "sex"),
                          data = simdata,
                          comp_labels = comp_labels)
test_that("Two ways of producing same model are equivalent", {
  expect_equal(coef(m1), coef(m2))
})


