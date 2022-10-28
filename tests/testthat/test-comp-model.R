simdata <- epicoda::simdata
comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
min_val_in_data <- min(simdata$vigorous[simdata$vigorous > 0])

# Cached models --------------------------------------------------------
test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "linear",
           outcome = "BMI",
           covariates = c("agegroup", "sex"),
           data = simdata,
           comp_labels = comp_labels, det_limit = min_val_in_data), file = "../test_data/example_model.rds")
})

test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "linear",
                                                outcome = "BMI",
                                                data = simdata,
                                                comp_labels = comp_labels, det_limit = min_val_in_data), file = "../test_data/example_model_null.rds")
})

test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "logistic",
                                                outcome = "disease",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata,
                                                comp_labels = comp_labels, det_limit = min_val_in_data), file = "../test_data/example_model_logistic.rds")
})

test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "logistic",
                                                outcome = "disease",
                                                data = simdata,
                                                comp_labels = comp_labels, det_limit = min_val_in_data), file = "../test_data/example_model_logistic_null.rds")
})

test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "cox",
                                                follow_up_time = "follow_up_time",
                                                event = "event",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata,
                                                comp_labels = comp_labels, det_limit = min_val_in_data), file = "../test_data/example_model_cox.rds")
})

test_that("Model unchanged from cached model.", {
  expect_equal_to_reference(epicoda::comp_model(type = "cox",
                                                follow_up_time = "follow_up_time",
                                                event = "event",
                                                data = simdata,
                                                comp_labels = comp_labels, det_limit = min_val_in_data), file = "../test_data/example_model_cox_null.rds")
})



# TEST THAT DET LIMIT PASSES CORRECTLY WITH RESCALING--------------------------------------------
simdata_scaled1 <- simdata
simdata_scaled2<- simdata
simdata_scaled1[, comp_labels] <- (1/24)*simdata[, comp_labels]
simdata_scaled2[, comp_labels] <- 35*simdata[, comp_labels]
min_val_in_data_scaled1 <- (1/24)*min_val_in_data
min_val_in_data_scaled2 <- (35)*min_val_in_data

test_that("Model invariant to scaling of data and det_limit.", {
  expect_equal_to_reference(epicoda::comp_model(type = "linear",
                                                outcome = "BMI",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata_scaled1,
                                                comp_labels = comp_labels, det_limit = min_val_in_data_scaled1), file = "../test_data/example_model.rds")
})


test_that("Model invariant to scaling of data and det_limit.", {
  expect_equal_to_reference(epicoda::comp_model(type = "linear",
                                                outcome = "BMI",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata_scaled2,
                                                comp_labels = comp_labels, det_limit = min_val_in_data_scaled2), file = "../test_data/example_model.rds")
})


test_that("Model invariant to scaling of data and det_limit.", {
  expect_equal_to_reference(epicoda::comp_model(type = "logistic",
                                                outcome = "disease",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata_scaled1,
                                                comp_labels = comp_labels, det_limit = min_val_in_data_scaled1), file = "../test_data/example_model_logistic.rds")
})

test_that("Model invariant to scaling of data and det_limit.", {
  expect_equal_to_reference(epicoda::comp_model(type = "logistic",
                                                outcome = "disease",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata_scaled2,
                                                comp_labels = comp_labels, det_limit = min_val_in_data_scaled2), file = "../test_data/example_model_logistic.rds")
})


test_that("Model invariant to scaling of data and det_limit.", {
  expect_equal_to_reference(epicoda::comp_model(type = "cox",
                                                follow_up_time = "follow_up_time",
                                                event = "event",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata_scaled1,
                                                comp_labels = comp_labels, det_limit = min_val_in_data_scaled1), file = "../test_data/example_model_cox.rds")
})



test_that("Model invariant to scaling of data and det_limit.", {
  expect_equal_to_reference(epicoda::comp_model(type = "cox",
                                                follow_up_time = "follow_up_time",
                                                event = "event",
                                                covariates = c("agegroup", "sex"),
                                                data = simdata_scaled2,
                                                comp_labels = comp_labels, det_limit = min_val_in_data_scaled2), file = "../test_data/example_model_cox.rds")
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
                                    comp_labels = "sleep", det_limit = min_val_in_data))
})

# Behaviours as expected ------------------------------------------------------------------------------------------
for (cov_vec in list(NULL, c("agegroup"), c("agegroup", "sex"))){{
  for (comp_labels in list(c("light", "sleep"), c("light", "moderate", "sleep"), c("light", "moderate", "sleep", "sedentary", "vigorous"))){
    model1 <- epicoda::comp_model(type = "linear",
                                  outcome = "BMI",
                                  covariates = cov_vec,
                                  data = simdata,
                                  comp_labels = comp_labels, rounded_zeroes = FALSE)
    model2 <- epicoda::comp_model(type = "logistic",
                                  outcome = "disease",
                                  covariates = cov_vec,
                                  data = simdata,
                                  comp_labels = comp_labels, rounded_zeroes = FALSE)
    model3 <- epicoda::comp_model(type = "cox",
                                  event = "event",
                                  follow_up_time = "follow_up_time",
                                  covariates = cov_vec,
                                  data = simdata,
                                  comp_labels = comp_labels, rounded_zeroes = FALSE)
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
                    comp_labels = comp_labels, det_limit = min_val_in_data)
m2 <- epicoda::comp_model(type = "cox",
                          outcome  = survival::Surv(simdata$follow_up_time, simdata$event),
                          covariates = c("agegroup", "sex"),
                          data = simdata,
                          comp_labels = comp_labels, det_limit = min_val_in_data)
test_that("Two ways of producing same model are equivalent", {
  expect_equal(coef(m1), coef(m2))
})

