# Warning in appropriate case ========================================
test_that("warning if try to exponentiate coefficients from linear model", {
  expect_warning(tab_coefs(scale_type = "exp", # This argument can be "lp" or "exp" and determines whether
                          # coefficients are presented on the scale of the linear predictors ("lp")
                          # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
                          # logistic regression models and the Hazard Ratio for Cox regression models.
                         level = 0.95,
                         type = "linear",
                         outcome = "BMI",
                         covariates = c("agegroup", "sex"),
                         data = simdata[1:10, ],
                         comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep" ), rounded_zeroes = FALSE))
})

# Set up data =================================================
cov_vec <- c("agegroup", "sex")
comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
sim_mini <- epicoda::simdata[1:2000, ] # Use smaller data to speed up

mini_dat2 <- sim_mini
mini_dat2$sex <- stats::rbinom(nrow(mini_dat2),1,  prob = (mini_dat2$sleep/24 + mini_dat2$light/24)+ mini_dat2$disease*(mini_dat2$sedentary/24)) # The idea of this is to try to make sex into an important confounder just to check the equality not driven by lack of correlation
mini_dat2$sex[mini_dat2$sex == 0] <- "Female"
mini_dat2$sex[mini_dat2$sex == 1] <- "Male"

# Look at 2 datasets
for (mini_dat in list(sim_mini, mini_dat2)){
  model1 <- epicoda::comp_model(type = "linear",
                                outcome = "BMI",
                                covariates = cov_vec,
                                data = mini_dat,
                                comp_labels = comp_labels, rounded_zeroes = FALSE)
  tab0 <- tab_coefs(scale_type = "lp", # This argument can be "lp" or "exp" and determines whether
                    # coefficients are presented on the scale of the linear predictors ("lp")
                    # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
                    # logistic regression models and the Hazard Ratio for Cox regression models.
                    level = 0.95,
                    type = "linear",
                    outcome = "BMI",
                    covariates = cov_vec,
                    data = mini_dat,
                    comp_labels = comp_labels, rounded_zeroes = FALSE)
  model2 <- epicoda::comp_model(type = "logistic",
                                outcome = "disease",
                                covariates = cov_vec,
                                data = mini_dat,
                                comp_labels = comp_labels, rounded_zeroes = FALSE)
  tab1 <- epicoda::tab_coefs(scale_type = "lp", # This argument can be "lp" or "exp" and determines whether
                    # coefficients are presented on the scale of the linear predictors ("lp")
                    # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
                    # logistic regression models and the Hazard Ratio for Cox regression models.
                    level = 0.95,
                    type = "logistic",
                    outcome = "disease",
                    covariates = cov_vec,
                    data = mini_dat,
                    comp_labels = comp_labels, rounded_zeroes = FALSE)
  tab2 <- epicoda::tab_coefs(scale_type = "exp", # This argument can be "lp" or "exp" and determines whether
                             # coefficients are presented on the scale of the linear predictors ("lp")
                             # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
                             # logistic regression models and the Hazard Ratio for Cox regression models.
                             level = 0.95,
                             type = "logistic",
                             outcome = "disease",
                             covariates = cov_vec,
                             data = mini_dat,
                             comp_labels = comp_labels, rounded_zeroes = FALSE)

  model3 <- epicoda::comp_model(type = "cox",
                                event = "event",
                                follow_up_time = "follow_up_time",
                                covariates = cov_vec,
                                data = mini_dat,
                                comp_labels = comp_labels, rounded_zeroes = FALSE)
  model4 <- epicoda::comp_model(type = "cox",
                                event = "event",
                                follow_up_time = "follow_up_time",
                                covariates = cov_vec,
                                data = mini_dat,
                                comp_labels =c("sleep", "moderate", "light", "vigorous", "sedentary"), rounded_zeroes = FALSE)
  tab3 <- tab_coefs(scale_type = "exp", # This argument can be "lp" or "exp" and determines whether
            # coefficients are presented on the scale of the linear predictors ("lp")
            # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
            # logistic regression models and the Hazard Ratio for Cox regression models.
            level = 0.95,
            type = "cox",
            event = "event",
            follow_up_time = "follow_up_time",
            covariates = cov_vec,
            data = mini_dat,
            comp_labels = comp_labels, rounded_zeroes = FALSE)

  # Get out numbers which should match
  est <- as.numeric(as.vector(summary(model1)$coef["ilr_1_vigorous_vs_parts_2_to_5", 1]))
  t_val <-  qt(0.975, model1$df.residual)
  se <-  as.numeric(as.vector(summary(model1)$coef["ilr_1_vigorous_vs_parts_2_to_5", 2]))
  lci <- est - t_val*se
  uci <- est + t_val*se
  vec0a <- c(est, lci, uci)
  vec0b <- as.numeric(as.vector(tab0[c("First pivot coordinate: vigorous vs All other parts"), ]))
  names(vec0b) <- NULL

  # Get out numbers which should match
  est <- as.numeric(as.vector(summary(model2)$coef["ilr_1_vigorous_vs_parts_2_to_5", 1]))
  z_val <-  qnorm(0.975)
  se <-  as.numeric(as.vector(summary(model2)$coef["ilr_1_vigorous_vs_parts_2_to_5", 2]))
  lci <- est - z_val*se
  uci <- est + z_val*se
  vec1a <- c(est, lci, uci)
  vec1b <- as.numeric(as.vector(tab1[c("First pivot coordinate: vigorous vs All other parts"), ]))
  names(vec1b) <- NULL

  vec2a <- as.numeric(as.vector(questionr::odds.ratio(model2)["ilr_1_vigorous_vs_parts_2_to_5", 1:3]))
  names(vec2a) <- NULL
  vec2b <- as.numeric(as.vector(tab2[c("First pivot coordinate: vigorous vs All other parts"), ]))
  names(vec2b) <- NULL

  # Get out numbers which should match
  vec3a <- as.numeric(as.vector(summary(model3)$conf.int["ilr_1_vigorous_vs_parts_2_to_5", c(1, 3, 4)]))
  names(vec3a) <- NULL
  vec3b <- as.numeric(as.vector(tab3[c("First pivot coordinate: vigorous vs All other parts"), ]))
  names(vec3b) <- NULL
  vec4a <- as.numeric(as.vector(summary(model4)$conf.int["ilr_1_sleep_vs_parts_2_to_5", c(1, 3, 4)]))
  names(vec4a) <- NULL
  vec4b <- as.numeric(as.vector(tab3[c("First pivot coordinate: sleep vs All other parts"), ]))
  names(vec4b) <- NULL


  test_that("coefficients from tab_coefs match those from model output", {
    expect_equal(vec0a, vec0b)
  })


  test_that("coefficients from tab_coefs match those from model output", {
    expect_equal(vec1a, vec1b, tolerance = 0.01) # Only expect approx equality here as comparing profile likelihood with Wald intervals
  })

  test_that("coefficients from tab_coefs match those from model output", {
    expect_equal(vec2a, vec2b)
  })
  test_that("coefficients from tab_coefs match those from model output", {
    expect_equal(vec3a, vec3b)
  })
  test_that("coefficients from tab_coefs match those from model output", {
    expect_equal(vec4a, vec4b)
  })
}

# COVARIATE COEFFICIENTS ==========================================================================================
for (mini_dat in list(sim_mini, mini_dat2)){
  model1 <- epicoda::comp_model(type = "linear",
                                outcome = "BMI",
                                covariates = cov_vec,
                                data = mini_dat,
                                comp_labels = comp_labels, rounded_zeroes = FALSE)
  tab0 <- tab_coefs(scale_type = "lp", # This argument can be "lp" or "exp" and determines whether
                    # coefficients are presented on the scale of the linear predictors ("lp")
                    # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
                    # logistic regression models and the Hazard Ratio for Cox regression models.
                    level = 0.95,
                    type = "linear",
                    outcome = "BMI",
                    covariates = cov_vec,
                    data = mini_dat,
                    comp_labels = comp_labels, rounded_zeroes = FALSE)
  model2 <- epicoda::comp_model(type = "logistic",
                                outcome = "disease",
                                covariates = cov_vec,
                                data = mini_dat,
                                comp_labels = comp_labels, rounded_zeroes = FALSE)
  tab1 <- epicoda::tab_coefs(scale_type = "lp", # This argument can be "lp" or "exp" and determines whether
                             # coefficients are presented on the scale of the linear predictors ("lp")
                             # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
                             # logistic regression models and the Hazard Ratio for Cox regression models.
                             level = 0.95,
                             type = "logistic",
                             outcome = "disease",
                             covariates = cov_vec,
                             data = mini_dat,
                             comp_labels = comp_labels, rounded_zeroes = FALSE)
  tab2 <- epicoda::tab_coefs(scale_type = "exp", # This argument can be "lp" or "exp" and determines whether
                             # coefficients are presented on the scale of the linear predictors ("lp")
                             # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
                             # logistic regression models and the Hazard Ratio for Cox regression models.
                             level = 0.95,
                             type = "logistic",
                             outcome = "disease",
                             covariates = cov_vec,
                             data = mini_dat,
                             comp_labels = comp_labels, rounded_zeroes = FALSE)

  model3 <- epicoda::comp_model(type = "cox",
                                event = "event",
                                follow_up_time = "follow_up_time",
                                covariates = cov_vec,
                                data = mini_dat,
                                comp_labels = comp_labels, rounded_zeroes = FALSE)
  model4 <- epicoda::comp_model(type = "cox",
                                event = "event",
                                follow_up_time = "follow_up_time",
                                covariates = cov_vec,
                                data = mini_dat,
                                comp_labels =c("sleep", "moderate", "light", "vigorous", "sedentary"), rounded_zeroes = FALSE)
  tab3 <- tab_coefs(scale_type = "exp", # This argument can be "lp" or "exp" and determines whether
                    # coefficients are presented on the scale of the linear predictors ("lp")
                    # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
                    # logistic regression models and the Hazard Ratio for Cox regression models.
                    level = 0.95,
                    type = "cox",
                    event = "event",
                    follow_up_time = "follow_up_time",
                    covariates = cov_vec,
                    data = mini_dat,
                    comp_labels = comp_labels, rounded_zeroes = FALSE)

  # Get out numbers which should match
  est <- as.numeric(as.vector(summary(model1)$coef["sexMale", 1]))
  t_val <-  qt(0.975, model1$df.residual)
  se <-  as.numeric(as.vector(summary(model1)$coef["sexMale", 2]))
  lci <- est - t_val*se
  uci <- est + t_val*se
  vec0a <- c(est, lci, uci)
  vec0b <- as.numeric(as.vector(tab0[c("sexMale"), ]))
  names(vec0b) <- NULL

  # Get out numbers which should match
  est <- as.numeric(as.vector(summary(model2)$coef["sexMale", 1]))
  z_val <-  qnorm(0.975)
  se <-  as.numeric(as.vector(summary(model2)$coef["sexMale", 2]))
  lci <- est - z_val*se
  uci <- est + z_val*se
  vec1a <- c(est, lci, uci)
  vec1b <- as.numeric(as.vector(tab1[c("sexMale"), ]))
  names(vec1b) <- NULL

  vec2a <- as.numeric(as.vector(questionr::odds.ratio(model2)["sexMale", 1:3]))
  names(vec2a) <- NULL
  vec2b <- as.numeric(as.vector(tab2[c("sexMale"), ]))
  names(vec2b) <- NULL

  # Get out numbers which should match
  vec3a <- as.numeric(as.vector(summary(model3)$conf.int["sexMale", c(1, 3, 4)]))
  names(vec3a) <- NULL
  vec3b <- as.numeric(as.vector(tab3[c("sexMale"), ]))
  names(vec3b) <- NULL


  test_that("coefficients from tab_coefs match those from model output", {
    expect_equal(vec0a, vec0b)
  })


  test_that("coefficients from tab_coefs match those from model output", {
    expect_equal(vec1a, vec1b, tolerance = (vec1b[3] - vec1b[2])/20 )# Only expect approx equality here as comparing profile likelihood with Wald intervals - use a heuristic based on interval width
  })

  test_that("coefficients from tab_coefs match those from model output", {
    expect_equal(vec2a, vec2b)
  })
  test_that("coefficients from tab_coefs match those from model output", {
    expect_equal(vec3a, vec3b)
  })

}
