# SET UP MODELS=======================================================
simdata <- epicoda::simdata
min_val_in_data <- min(simdata$vigorous[simdata$vigorous >0 ])
lm_outcome <- comp_model(
  type = "linear",
  outcome = "BMI",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = min_val_in_data
)

logistic_outcome <- comp_model(
  type = "logistic",
  outcome = "disease",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = min_val_in_data
)
cox_outcome <- comp_model(
  type = "cox",
  event = "event",
  follow_up_time = "follow_up_time",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = min_val_in_data
)

old_comp <- comp_mean(
  simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
  units = "hr/day", det_limit = min_val_in_data
)
new_comp1 <-
  change_composition(
    composition = old_comp,
    main_part = "moderate",
    main_change = +0.5,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
  )


new_comp2 <- new_comp1 # This is to also try with a composition which isn't just proportional
new_comp2$light <- new_comp1$light + 0.6
new_comp2$sedentary <- new_comp1$sedentary - 0.6


for (new_comp in list(new_comp1, new_comp2)){
  # BASIC EXPECTED PROPERTIES=======================================================
  test_that("expected order of confidence interval limits", {
    expect_gt(
      predict_fit_and_ci(
        model = cox_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$upper_CI, predict_fit_and_ci(
      model = cox_outcome,
      terms = FALSE,
      new_data = new_comp,
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
    )$fit )
  })

  test_that("expected order of confidence interval limits", {
    expect_lt(
      predict_fit_and_ci(
        model = cox_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$lower_CI, predict_fit_and_ci(
        model = cox_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$fit )
  })

  test_that("expected order of confidence interval limits", {
    expect_gt(
      predict_fit_and_ci(
        model = lm_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$upper_CI, predict_fit_and_ci(
        model = lm_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$fit )
  })

  test_that("expected order of confidence interval limits", {
    expect_lt(
      predict_fit_and_ci(
        model = lm_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$lower_CI, predict_fit_and_ci(
        model = lm_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$fit )
  })

  test_that("expected order of confidence interval limits", {
    expect_gt(
      predict_fit_and_ci(
        model = logistic_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$upper_CI, predict_fit_and_ci(
        model = logistic_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$fit )
  })

  test_that("expected order of confidence interval limits", {
    expect_lt(
      predict_fit_and_ci(
        model = logistic_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$lower_CI, predict_fit_and_ci(
        model = logistic_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$fit )
  })


  test_that("input scale of new composition doesn't matter", {
    expect_equal(
      predict_fit_and_ci(
        model = logistic_outcome,
        terms = FALSE,
        new_data = new_comp*24,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$fit, predict_fit_and_ci(
        model = logistic_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
      )$fit )
  })

  # ERRORS THROWN WHEN EXPECTED===============================================================
  test_that("wrong labels for composition throws error", {
    expect_error(
      predict_fit_and_ci(
        model = logistic_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary")
      ))
  })

  test_that("wrong labels for composition throws error", {
    expect_error(
      predict_fit_and_ci(
        model = logistic_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep", "extra")
      ))
  })
  test_that("wrong labels for composition throws error", {
    expect_error(
      predict_fit_and_ci(
        model = logistic_outcome,
        terms = FALSE,
        new_data = new_comp,
        comp_labels = c("viggy", "moderate", "light", "sedentary", "sleep", "extra")
      ))
  })

  # CORRECT BEHAVIOUR OF TERMS ARGUMENTS WITH FIXED VALUES ARGUMENTS=========================================
  comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
  models_list <- list("linear"= lm_outcome, "logistic" = logistic_outcome, "cox" = cox_outcome)
  for (name in names(models_list)){
    model <- models_list[[name]]
    fixedvalfem <- data.frame("agegroup" = "Middle", "sex" = "Female")
    fixedvalmal <- data.frame("agegroup" = "Middle", "sex" = "Male")

    fitnontermsfem <- predict_fit_and_ci(model, new_comp, comp_labels, terms = FALSE, fixed_values = fixedvalfem)$fit
    fitnontermsmal <- predict_fit_and_ci(model, new_comp, comp_labels, terms = FALSE, fixed_values = fixedvalmal)$fit

    test_that("fixed values work as expected with terms = FALSE", {
      expect_false(isTRUE(all.equal(fitnontermsfem, fitnontermsmal)))
    })

    fittermsfem <- predict_fit_and_ci(model, new_comp, comp_labels, fixed_values = fixedvalfem)$fit
    fittermsmal <- predict_fit_and_ci(model, new_comp, comp_labels, fixed_values = fixedvalmal)$fit

    test_that("fixed values have no effect with terms = TRUE", {
      expect_equal(fittermsfem, fittermsmal)
    })

  }



  # EQUALITY WITH CALCULATION USING COEFFICIENTS=============================================================
  transf_comp <- transform_comp(new_comp, comp_labels = comp_labels)
  tl <- transf_labels(comp_labels, "ilr")
  transf_comp_mean <- get_cm_from_model(lm_outcome, comp_labels = comp_labels, transf_labels = tl)[[2]]
  delta_comp <- transf_comp[, tl] - transf_comp_mean[, tl]

  for (name in names(models_list)){
    model <- models_list[[name]]
    terms_pred <- as.matrix(delta_comp) %*% coef(model)[tl]
    sum_pred <- apply(terms_pred, 1, sum)
    assign(paste0(name, "_sumfit"), sum_pred)
    fitfromfunction <- predict_fit_and_ci(model, new_comp, comp_labels)$fit
    if (name %in% c("logistic", "cox")){
      fitfrombasics <- exp(sum_pred)
    } else if (name %in% c("linear")) {fitfrombasics <- sum_pred}
    test_that("terms predictions for fit correct", {
      expect_equal(fitfromfunction, fitfrombasics)
    })

  }


}

# TEST GET EXPECTED VALUES WHEN WORKING WITH CHANGES AFFECTING A SINGLE COEFFICIENT ================
new_comp3 <-
  change_composition(
    composition = old_comp,
    main_part = "vigorous",
    main_change = +0.5,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
  )
new_comp3$agegroup <- "Middle"
new_comp3$sex <- "Female"
new_comp3 <- transform_comp(new_comp3, comp_labels)

ilr1_diff <- sqrt(4/5)*log(new_comp3$vigorous/((new_comp3$moderate*new_comp3$light*new_comp3$sedentary*new_comp3$sleep)^(1/4))) - sqrt(4/5)*log(old_comp$vigorous/((old_comp$moderate*old_comp$light*old_comp$sedentary*old_comp$sleep)^(1/4)))


for (name in names(models_list)){
  model <- models_list[[name]]
  coefs <- summary(model)$coef
  fitdiff <- coefs[grepl("ilr_1", rownames(coefs)), 1]*ilr1_diff
  fitfromfunction <- predict_fit_and_ci(model, new_comp3, comp_labels)$fit[1]
  uppercifunction <- predict_fit_and_ci(model, new_comp3, comp_labels)$upper_CI[1]
  fitfrompredict_link <- predict(model, type = "terms", terms = "ilr_1_vigorous_vs_parts_2_to_5", se.fit = TRUE, newdata = new_comp3)$fit[1]
  sefrompredict <- predict(model, type = "terms", terms = "ilr_1_vigorous_vs_parts_2_to_5", se.fit = TRUE, newdata = new_comp3)$se.fit[1]
  attr(fitfrompredict_link, "constant") <- NULL
  colnames(fitfrompredict_link) <- NULL
  attr(sefrompredict, "constant") <- NULL
  colnames(sefrompredict) <- NULL

  col_se <- ifelse(name %in% c("linear", "logistic"), 2, 3) # columns of SE sits in different places in fit
  sediff <- coefs[grepl("ilr_1", rownames(coefs)), col_se]*ilr1_diff
  if (name %in% c("logistic", "cox")){
    fitfrombasics <- exp(fitdiff)
    uppercibasics <- exp(fitdiff + sediff*qnorm(0.975))
    fitfrompredict <- exp(fitfrompredict_link)
    uppercifrompredict <- exp(fitfrompredict_link + sefrompredict*qnorm(0.975))
  }
  if (name %in% c("linear")) {
    fitfrombasics <- fitdiff
    uppercibasics <- fitdiff + sediff*qt(0.975, model$df.residual)
    fitfrompredict <- fitfrompredict_link
    uppercifrompredict <- fitfrompredict_link + sefrompredict*qt(0.975, model$df.residual)

  }
  test_that("terms predictions for fit correct", {
     expect_equal(fitfromfunction, fitfrombasics)
  })
  test_that("terms predictions for upper ci correct", {
     expect_equal(uppercifunction, uppercibasics)
  })

  test_that("terms predictions for fit correct", {
    expect_equal(fitfromfunction, fitfrompredict)
  })
  test_that("terms predictions for upper ci correct", {
     expect_equal(uppercifunction, uppercifrompredict)
  })

}


#==============
# Test against deltacomp package

# setup
comp_labels <- c("sl", "sb", "lpa", "mvpa")
fat_data <- deltacomp::fat_data

## deltacomp predictions
dcs <- deltacomp::predict_delta_comps(
  dataf = fat_data,
  y = "fat",
  comps = comp_labels,
  covars = c("sibs", "parents", "ed"),
  deltas = seq(-60, 60, by = 5) / (24 * 60),
  comparisons = "one-v-one",
  alpha = 0.05
)

## epicoda model
epic_linear <- comp_model(type = "linear", data = fat_data, rounded_zeroes = FALSE, outcome = "fat",comp_labels = comp_labels ,covariates = c("sibs", "parents", "ed"))

## set up new data
newdata <- data.frame(matrix(ncol = 0, nrow = 2))
newdata$sb <- c(comp_mean(fat_data, comp_labels = comp_labels)$sb - (-1/24), comp_mean(fat_data, comp_labels = comp_labels)$sb)
newdata$sl <-  c(comp_mean(fat_data, comp_labels = comp_labels)$sl + (-1/24), comp_mean(fat_data, comp_labels = comp_labels)$sl + (-1/24))
newdata$lpa <- c(comp_mean(fat_data, comp_labels = comp_labels)$lpa, comp_mean(fat_data, comp_labels = comp_labels)$lpa + 1/24)
newdata$mvpa <- c(comp_mean(fat_data, comp_labels = comp_labels)$mvpa, comp_mean(fat_data, comp_labels = comp_labels)$mvpa)

## epicoda predictions
ps <- predict_fit_and_ci(epic_linear, newdata, comp_labels = comp_labels)

## tests
test_that("equals results from deltacomp package", {
  expect_equal(ps$fit[1:2], dcs$delta_pred[1:2])
})
test_that("equals lower ci from deltacomp package", {
  expect_equal(ps$lower_CI[1:2], dcs$ci_lo[1:2])
})
test_that("equals upper ci from deltacomp package", {
  expect_equal(ps$upper_CI[1:2], dcs$ci_up[1:2])
})

## new set up, new data
comp_labels <- c("sed", "sleep", "lpa", "mvpa")
fat_data <- deltacomp::fairclough

## deltacomp predictions, one one reallocations
dcs <- deltacomp::predict_delta_comps(
  dataf = fat_data,
  y = "z_bmi",
  comps = comp_labels,
  covars = c("shuttles_20m", "height"),
  deltas = seq(-10, 10, by = 5) / (24 * 60),
  comparisons = "one-v-one",
  alpha = 0.05
)

## deltacomp predictions, proportional reallocations
dcs2 <- deltacomp::predict_delta_comps(
  dataf = fat_data,
  y = "z_bmi",
  comps = comp_labels,
  covars = c("shuttles_20m", "height"),
  deltas = seq(-10, 10, by = 5) / (24 * 60),
  comparisons = "prop-realloc",
  alpha = 0.05
)

## epicoda model
epic_linear <- comp_model(type = "linear", data = fat_data, rounded_zeroes = FALSE, outcome = "z_bmi",comp_labels = comp_labels,covariates = c("shuttles_20m", "height"))

## set up new data NB this also tests change_composition function
cd <- as.data.frame(comp_mean(fat_data, comp_labels = comp_labels))
newdata <- change_composition(cd, main_change = -1/(24*6), main_part = "sed", at_expense_of = "sleep", comp_labels = comp_labels)
newdata[2, comp_labels] <- change_composition(cd, main_change = -1/(24*6), main_part = "sed", at_expense_of = "lpa", comp_labels = comp_labels)
newdata[3, comp_labels] <- change_composition(cd, main_change = -1/(24*6), main_part = "sed", comp_labels = comp_labels) # no at_expense_of argument defaults to all

## epicoda predictions
ps <- predict_fit_and_ci(epic_linear, newdata, comp_labels = comp_labels)

## tests
test_that("equals results from deltacomp package", {
  expect_equal(ps$fit[1:2], dcs$delta_pred[1:2])
})
test_that("equals lower ci from deltacomp package", {
  expect_equal(ps$lower_CI[1:2], dcs$ci_lo[1:2])
})
test_that("equals upper ci from deltacomp package", {
  expect_equal(ps$upper_CI[1:2], dcs$ci_up[1:2])
})
test_that("equals results from deltacomp package", {
  expect_equal(ps$fit[3], dcs2$delta_pred[1])
})
test_that("equals lower ci from deltacomp package", {
  expect_equal(ps$lower_CI[3], dcs2$ci_lo[1])
})
test_that("equals upper ci from deltacomp package", {
  expect_equal(ps$upper_CI[3], dcs2$ci_up[1])
})

#==============
# Test various aspects of expected behaviour

fv <- epicoda:::generate_fixed_values(simdata, comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
cm_df <- as.data.frame(comp_mean(simdata, c("vigorous", "moderate", "light", "sedentary", "sleep"), units = "hr/day", det_limit = min_val_in_data))
fv[, c("vigorous", "moderate", "light", "sedentary", "sleep")] <- cm_df
md <- epicoda:::make_new_data(from_part = "sedentary",
                        to_part = "moderate",
                        dataset = simdata,
                        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),  fixed_values = fv, units = "hr/day")

comp_labels <- c("vigorous", "moderate", "light", "sedentary", "sleep")
m3 <- comp_model(
  type = "cox",
  data = simdata,
  follow_up_time = "follow_up_time",
  event = "event",
  comp_labels = comp_labels,
  rounded_zeroes = FALSE
)

m4 <- comp_model(
  type = "cox",
  data = epicoda::simdata,
  follow_up_time = "follow_up_time",
  event = "event",
  comp_labels = comp_labels,
  covariates = c("agegroup"),
  rounded_zeroes = FALSE
)

t_yes <- predict_fit_and_ci(m3, md, comp_labels = comp_labels)
t_no <- predict_fit_and_ci(m3, md, comp_labels = comp_labels , terms = FALSE)

t_yes_cov <- predict_fit_and_ci(m4, md, comp_labels = comp_labels)
t_no_cov <- predict_fit_and_ci(m4, md, comp_labels = comp_labels , terms = FALSE)

test_that("when no covariates, two CI types for Cox are same ", {
  expect_equal(t_yes$fit, t_no$fit)
})

test_that("when no covariates, two CI types for Cox are same ", {
  expect_equal(as.vector(t_yes$lower_CI), as.vector(t_no$lower_CI))
})

test_that("when no covariates, two CI types for Cox are same ", {
  expect_equal(as.vector(t_yes$upper_CI), as.vector(t_no$upper_CI))
})

test_that("when covariates, two CI types for Cox are same", {
  expect_equal(as.vector((log(t_yes$upper_CI) - log(t_yes$fit))/1.96), as.vector(t_no$se.fit), tolerance = 0.00001 )
  })

test_that("when covariates, two CI types for Cox are different", {
  expect_false(isTRUE(all.equal(as.vector(t_yes_cov$upper_CI), as.vector(t_no_cov$upper_CI))))
})

test_that("when covariates, two CI types for Cox are different", {
  expect_false(isTRUE(all.equal(as.vector((log(t_yes_cov$upper_CI) - log(t_yes_cov$fit))/1.96), as.vector(t_no_cov$se.fit),  tolerance = 0.00001 )))
})

#==============
# Test known invariance properties
m5 <- comp_model(
  type = "cox",
  data = epicoda::simdata,
  follow_up_time = "follow_up_time",
  event = "event",
  comp_labels = comp_labels,
  covariates = c("agegroup"),
  part_1 = "sedentary",
  rounded_zeroes = FALSE
)

m6 <- comp_model(
  type = "cox",
  data = epicoda::simdata,
  follow_up_time = "follow_up_time",
  event = "event",
  comp_labels = comp_labels,
  covariates = c("agegroup"),
  part_1 = "sleep",
  rounded_zeroes = FALSE
)

t_p1_sed <- predict_fit_and_ci(m5, md, comp_labels = comp_labels, part_1 = "sedentary")
t_p1_sleep <- predict_fit_and_ci(m6, md, comp_labels = comp_labels, part_1 = "sleep")

test_that("order of ilr parts doesn't matter for global compositional predictions", {
  expect_equal(t_p1_sed$fit,t_p1_sleep$fit)
})

test_that("order of ilr parts doesn't matter - CI ", {
  expect_equal(as.vector(t_p1_sed$lower_CI), as.vector(t_p1_sleep$lower_CI))
})

test_that("order of ilr parts doesn't matter - CI ", {
  expect_equal(as.vector(t_p1_sed$upper_CI), as.vector(t_p1_sleep$upper_CI))
})
