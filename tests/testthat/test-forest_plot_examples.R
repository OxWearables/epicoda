# Example using a list of models
# First we set up composition list
df <-
  as.data.frame(comp_mean(
    data = simdata,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
    det_limit = 0.00119,
    units = "hr/day"
  ))
new_comp <-
  change_composition(
    composition = df,
    main_part = "moderate",
    main_change = +0.5,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
  )
new_comp2 <-
  change_composition(
    composition = df,
    main_part = "sedentary",
    main_change = -3.5,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
  )
list_for_plot <-
  list("Extra 0.5 hr/day moderate" = new_comp,
       "3.5 hr/day less sedentary" = new_comp2)

# Then calculate models
lm_BMI_unadjusted <-
  comp_model(
    type = "linear",
    outcome = "BMI",
    data = simdata,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
    det_limit = 0.00119
  )
lm_BMI_age_group_only <-
  comp_model(
    type = "linear",
    outcome = "BMI",
    covariates = c("agegroup"),
    data = simdata,
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
    det_limit = 0.00119
  )

log_outcome <- comp_model(
  type = "logistic",
  outcome = "disease",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
  det_limit = 0.00119
)



test_that("Error when forest plotting plots of different types", {
  expect_error(
    forest_plot_comp(
      composition_list = list_for_plot,
      models_list = list(
        "Unadjusted" = lm_BMI_unadjusted,
        "Age-adjusted" = lm_BMI_age_group_only,
        "Log" = log_outcome
      ),
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
      terms = FALSE,
      xllimit = -1
    )
  )
})




# # CHECK PLOTTING CODE RUNS WITHOUT ERRORS ON A VARIETY OF DIFFERENT SETTINGS -------------------------------------------------------------------
# Note that this is not a check of plot correctness
lm_outcome <- comp_model(
  type = "linear",
  outcome = "BMI",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
  rounded_zeroes = FALSE
)

logistic_outcome <- comp_model(
  type = "logistic",
  outcome = "disease",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
  rounded_zeroes = FALSE
)

logistic_outcome_unadjusted <- comp_model(
  type = "logistic",
  outcome = "disease",
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
  rounded_zeroes = FALSE
)

cox_outcome <- comp_model(
  type = "cox",
  event = "event",
  follow_up_time = "follow_up_time",
  covariates = c("agegroup", "sex"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
  rounded_zeroes = FALSE
)

cox_outcome_age <- comp_model(
  type = "cox",
  event = "event",
  follow_up_time = "follow_up_time",
  covariates = c("agegroup"),
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
  rounded_zeroes = FALSE
)


cox_outcome_unadjusted <- comp_model(
  type = "cox",
  event = "event",
  follow_up_time = "follow_up_time",
  data = simdata,
  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
  rounded_zeroes = FALSE
)



model_list <- list(
  "linear" = lm_outcome,
  "logistic" = logistic_outcome,
  "cox" = cox_outcome
)

for (name in names(model_list) ){
  test_that("Plot runs.", {
    expect_error(forest_plot_comp(
      composition_list = list_for_plot,
      model = model_list[[name]],
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")), NA)
  })

  p1 <- forest_plot_comp(
    composition_list = list_for_plot,
    model = model_list[[name]],
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
  p2 <- forest_plot_comp(
    composition_list = list_for_plot,
    model = model_list[[name]],
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), terms = TRUE)
  # these can contain grid related parameters which chnage plot to plot so cause failures
  p2$labels <- p1$labels
  p2$axisList <- p1$axisList

  test_that("Plots match when arguments specified or not.", {
    expect_equal(p1, p2)})



  if (name != "linear"){
    test_that("Plot runs with log scale plotting.", {
      expect_error(forest_plot_comp(
        composition_list = list_for_plot,
        model = model_list[[name]],
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
        plot_log = TRUE), NA)})

      p1 <- forest_plot_comp(
        composition_list = list_for_plot,
        model = model_list[[name]],
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
      p2 <- forest_plot_comp(
        composition_list = list_for_plot,
        model = model_list[[name]],
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), plot_log = TRUE)

      test_that("Plot not equal with and without log scale plotting",
                {expect_false(isTRUE(all.equal(p1$estimates, p2$estimates)))}) # can't just check for equality of overall objects because behave weirdly in all sorts of ways which could lead to false failures


    test_that("Message for Cox models and logistic models with terms.", {
      expect_message(forest_plot_comp(
        composition_list = list_for_plot,
        model = model_list[[name]],
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")))
    })


  }




  test_that("Plot runs with terms = FALSE", {
    expect_error(forest_plot_comp(
      composition_list = list_for_plot,
      model = model_list[[name]],
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                         units = "min/wk",
                                         terms = FALSE), NA)
  })


  test_that("Plot runs with plot_log and terms = FALSE", {
    if (name == "logistic"){ # this is a hack to stop it trying to plot impossible values in quite a stupid case (plotting probabilities with log scale, which makes little sense). As per issue #83 there are some very weird behaviours of axis scales at the moment.
    expect_error(forest_plot_comp(
      composition_list = list_for_plot,
      model = model_list[[name]],
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                         plot_log = TRUE,
                                         terms = FALSE, xllimit = 0.1), NA)
    }
    else {expect_error(forest_plot_comp(
      composition_list = list_for_plot,
      model = model_list[[name]],
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
      plot_log = TRUE,
      terms = FALSE), NA)}
  })


}



# Error if models in model list from different datasets =======
test_that("Error if models in list from different datasets", {
  expect_error(forest_plot_comp(
    composition_list = list_for_plot,
    models_list = list(lm_outcome, lm_BMI_unadjusted),
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")))
})

# CHECK THAT RUNS WITH LISTS OF MODELS =========================
# Again this is just basically checking stuff runs

model_list_list <- list(
  "linear" = list("Unadjusted" = lm_BMI_unadjusted,
                  "Age-adjusted" = lm_BMI_age_group_only),
  "logistic" = list("Unadj" = logistic_outcome_unadjusted, "Adjusted" = logistic_outcome),
  "cox" = list("Unad" = cox_outcome_unadjusted, "adjusted1" = cox_outcome_age, "adjusted" = cox_outcome)
)

for ( name in names(model_list_list) ){
  test_that("Plot runs.", {
    expect_error(forest_plot_comp(
      composition_list = list_for_plot,
      models_list = model_list_list[[name]],
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")), NA)
  })

  p1 <- forest_plot_comp(
    composition_list = list_for_plot,
    models_list = model_list_list[[name]],
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
  p2 <- forest_plot_comp(
    composition_list = list_for_plot,
    models_list = model_list_list[[name]],
    comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), terms = TRUE)
  # these can contain grid related parameters which chnage plot to plot so cause failures
  p2$labels <- p1$labels
  p2$axisList <- p1$axisList

  test_that("Plots match when arguments specified or not.", {
    expect_equal(p1, p2)})


  if (name != "linear"){
    test_that("Plot runs with log scale plotting.", {
      expect_error(forest_plot_comp(
        composition_list = list_for_plot,
        models_list = model_list_list[[name]],
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
        plot_log = TRUE), NA)})

      p1 <- forest_plot_comp(
        composition_list = list_for_plot,
        models_list = model_list_list[[name]],
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
      p2 <- forest_plot_comp(
        composition_list = list_for_plot,
        models_list = model_list_list[[name]],
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), plot_log = TRUE)

     test_that("Plot not equal with and without log scale plotting",
                {expect_false(isTRUE(all.equal(p1$estimates, p2$estimates)))}) # can't just check for equality of overall objects because behave weirdly in all sorts of ways which could lead to false failures

    test_that("Message for Cox models and logistic models with terms.", {
      expect_message(forest_plot_comp(
        composition_list = list_for_plot,
        models_list = model_list_list[[name]],
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")))
    })


  }




  # WARNING: In general, plotting a comparison of models with terms = FALSE is a very weird thing to do, as if you fix the covariate values
  # at different levels the interpretation of the different estimates is just very odd.
  test_that("Plot runs with terms = FALSE", {
    expect_error(forest_plot_comp(
      composition_list = list_for_plot,
      models_list = model_list_list[[name]],
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
      units = "min/wk",
      terms = FALSE), NA)
  })


  test_that("Plot runs with plot_log and terms = FALSE", {
    xllimit <- NULL
    if (name == "logistic"){ # this is a hack to stop it trying to plot impossible values in quite a stupid case (plotting probabilities with log scale, which makes little sense)
    expect_error(forest_plot_comp(
      composition_list = list_for_plot,
      models_list = model_list_list[[name]],
      comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
      plot_log = TRUE,
      terms = FALSE , xllimit = 0.1), NA)
    }
    else {
      expect_error(forest_plot_comp(
        composition_list = list_for_plot,
        models_list = model_list_list[[name]],
        comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
        plot_log = TRUE,
        terms = FALSE, xllimit = 1), NA)
    }
  })


}

