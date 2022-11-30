min_val_in_data <- min(simdata$vigorous[simdata$vigorous > 0])
lm_outcome <- comp_model(type = "linear",
outcome = "BMI",
covariates = c("agegroup", "sex"),
data = simdata,
comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), det_limit = min_val_in_data)

test_that("error if try to use parts not appearing in comp_labels", {
  expect_error(plot_transfers(from_part = "sedentary",
                                       to_part = "BMI",
                                       model = lm_outcome ,
                                       comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                       y_label = "Model-predicted difference in BMI",
                                       units = "hr/day",
                                       terms = TRUE))
})

test_that("error if try to use wrong comp_labels", {
  expect_error(plot_transfers(from_part = "sedentary",
                                       to_part = "moderate",
                                       model = lm_outcome ,
                                       comp_labels =c("vigorous", "light", "sedentary", "sleep"),
                                       y_label = "Model-predicted difference in BMI",
                                       units = "hr/day",
                                       terms = TRUE))
})



test_that("result unchanged ", {
  expect_equal_to_reference(plot_transfers(from_part = "sedentary",
                                       to_part = "moderate",
                                       model = lm_outcome ,
                                       comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                       y_label = "Model-predicted difference in BMI",
                                       units = "hr/day",
                                       terms = TRUE), "../test_data/plot_example.RDS")
})

test_that("error if compositional parts in different order ", {
  expect_error(plot_transfers(from_part = "sedentary",
                                                    to_part = "moderate",
                                                    model = lm_outcome ,
                                                    comp_labels =c("vigorous", "moderate",  "sedentary","light", "sleep"),
                                                    y_label = "Model-predicted difference in BMI",
                                                    units = "hr/day",
                                                    terms = TRUE))
})



# CHECK PLOTTING CODE RUNS WITHOUT ERRORS ON A VARIETY OF DIFFERENT SETTINGS -------------------------------------------------------------------
# Note that this is not a check of plot correctness
log_outcome <- comp_model(
  type = "logistic",
  outcome = "disease",
  covariates = c("agegroup", "sex"),
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

model_list <- list(
  "linear" = lm_outcome,
  "logistic" = log_outcome,
  "cox" = cox_outcome
  )

for ( name in names(model_list) ){
  test_that("Plot runs.", {
    expect_error(plot_transfers(from_part = "sedentary",
                               to_part = "moderate",
                               model = model_list[[name]] ,
                               comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                               units = "hr/day",
                               granularity = 20), NA)
  })


  test_that("Plots match when arguments specified or not.", {
    expect_equal(plot_transfers(from_part = "sedentary",
                                to_part = "sleep",
                                model = model_list[[name]] ,
                                comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                units = "hr/day",
                                granularity = 20), plot_transfers(from_part = "sedentary",
                                                                  to_part = "sleep",
                                                                  model = model_list[[name]] ,
                                                                  comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                                                  units = "hr/day", terms = TRUE,
                                                                  granularity = 20))
  })


  if (name != "linear"){
    test_that("Plot runs with log scale plotting.", {
      expect_error(plot_transfers(from_part = "sedentary",
                                 to_part = "moderate",
                                 model = model_list[[name]] ,
                                 comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                 terms = TRUE, plot_log = TRUE,
                                 granularity = 20), NA)
    })

    test_that("Not equal with and without log scale plotting.", {
      expect_false(isTRUE(all.equal(plot_transfers(from_part = "sedentary",
                                                                to_part = "moderate",
                                                                model = model_list[[name]] ,
                                                                comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                                                terms = TRUE, plot_log = TRUE,
                                                                granularity = 20), plot_transfers(from_part = "sedentary",
                                                                                                               to_part = "moderate",
                                                                                                               model = model_list[[name]] ,
                                                                                                               comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                                                                                               terms = TRUE,
                                                                                                               granularity = 20))))
    })

    test_that("Message for Cox models and logistic models with terms.", {
      expect_message(plot_transfers(from_part = "light",
                                  to_part = "moderate",
                                  model = model_list[[name]] ,
                                  comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                  granularity = 20))
    })


  }




  test_that("Plot runs with terms = FALSE", {
    expect_error(plot_transfers(from_part = "sedentary",
                               to_part = "moderate",
                               model = model_list[[name]] ,
                               comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                               units = "min/wk",
                               terms = FALSE), NA)
    })


  test_that("Plot runs with plot_log and terms = FALSE", {
      expect_error(plot_transfers(from_part = "sedentary",
                               to_part = "light",
                               model = model_list[[name]] ,
                               comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                               units = "min/day",
                               plot_log = TRUE,
                               terms = FALSE), NA)
  })

  test_that("Not equal with and without log scale plotting.", {
    expect_false(isTRUE(all.equal(plot_transfers(from_part = "sedentary",
                                                 to_part = "moderate",
                                                 model = model_list[[name]] ,
                                                 comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                                 terms = FALSE, plot_log = TRUE,
                                                 granularity = 20), plot_transfers(from_part = "sedentary",
                                                                                   to_part = "moderate",
                                                                                   model = model_list[[name]] ,
                                                                                   comp_labels =c("vigorous", "moderate", "light", "sedentary", "sleep"),
                                                                                   terms = FALSE,
                                                                                   granularity = 20))))
  })


}

