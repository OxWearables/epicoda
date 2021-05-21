# Example using a list of models
# First we set up composition list
df <- as.data.frame(comp_mean(data = simdata, comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), units = "hr/day"))
new_comp <- change_composition(composition = df, main_part = "moderate", main_change = +0.5, comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
new_comp2 <- change_composition(composition = df, main_part = "sedentary", main_change = -3.5, comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
list_for_plot <- list("Extra 0.5 hr/day moderate" = new_comp, "3.5 hr/day less sedentary" = new_comp2)

# Then calculate models
lm_BMI_unadjusted <- comp_model(type = "linear", outcome = "BMI", data = simdata, comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
lm_BMI_age_group_only <- comp_model(type = "linear", outcome = "BMI", covariates = c("agegroup"), data = simdata, comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))

log_outcome <- comp_model(type = "logistic",
                          outcome = "disease",
                          covariates = c("agegroup", "sex"),
                          data = simdata,
                          comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))



test_that("Error when forest plotting plots of different types", {
  expect_error(  forest_plot_comp(composition_list = list_for_plot,
                     models_list = list("Unadjusted" = lm_BMI_unadjusted, "Age-adjusted" = lm_BMI_age_group_only, "Log" = log_outcome),
                     dataset = simdata,
                     comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"), terms = FALSE, xllimit = -1)
  )
})
