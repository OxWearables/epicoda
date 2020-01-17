devtools::load_all()
simdata <- epicoda::simdata
comp_labels <- c("partA", "partB", "partC", "partD", "partE")

head(normalise_comp(simdata, comp_labels))



simdata$sex <- as.factor(simdata$sex)
simdata$agegroup <- as.factor(simdata$agegroup)

plot_density_ternary(data = simdata, parts_to_plot = c("partB", "partC", "partD"), groups = "agegroup")
data_ilr_impute_zeroes <- epicoda::transform_comp(data = simdata,
                                         comp_labels = comp_labels,
                                         transformation_type = "ilr",
                                         rounded_zeroes = TRUE, det_limit = 0.0011)
data_ilr_impute_zeroes
transf_vec <- transf_labels(comp_labels = comp_labels, transformation_type = "ilr")
transf_sum <- vector_to_sum(transf_vec)
lm_outcome <- lm(as.formula(paste("linear_outcome ~ agegroup + sex + ", transf_sum)),
                 data_ilr_impute_zeroes)

log_outcome <- glm(as.formula(paste("binary_outcome ~ agegroup + sex + ", transf_sum)),
                   data_ilr_impute_zeroes, family = "binomial")

summary(log_outcome)
summary(lm_outcome)
tab_covariate_coefs(lm_outcome, comp_labels = comp_labels)
tab_coefs(scale_type = "lp", level = 0.95, type = "linear",
                outcome = "linear_outcome",
                covariates = c("agegroup", "sex"),
                follow_up_time = "follow_up_time",
                event = "event",
                data = simdata,
                comp_labels = comp_labels,
                rounded_zeroes = TRUE)

tab_coefs( scale_type = "exp", level = 0.95, type = "cox",
           outcome = NULL,
           covariates = c("agegroup", "sex"),
           follow_up_time = "follow_up_time",
           event = "event",
           data = simdata,
           comp_labels = comp_labels,
           rounded_zeroes = TRUE)

summary(log_outcome)
epicoda::plot_transfers(from_part = "partA",
               to_part = "partD",
               model = lm_outcome ,
               dataset = data_ilr_impute_zeroes,
               transformation_type = "ilr",
               comp_labels = comp_labels,
               y_label = NULL,
               units = "hr/day",
               rounded_zeroes = TRUE,
               terms = TRUE,  plot_log = FALSE, granularity = 100)

head(data_ilr_impute_zeroes)
epicoda::plot_transfers

cox_outcome <- comp_model(
  type = "cox",
  outcome = NULL,
  covariates = c("agegroup", "sex"),
  follow_up_time = "follow_up_time",
  event = "event",
  data = simdata,
  comp_labels = comp_labels,
  transformation_type = "ilr",
  rounded_zeroes = TRUE,
  comparison_part = NULL,
  part_1 = NULL
)
cox_outcome_no_sex <- comp_model(
  type = "cox",
  outcome = NULL,
  covariates = c("agegroup"),
  follow_up_time = "follow_up_time",
  event = "event",
  data = simdata,
  comp_labels = comp_labels,
  transformation_type = "ilr",
  rounded_zeroes = TRUE,
  comparison_part = NULL,
  part_1 = NULL
)
summary(cox_outcome)
p <- plot_transfers(from_part = "partD",
              to_part = "partA",
              model = cox_outcome ,
              dataset = simdata,
              transformation_type = "ilr",
              comp_labels = comp_labels,
              y_label = NULL,
              units = "hr/day",
              rounded_zeroes = TRUE,
              terms = TRUE,  plot_log = FALSE)
p
save_plot(p, filename = "testing1.tiff")

head(simdata)
composition_list <- list("cm" = as.data.frame(
  comp_mean(
    data = simdata,
    comp_labels = comp_labels,
    rounded_zeroes = TRUE,
    units = "hr/day" # Note whatever the units are here is the units you should specify the changes in in the next two lines.
  )),
  "trial2" = data.frame("partA" = c(0.001), "partB" = 1.49, "partC" = 3.5, "partD" = 9, "partE" = 10))
devtools::load_all()
forest_plot_comp(composition_list = composition_list,
         model = cox_outcome,
         dataset = simdata,
         fixed_values = NULL,
         transformation_type = "ilr",
         comparison_part = NULL,
         part_1 = NULL,
         comp_labels = comp_labels,
         units = "hr/day",
         specified_units = NULL,
         rounded_zeroes = TRUE,
         terms = TRUE,
         x_label = NULL,
         xllimit = NULL,
         xulimit = NULL,
         plot_log = FALSE
         )



has_this_worked <- comp_model(type = "linear",
         outcome = "linear_outcome",
         covariates = c("agegroup", "sex"),
         follow_up_time = NULL,
         event = NULL,
         data = simdata,
         comp_labels = comp_labels,
         transformation_type = "ilr",
         rounded_zeroes = TRUE,
         comparison_part = NULL,
         part_1 = NULL)

