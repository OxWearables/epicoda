devtools::load_all()
simdataplain <- epicoda::simdataplain
simdata <- epicoda::simdata
comp_labels <- c("partA", "partB", "partC", "partD", "partE")

simdata$sex <- as.factor(simdataplain$sex)
simdata$agegroup <- as.factor(simdataplain$agegroup)
data_ilr_impute_zeroes <- epicoda::transform_comp(data = simdataplain,
                                         comp_labels = comp_labels,
                                         transformation_type = "ilr",
                                         rounded_zeroes = TRUE,
                                         det_limit = 0.0083)
data_ilr_impute_zeroes
transf_vec <- transf_labels(comp_labels = comp_labels, transformation_type = "ilr")
transf_sum <- vector_to_sum(transf_vec)
lm_outcome <- lm(as.formula(paste("linear_outcome ~ agegroup + sex + ", transf_sum)),
                 data_ilr_impute_zeroes)

log_outcome <- glm(as.formula(paste("binary_outcome ~ agegroup + sex + ", transf_sum)),
                   data_ilr_impute_zeroes, family = "binomial")

summary(log_outcome)
data_ilr_impute_zeroes
summary(lm_outcome)
tab_covariate_coefs(lm_outcome, comp_labels = comp_labels)
tab_coefs( scale_type = "lp", level = 0.95, type = "linear",
                outcome = "linear_outcome",
                covariates = c("agegroup", "sex"),
                follow_up_time = "follow_up_time",
                event = "event",
                data = simdataplain,
                comp_labels = comp_labels,
                rounded_zeroes = TRUE,
                det_limit = 0.0083)

tab_coefs( scale_type = "exp", level = 0.95, type = "cox",
           outcome = NULL,
           covariates = c("agegroup", "sex"),
           follow_up_time = "follow_up_time",
           event = "event",
           data = simdataplain,
           comp_labels = comp_labels,
           rounded_zeroes = TRUE,
           det_limit = 0.0083)

summary(log_outcome)
epicoda::plot_transfers(from_part = "partB",
               to_part = "partD",
               model = lm_outcome ,
               dataset = data_ilr_impute_zeroes,
               transformation_type = "ilr",
               comp_labels = comp_labels,
               y_label = NULL,
               units = "hr/day",
               rounded_zeroes = TRUE,
               det_limit = 0.0083,
               terms = TRUE,  plot_log = FALSE)

head(data_ilr_impute_zeroes)
epicoda::plot_transfers

cox_outcome <- comp_model(
  type = "cox",
  outcome = NULL,
  covariates = c("agegroup", "sex"),
  follow_up_time = "follow_up_time",
  event = "event",
  data = simdataplain,
  comp_labels = comp_labels,
  transformation_type = "ilr",
  rounded_zeroes = TRUE,
  det_limit = 0.0083,
  comparison_part = NULL,
  part_1 = NULL
)
summary(cox_outcome)
plot_transfers(from_part = "partD",
              to_part = "partB",
              model = cox_outcome ,
              dataset = simdataplain,
              transformation_type = "ilr",
              comp_labels = comp_labels,
              y_label = NULL,
              units = "hr/day",
              rounded_zeroes = TRUE,
              det_limit = 0.0083,
              terms = TRUE,  plot_log = FALSE)

head(simdata)
composition_list <- list("trial" = data.frame("partA" = c(0.001), "partB" = 1.499, "partC" = 4.5, "partD" = 9, "partE" = 9))

forest_plot_comp(composition_list = composition_list,
         model = lm_outcome,
         dataset = data_ilr_impute_zeroes,
         fixed_values = NULL,
         transformation_type = "ilr",
         comparison_part = NULL,
         part_1 = NULL,
         comp_labels = comp_labels,
         units = "hr/day",
         specified_units = NULL,
         rounded_zeroes = FALSE,
         det_limit = NULL,
         terms = TRUE,
         x_label = NULL,
         xllimit = NULL,
         xulimit = NULL,
         plot_log = FALSE)



has_this_worked <- comp_model(type = "linear",
         outcome = "linear_outcome",
         covariates = c("agegroup", "sex"),
         follow_up_time = NULL,
         event = NULL,
         data = simdataplain,
         comp_labels = comp_labels,
         transformation_type = "ilr",
         rounded_zeroes = TRUE,
         det_limit = 0.0083,
         comparison_part = NULL,
         part_1 = NULL)
