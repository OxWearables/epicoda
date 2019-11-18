devtools::load_all()
simdataplain <- epicoda::simdataplain
comp_labels <- c("compA", "compB", "compC", "compD", "compE")
simdataplain$sex <- as.factor(simdataplain$sex)
simdataplain$agegroup <- as.factor(simdataplain$agegroup)
data_ilr_impute_zeroes <- transform_comp(data = simdataplain,
                                         comp_labels = comp_labels,
                                         transformation_type = "ilr",
                                         rounded_zeroes = TRUE,
                                         det_limit = 0.0083)

transf_vec <- transf_labels(comp_labels = comp_labels, transformation_type = "ilr")
transf_sum <- vector_to_sum(transf_vec)
lm_outcome <- lm(as.formula(paste("outcome ~ agegroup + sex + ", transf_sum)),
                 data_ilr_impute_zeroes)
plot_transfers(from_component = "compD",
               to_component = "compA",
               model = lm_outcome ,
               dataset = data_ilr_impute_zeroes,
               transformation_type = "ilr",
               comp_labels = comp_labels,
               y_label = NULL,
               units = "hr/day",
               rounded_zeroes = TRUE,
               det_limit = 0.0083,
               terms = TRUE)

