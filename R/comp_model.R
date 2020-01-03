#' Statistical models with compositional exposure variables
#'
#' This is a wrapper for \code{lm}, \code{glm} and \code{survival::coxph} which performs the compositional transformation before generating the model.
#'
#' @param type Type of statistical model to use. It should be one of "linear", "logistic", "cox".
#' @param outcome Column name of outcome variable in data. If \code{type} is \code{"linear"}, this should be a continuous variable.  If \code{type} is \code{"logistic"}, this should be a binary outcome.  If \code{type} is \code{"cox"}, this should be left as \code{NULL} and instead \code{follow_up_time} and \code{event} should be set.
#' @param covariates Character vector of column names of covariates to adjust models for. As this is used as a character vector, special arguments to the standard models (like strata(variable) for a Cox model) can be used here.
#' @param follow_up_time Only needed if \code{type} is \code{"cox"}. Follow-up time.
#' @param event Only needed if \code{type} is \code{"cox"}. Binary variable indicating whether or not an event was observed.
#' @inheritParams transform_comp
#'
#' @return model
#' @examples
#' @export
comp_model <-
  function(type = NULL,
           outcome = NULL,
           covariates = NULL,
           follow_up_time = NULL,
           event = NULL,
           data,
           comp_labels,
           transformation_type = "ilr",
           rounded_zeroes = TRUE,
           det_limit = NULL,
           comparison_part = NULL,
           part_1 = NULL) {
    if ((type != "linear") && (type != "logistic") && (type != "cox")) {
      stop("type must be \"linear\", \"logistic\", or \"cox\".")
    }

    data_ready <- transform_comp(
      data = data,
      comp_labels = comp_labels,
      transformation_type = transformation_type,
      rounded_zeroes = rounded_zeroes,
      det_limit = det_limit,
      comparison_part = comparison_part,
      part_1
    )
    transf_vec <-
      transf_labels(comp_labels = comp_labels, transformation_type = transformation_type)
    transf_sum <- vector_to_sum(transf_vec)
    cov_sum <- vector_to_sum(covariates)

    if (type == "linear") {
      model <-
        lm(as.formula(paste(outcome, "~", cov_sum, "+", transf_sum)),
           data = data_ready)
    }

    if (type == "logistic") {
      model <-
        glm(as.formula(paste(outcome, "~", cov_sum, "+", transf_sum)),
            data = data_ready, family = "binomial")
    }
    if (type == "cox") {
      survival_object <- Surv(data[, follow_up_time], data[, event])
      model <-
        coxph(as.formula(paste(
          "survival_object ~", cov_sum, "+", transf_sum
        )))
    }

    return(model)

  }
