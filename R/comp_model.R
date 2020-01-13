#' Statistical models with compositional exposure variables
#'
#' This is a wrapper for \code{lm}, \code{glm} and \code{survival::coxph} which performs the compositional transformation before generating the model.
#'
#' @param type Type of statistical model to use. It should be one of "linear", "logistic", "cox".
#' @param outcome Column name of outcome variable in data. If \code{type} is \code{"linear"}, this should be a continuous variable.  If \code{type} is \code{"logistic"}, this should be a binary outcome.  If \code{type} is \code{"cox"}, if this is set it should be a \code{Surv} object from package \code{survival} (if it is set, the function defaults to attempt to use it, even if \code{follow_up_time} and \code{event} are set). If this is left as \code{NULL}, \code{follow_up_time} and \code{event} can be set instead.
#' @param covariates Character vector of column names of covariates to adjust models for. As this is used as a character vector, special arguments to the standard models (like strata(variable) for a Cox model) can be used here.
#' @param follow_up_time Only used if \code{type} is \code{"cox"} and \code{outcome} is \code{NULL}.  Follow-up time.
#' @param event Only used if \code{type} is \code{"cox"} and \code{outcome} is \code{NULL}.Binary variable indicating whether or not an event was observed.
#' @inheritParams transform_comp
#'
#' @return model
#' @examples
#'  comp_model(type = "linear",
#' outcome = "linear_outcome",
#' covariates = c("agegroup", "sex"),
#' follow_up_time = NULL,
#' event = NULL,
#' data = simdata,
#' comp_labels = c("partA", "partB", "partC", "partD", "partE"),
#' rounded_zeroes = TRUE,
#' det_limit = 0.0083)
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
    if (transformation_type == "clr"){
      stop("clr variables are singular and shouldn't be used for regression modelling.")
    }
    data_ready <- transform_comp(
      data = data,
      comp_labels = comp_labels,
      transformation_type = transformation_type,
      rounded_zeroes = rounded_zeroes,
      det_limit = det_limit,
      comparison_part = comparison_part,
      part_1 = part_1
    )
    transf_vec <-
      transf_labels(comp_labels = comp_labels, transformation_type = transformation_type, comparison_part = comparison_part,
                    part_1 = part_1)
    transf_sum <- vector_to_sum(transf_vec)
    cov_sum <- vector_to_sum(covariates)

    if (type == "linear") {
      if (is.null(outcome)){
        stop("outcome must be set.")
      }
      if (is.null(covariates)){
        model <-
          stats::lm(stats::as.formula(paste(outcome, "~", transf_sum)),
                    data = data_ready)
      }
      else{
         model <-
        stats::lm(stats::as.formula(paste(outcome, "~", cov_sum, "+", transf_sum)),
           data = data_ready)
      }

    }

    if (type == "logistic") {
      if (is.null(outcome)){
        stop("outcome must be set.")
      }
      if (is.null(covariates)){
              model <-
        stats::glm(stats::as.formula(paste(outcome, "~", cov_sum, "+", transf_sum)),
            data = data_ready, family = "binomial")
      }
      else {
        model <-
          stats::glm(stats::as.formula(paste(outcome, "~", cov_sum, "+", transf_sum)),
                     data = data_ready, family = "binomial")
      }

    }
    if ((type == "cox")) {
       if (is.null(outcome)){
        if (is.null(follow_up_time) | is.null(event)){
          stop("follow_up_time and event, or outcome, must be set.")
        }
         survival_object <- survival::Surv(data_ready[, follow_up_time], data_ready[, event])
        }
       if (!(is.null(outcome))){
         survival_object <- outcome
       }

      if (is.null(covariates)){
        model <-
          survival::coxph(stats::as.formula(paste(
            "survival_object ~",  transf_sum
          )), data = data_ready)
      }
      if (!is.null(covariates)){
           model <-
        survival::coxph(stats::as.formula(paste(
          "survival_object ~", cov_sum, "+", transf_sum
        )), data = data_ready)
      }

    }


    return(model)

  }
