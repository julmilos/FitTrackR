# Internal helper: compute final fuzzy weights
.get_final_weights <- function(decision_matrix,
                               weights,
                               bwm_criteria,
                               bwm_best,
                               bwm_worst) {

  n_crit <- ncol(decision_matrix) / 3

  if (!missing(weights)) {
    if (length(weights) != ncol(decision_matrix)) {
      stop("Length of 'weights' must equal number of fuzzy columns (3 × criteria).")
    }
    return(weights)
  }

  if (!missing(bwm_criteria) && !missing(bwm_best) && !missing(bwm_worst)) {
    bwm_res <- calculate_bwm_weights(
      bwm_criteria,
      bwm_best,
      bwm_worst
    )

    crisp_weights <- bwm_res$criteriaWeights

    if (length(crisp_weights) != n_crit) {
      stop("BWM weights do not match number of criteria.")
    }

    return(rep(crisp_weights, each = 3))
  }

  stop("Provide either explicit fuzzy weights or BWM parameters.")
}
