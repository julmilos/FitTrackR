# Internal helper: compute final fuzzy weights
.compute_final_weights <- function(
    decision_matrix,
    weights = NULL,
    bwm_criteria = NULL,
    bwm_best = NULL,
    bwm_worst = NULL
) {

  n_criteria <- ncol(decision_matrix) / 3

  if (!is.null(weights)) {

    if (length(weights) != ncol(decision_matrix)) {
      stop("Length of 'weights' must equal number of fuzzy columns (3 × criteria).")
    }

    return(weights)
  }

  if (!is.null(bwm_criteria) &&
      !is.null(bwm_best) &&
      !is.null(bwm_worst)) {

    bwm_result <- calculate_bwm_weights(
      bwm_criteria,
      bwm_best,
      bwm_worst
    )

    crisp_weights <- bwm_result$criteriaWeights

    if (length(crisp_weights) != n_criteria) {
      stop("BWM weights do not match number of criteria.")
    }

    return(rep(crisp_weights, each = 3))
  }

  stop("Provide either explicit fuzzy weights or BWM parameters.")
}
