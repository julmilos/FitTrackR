#' Fuzzy VIKOR Decision Method
#'
#' Implements a fuzzy multi-criteria decision-making method
#' based on the VIKOR compromise ranking approach.
#'
#' @param decision_matrix Matrix (m × 3n) of triangular fuzzy numbers.
#' @param criteria_direction Character vector ("max" / "min") for each criterion.
#' @param strategy_weight Numeric [0,1]. Importance of group utility.
#' @param weights Optional fuzzy weights (length 3n).
#' @param bwm_criteria Optional criteria names for BWM.
#' @param bwm_best Optional BWM best-to-others vector.
#' @param bwm_worst Optional BWM others-to-worst vector.
#' @param epsilon Small numeric constant for numerical stability.
#'
#' @return Object of class `fuzzy_vikor_res`.
#' @export
fuzzy_vikor <- function(
    decision_matrix,
    criteria_direction,
    strategy_weight = 0.5,
    weights,
    bwm_criteria,
    bwm_best,
    bwm_worst,
    epsilon = 1e-9
) {

  if (!is.matrix(decision_matrix)) {
    stop("'decision_matrix' must be a matrix.")
  }

  n_crit <- ncol(decision_matrix) / 3
  if (length(criteria_direction) != n_crit) {
    stop("Length of 'criteria_direction' must equal number of criteria.")
  }

  final_weights <- .get_final_weights(
    decision_matrix,
    weights,
    bwm_criteria,
    bwm_best,
    bwm_worst
  )

  fuzzy_dir <- rep(criteria_direction, each = 3)
  n_cols <- ncol(decision_matrix)

  positive_ideal <- ifelse(
    fuzzy_dir == "max",
    apply(decision_matrix, 2, max),
    apply(decision_matrix, 2, min)
  )

  negative_ideal <- ifelse(
    fuzzy_dir == "min",
    apply(decision_matrix, 2, max),
    apply(decision_matrix, 2, min)
  )

  distance_matrix <- matrix(0, nrow(decision_matrix), n_cols)

  for (i in seq(1, n_cols, 3)) {
    denom <- positive_ideal[i + 2] - negative_ideal[i]
    if (denom == 0) denom <- epsilon

    if (fuzzy_dir[i] == "max") {
      distance_matrix[, i]   <- (positive_ideal[i]   - decision_matrix[, i + 2]) / denom
      distance_matrix[, i+1] <- (positive_ideal[i+1] - decision_matrix[, i + 1]) / denom
      distance_matrix[, i+2] <- (positive_ideal[i+2] - decision_matrix[, i])     / denom
    } else {
      distance_matrix[, i]   <- (decision_matrix[, i]     - positive_ideal[i + 2]) / denom
      distance_matrix[, i+1] <- (decision_matrix[, i + 1] - positive_ideal[i + 1]) / denom
      distance_matrix[, i+2] <- (decision_matrix[, i + 2] - positive_ideal[i])     / denom
    }
  }

  weighted_distances <- distance_matrix %*% diag(final_weights)

  aggregated_loss <- rowSums(weighted_distances[, seq(1, n_cols, 3), drop = FALSE])
  worst_case_loss <- apply(weighted_distances[, seq(1, n_cols, 3), drop = FALSE], 1, max)

  comp_index <- strategy_weight *
    (aggregated_loss - min(aggregated_loss)) /
    (max(aggregated_loss) - min(aggregated_loss) + epsilon) +
    (1 - strategy_weight) *
    (worst_case_loss - min(worst_case_loss)) /
    (max(worst_case_loss) - min(worst_case_loss) + epsilon)

  results <- data.frame(
    alternative_id = seq_len(nrow(decision_matrix)),
    aggregated_loss = aggregated_loss,
    worst_case_loss = worst_case_loss,
    compromise_index = comp_index,
    final_rank = rank(comp_index, ties.method = "first")
  )

  output <- list(
    results = results,
    parameters = list(strategy_weight = strategy_weight)
  )

  class(output) <- "fuzzy_vikor_res"
  return(output)
}
