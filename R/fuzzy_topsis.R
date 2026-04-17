#' Fuzzy TOPSIS Decision Method
#'
#' Implements a fuzzy TOPSIS (Technique for Order Preference by Similarity to Ideal Solution)
#' for multi-criteria decision-making using triangular fuzzy numbers.
#'
#' @param decision_matrix Matrix (m × 3n) of triangular fuzzy numbers.
#' @param criteria_direction Character vector ("max" / "min") for each criterion.
#' @param weights Optional numeric vector of weights.
#' @param bwm_criteria Optional criteria names for BWM weighting.
#' @param bwm_best Optional BWM best-to-others vector.
#' @param bwm_worst Optional BWM others-to-worst vector.
#' @param epsilon Small numeric constant for numerical stability.
#'
#' @return Object of class `fuzzy_topsis_res`.
#' @export
fuzzy_topsis <- function(
    decision_matrix,
    criteria_direction,
    weights = NULL,
    bwm_criteria = NULL,
    bwm_best = NULL,
    bwm_worst = NULL,
    epsilon = 1e-9
) {

  if (!is.matrix(decision_matrix)) {
    stop("'decision_matrix' must be a matrix.")
  }

  n_criteria <- ncol(decision_matrix) / 3

  if (length(criteria_direction) != n_criteria) {
    stop("Length of 'criteria_direction' must match number of criteria.")
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

  normalized_matrix <- matrix(0, nrow(decision_matrix), n_cols)

  for (j in seq(1, n_cols, 3)) {

    if (fuzzy_dir[j] == "max") {
      max_val <- apply(decision_matrix[, j:(j+2)], 2, max)

      normalized_matrix[, j]   <- decision_matrix[, j]   / (max_val[3] + epsilon)
      normalized_matrix[, j+1] <- decision_matrix[, j+1] / (max_val[3] + epsilon)
      normalized_matrix[, j+2] <- decision_matrix[, j+2] / (max_val[3] + epsilon)

    } else {
      min_val <- apply(decision_matrix[, j:(j+2)], 2, min)

      normalized_matrix[, j]   <- min_val[1] / (decision_matrix[, j]   + epsilon)
      normalized_matrix[, j+1] <- min_val[1] / (decision_matrix[, j+1] + epsilon)
      normalized_matrix[, j+2] <- min_val[1] / (decision_matrix[, j+2] + epsilon)
    }
  }

  weighted_matrix <- normalized_matrix %*% diag(final_weights)

  ideal_solution <- apply(weighted_matrix, 2, max)
  anti_ideal_solution <- apply(weighted_matrix, 2, min)

  distance_to_ideal <- sqrt(rowSums((weighted_matrix - matrix(ideal_solution, nrow(weighted_matrix), n_cols, byrow = TRUE))^2))
  distance_to_anti_ideal <- sqrt(rowSums((weighted_matrix - matrix(anti_ideal_solution, nrow(weighted_matrix), n_cols, byrow = TRUE))^2))

  closeness_coefficient <- distance_to_anti_ideal /
    (distance_to_ideal + distance_to_anti_ideal + epsilon)

  results <- data.frame(
    alternative_id = seq_len(nrow(decision_matrix)),
    distance_to_ideal = distance_to_ideal,
    distance_to_anti_ideal = distance_to_anti_ideal,
    closeness_coefficient = closeness_coefficient,
    ranking_position = rank(-closeness_coefficient, ties.method = "first")
  )

  output <- list(
    results = results,
    parameters = list()
  )

  class(output) <- "fuzzy_topsis_res"
  return(output)
}
