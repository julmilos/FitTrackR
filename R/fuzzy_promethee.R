#' @title Fuzzy PROMETHEE II Method
#' @description Implements PROMETHEE II for fuzzy multi-criteria decision making
#'
#' @param decision_matrix Matrix (m × 3n) of triangular fuzzy numbers.
#' @param criteria_type Character vector ("max" / "min") for each criterion.
#' @param preference_params Parameters of preference functions.
#' @param weights Optional numeric vector of weights.
#' @param bwm_best Optional BWM best-to-others vector.
#' @param bwm_worst Optional BWM others-to-worst vector.
#'
#' @return Object of class `fuzzy_promethee_res`
#' @export
fuzzy_promethee <- function(
    decision_matrix,
    criteria_type,
    preference_params,
    weights      = NULL,
    bwm_best     = NULL,
    bwm_worst    = NULL
) {
  validate_mcda_input(decision_matrix, criteria_type)
  if (!is.matrix(decision_matrix)) {
    stop("decision_matrix must be a matrix")
  }

  n_criteria      <- ncol(decision_matrix) / 3
  n_alternatives  <- nrow(decision_matrix)

  final_weights <- get_final_weights(
    decision_matrix,
    weights   = weights,
    bwm_best  = bwm_best,
    bwm_worst = bwm_worst
  )

  # Defuzzify weights (mean of TFN)
  weights_crisp <- numeric(n_criteria)
  for (j in seq_len(n_criteria)) {
    idx <- (j - 1) * 3 + 1
    weights_crisp[j] <- mean(final_weights[idx:(idx + 2)])
  }
  weights_crisp <- weights_crisp / sum(weights_crisp)

  # Defuzzify decision matrix (weighted average of TFN)
  crisp_matrix <- matrix(0, n_alternatives, n_criteria)
  k <- 1
  for (j in seq(1, ncol(decision_matrix), 3)) {
    crisp_matrix[, k] <- (
      decision_matrix[, j] +
        4 * decision_matrix[, j + 1] +
        decision_matrix[, j + 2]
    ) / 6
    k <- k + 1
  }

  # Build preference matrix
  preference_matrix <- matrix(0, n_alternatives, n_alternatives)
  for (j in seq_len(n_criteria)) {
    pref_type <- as.character(preference_params$Type[j])
    q         <- preference_params$q[j]
    p         <- preference_params$p[j]
    s         <- preference_params$s[j]

    values <- crisp_matrix[, j]

    if (criteria_type[j] == "max") {
      diff_matrix <- outer(values, values, "-")
    } else {
      diff_matrix <- outer(values, values, "-") * -1
    }

    pref_matrix <- .oblicz_preferencje_promethee(
      diff_matrix, pref_type, q, p, s
    )
    preference_matrix <- preference_matrix + weights_crisp[j] * pref_matrix
  }
  diag(preference_matrix) <- 0

  # Compute net flows
  phi_plus  <- rowSums(preference_matrix) / (n_alternatives - 1)
  phi_minus <- colSums(preference_matrix) / (n_alternatives - 1)
  phi_net   <- phi_plus - phi_minus

  results <- data.frame(
    alternative_id = seq_len(n_alternatives),
    phi_plus       = phi_plus,
    phi_minus      = phi_minus,
    phi_net        = phi_net,
    ranking        = rank(-phi_net, ties.method = "first")
  )

  output <- list(
    results = results,
    method  = "PROMETHEE II"
  )
  class(output) <- "fuzzy_promethee_res"
  return(output)
}
