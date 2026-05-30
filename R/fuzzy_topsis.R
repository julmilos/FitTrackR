#' Metoda Fuzzy TOPSIS
#'
#' Implementuje metodę Fuzzy TOPSIS (Technique for Order Preference by Similarity
#' to Ideal Solution) dla wielokryterialnej analizy decyzyjnej z użyciem trójkątnych liczb rozmytych.
#'
#'
#' @param decision_matrix Macierz (m × 3n) lub TFN.
#' @param criteria_type Wektor ("max" / "min") dla każdego kryterium.
#' @param weights Opcjonalny wektor numeryczny lub waga.
#' @param bwm_best Opcjonalny wektor BWM najlepsze_do_innych.
#' @param bwm_worst Opcjonalny wektor BWM inne_do_najlepszego.
#' @param epsilon Mała stała numeryczna zapewniająca stabilność obliczeń
#' i zapobiegająca dzieleniu przez zero.
#'
#' @return Object of class `fuzzy_topsis_res`.
#' @export
fuzzy_topsis <- function(
    decision_matrix,
    criteria_type,
    weights   = NULL,
    bwm_best  = NULL,
    bwm_worst = NULL,
    epsilon   = 1e-9
) {
  validate_mcda_input(decision_matrix, criteria_type)
  if (!is.matrix(decision_matrix)) {
    stop("decision_matrix musi być macierza")
  }

  n_criteria <- ncol(decision_matrix) / 3
  if (length(criteria_type) != n_criteria) {
    stop("Dlugosc 'criteria_type' musi byc rowna ilosci kryteriow.")
  }

  final_weights <- get_final_weights(
    decision_matrix,
    weights   = weights,
    bwm_best  = bwm_best,
    bwm_worst = bwm_worst
  )

  fuzzy_dir <- rep(criteria_type, each = 3)
  n_cols    <- ncol(decision_matrix)

  normalized_matrix <- matrix(0, nrow(decision_matrix), n_cols)
  for (j in seq(1, n_cols, 3)) {
    if (fuzzy_dir[j] == "max") {
      max_val <- apply(decision_matrix[, j:(j + 2)], 2, max)
      normalized_matrix[, j]     <- decision_matrix[, j]     / (max_val[3] + epsilon)
      normalized_matrix[, j + 1] <- decision_matrix[, j + 1] / (max_val[3] + epsilon)
      normalized_matrix[, j + 2] <- decision_matrix[, j + 2] / (max_val[3] + epsilon)
    } else {
      min_val <- apply(decision_matrix[, j:(j + 2)], 2, min)
      normalized_matrix[, j]     <- min_val[1] / (decision_matrix[, j]     + epsilon)
      normalized_matrix[, j + 1] <- min_val[1] / (decision_matrix[, j + 1] + epsilon)
      normalized_matrix[, j + 2] <- min_val[1] / (decision_matrix[, j + 2] + epsilon)
    }
  }

  weighted_matrix <- normalized_matrix %*% diag(final_weights)

  ideal_solution      <- apply(weighted_matrix, 2, max)
  anti_ideal_solution <- apply(weighted_matrix, 2, min)

  distance_to_ideal <- sqrt(rowSums(
    (weighted_matrix - matrix(ideal_solution,      nrow(weighted_matrix), n_cols, byrow = TRUE))^2
  ))
  distance_to_anti_ideal <- sqrt(rowSums(
    (weighted_matrix - matrix(anti_ideal_solution, nrow(weighted_matrix), n_cols, byrow = TRUE))^2
  ))

  closeness_coefficient <- distance_to_anti_ideal /
    (distance_to_ideal + distance_to_anti_ideal + epsilon)

  results <- data.frame(
    alternative_id = if (!is.null(rownames(decision_matrix)))
      rownames(decision_matrix)
    else seq_len(nrow(decision_matrix)),
    distance_to_ideal      = distance_to_ideal,
    distance_to_anti_ideal = distance_to_anti_ideal,
    closeness_coefficient  = closeness_coefficient,
    score = closeness_coefficient,
    ranking = rank(-closeness_coefficient, ties.method = "first")
  )

  output <- list(
    results = results,
    details = results,
    method = "TOPSIS"
  )

  class(output) <- "fuzzy_topsis_res"
  return(output)
}
