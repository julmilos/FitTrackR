#' Metoda Fuzzy VIKOR
#'
#' Implementuje metodę Fuzzy VIKOR dla wielokryterialnej analizy decyzyjnej
#' w oparciu o ranking kompromisowy.
#'
#' @param decision_matrix Rozmyta macierz decyzyjna (m × 3n) zawierająca
#' trójkątne liczby rozmyte (TFN).
#' @param criteria_type Wektor typów kryteriów ("max" dla kryteriów zysków,
#' "min" dla kryteriów kosztów).
#' @param strategy_weight Wartość z przedziału [0,1] określająca znaczenie
#' użyteczności grupowej w metodzie VIKOR.
#' @param weights Opcjonalny wektor wag kryteriów w postaci rozmytej
#' (długość 3n).
#' @param bwm_best Opcjonalny wektor porównań najlepszego kryterium
#' względem pozostałych w metodzie BWM.
#' @param bwm_worst Opcjonalny wektor porównań pozostałych kryteriów
#' względem najgorszego w metodzie BWM.
#' @param epsilon Mała stała numeryczna zapewniająca stabilność obliczeń
#' i zapobiegająca dzieleniu przez zero.
#'
#' @return Object of class `fuzzy_vikor_res`.
#' @export
fuzzy_vikor <- function(
    decision_matrix,
    criteria_type,
    strategy_weight = 0.5,
    weights   = NULL,
    bwm_best  = NULL,
    bwm_worst = NULL,
    epsilon   = 1e-9
) {
  validate_mcda_input(decision_matrix, criteria_type)
  if (!is.matrix(decision_matrix)) {
    stop("decision_matrix musi byc macierza")
  }

  n_crit <- ncol(decision_matrix) / 3
  if (length(criteria_type) != n_crit) {
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
      distance_matrix[, i]     <- (positive_ideal[i]     - decision_matrix[, i + 2]) / denom
      distance_matrix[, i + 1] <- (positive_ideal[i + 1] - decision_matrix[, i + 1]) / denom
      distance_matrix[, i + 2] <- (positive_ideal[i + 2] - decision_matrix[, i])     / denom
    } else {
      distance_matrix[, i]     <- (decision_matrix[, i]     - positive_ideal[i + 2]) / denom
      distance_matrix[, i + 1] <- (decision_matrix[, i + 1] - positive_ideal[i + 1]) / denom
      distance_matrix[, i + 2] <- (decision_matrix[, i + 2] - positive_ideal[i])     / denom
    }
  }

  weighted_distances <- distance_matrix %*% diag(final_weights)

  aggregated_loss  <- rowSums(weighted_distances[, seq(1, n_cols, 3), drop = FALSE])
  worst_case_loss  <- apply(weighted_distances[, seq(1, n_cols, 3), drop = FALSE], 1, max)

  comp_index <- strategy_weight *
    (aggregated_loss - min(aggregated_loss)) /
    (max(aggregated_loss) - min(aggregated_loss) + epsilon) +
    (1 - strategy_weight) *
    (worst_case_loss - min(worst_case_loss)) /
    (max(worst_case_loss) - min(worst_case_loss) + epsilon)

  results <- data.frame(
    alternative_id = if (!is.null(rownames(decision_matrix)))
      rownames(decision_matrix)
    else seq_len(nrow(decision_matrix)),
    aggregated_loss  = aggregated_loss,
    worst_case_loss  = worst_case_loss,
    compromise_index = comp_index,
    score = comp_index,
    ranking = rank(comp_index, ties.method = "first")
  )

  output <- list(
    results = results,
    details = results,
    method = "VIKOR"
  )

  class(output) <- "fuzzy_vikor_res"
  return(output)
}
