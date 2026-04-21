# Internal helper: compute final fuzzy weights
get_final_weights <- function(
    decision_matrix,
    weights   = NULL,
    bwm_best  = NULL,
    bwm_worst = NULL
) {
  n_criteria <- ncol(decision_matrix) / 3

  # CASE 1: explicit fuzzy weights
  if (!is.null(weights)) {
    if (length(weights) != ncol(decision_matrix)) {
      stop("Length of 'weights' must equal number of fuzzy columns (3 × criteria).")
    }
    return(weights)
  }

  # CASE 2: BWM weights
  if (!is.null(bwm_best) && !is.null(bwm_worst)) {
    # Tworzymy tymczasowe nazwy kryteriów jeśli nie podano
    nazwy <- paste0("C", seq_len(n_criteria))

    bwm_result    <- oblicz_wagi_bwm(
      nazwy_kryteriow       = nazwy,
      najlepsze_do_innych   = bwm_best,
      inne_do_najgorszego   = bwm_worst
    )
    crisp_weights <- bwm_result$wagi_kryteriow  # polska nazwa pola!

    if (length(crisp_weights) != n_criteria) {
      stop("BWM weights do not match number of criteria.")
    }
    return(rep(crisp_weights, each = 3))
  }

  stop("Provide either explicit fuzzy weights or BWM parameters.")
}
