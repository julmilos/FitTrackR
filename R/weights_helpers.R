# Internal helper: compute final fuzzy weights
get_final_weights <- function(
    decision_matrix,
    weights   = NULL,
    bwm_best  = NULL,
    bwm_worst = NULL
) {
  n_criteria <- ncol(decision_matrix) / 3


  if (!is.null(weights)) {
    if (length(weights) != ncol(decision_matrix)) {
      stop("Dlugosc 'weights' musi byc rowna liczbie kolumn rozmytych  (3 × liczba kryteriow).")
    }
    return(weights)
  }


  if (!is.null(bwm_best) && !is.null(bwm_worst)) {

    nazwy <- paste0("C", seq_len(n_criteria))

    bwm_result    <- oblicz_wagi_bwm(
      nazwy_kryteriow       = nazwy,
      najlepsze_do_innych   = bwm_best,
      inne_do_najgorszego   = bwm_worst
    )
    crisp_weights <- bwm_result$wagi_kryteriow

    if (length(crisp_weights) != n_criteria) {
      stop("Liczba wag BWM nie zgadza sie z iloscia kryteriow.")
    }
    return(rep(crisp_weights, each = 3))
  }

  stop("Nalezy podac wagi kryteriow lub parametry metody BWM.")
}
