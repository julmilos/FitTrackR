#' @title Teoria Dominacji dla Rankingu
#' @description Funkcja pomocnicza do konsensusu.
#' @keywords internal
calculate_dominance_ranking <- function(rank_mat) {
  n <- nrow(rank_mat)
  final_rank <- rep(0, n)
  available <- rep(TRUE, n)

  for (current_position in 1:n) {
    current_mat <- rank_mat
    current_mat[!available, ] <- Inf

    candidates <- apply(current_mat, 2, which.min)
    freq_table <- table(candidates)

    max_votes <- max(freq_table)
    winners <- as.numeric(names(freq_table)[freq_table == max_votes])

    if (length(winners) == 1) {
      winner_idx <- winners
    } else {
      sums <- rowSums(rank_mat[winners, , drop = FALSE])
      winner_idx <- winners[which.min(sums)]
    }

    final_rank[winner_idx] <- current_position
    available[winner_idx] <- FALSE
  }

  return(final_rank)
}

#' @title Rozmyty Meta-Ranking (3 Metody)
#' @description Agreguje wyniki: VIKOR, TOPSIS, PROMETHEE.
#'
#' @param decision_matrix Rozmyta macierz decyzyjna.
#' @param criteria_type Wektor typów kryteriów ("min", "max").
#' @param weights Wektor wag (opcjonalny).
#' @param preference_params Ramka parametrów dla PROMETHEE (opcjonalna).
#' @param bwm_best,bwm_worst Parametry BWM.
#' @param v Parametr VIKOR (waga strategii grupowej).
#'
#' @export
#' @importFrom stats cor
fuzzy_meta_ranking <- function(
    decision_matrix,
    criteria_type,
    weights = NULL,
    preference_params = NULL,
    bwm_best = NULL,
    bwm_worst = NULL,
    v = 0.5
) {

  n_crit <- ncol(decision_matrix) / 3

  # Wagi (entropy jeśli brak BWM i weights)
  if (is.null(weights) && (is.null(bwm_best) || is.null(bwm_worst))) {
    message("Brak wag. Obliczam Entropię...")
    weights_raw <- oblicz_wagi_entropii(decision_matrix)
    weights <- rep(weights_raw, each = 3)
  }

  # Domyślne parametry PROMETHEE
  if (is.null(preference_params)) {
    preference_params <- data.frame(
      Type = rep("V", n_crit),
      q    = rep(0,   n_crit),
      p    = rep(1,   n_crit),
      s    = rep(0.5, n_crit)
    )
  }

  # =========================
  # VIKOR
  # =========================
  res_vikor <- do.call(
    fuzzy_vikor,
    c(
      list(
        decision_matrix = decision_matrix,
        criteria_type   = criteria_type,
        strategy_weight = v
      ),
      if (!is.null(weights))   list(weights = weights),
      if (!is.null(bwm_best))  list(bwm_best = bwm_best),
      if (!is.null(bwm_worst)) list(bwm_worst = bwm_worst)
    )
  )

  # =========================
  # TOPSIS
  # =========================
  res_topsis <- do.call(
    fuzzy_topsis,
    c(
      list(
        decision_matrix = decision_matrix,
        criteria_type   = criteria_type
      ),
      if (!is.null(weights))   list(weights = weights),
      if (!is.null(bwm_best))  list(bwm_best = bwm_best),
      if (!is.null(bwm_worst)) list(bwm_worst = bwm_worst)
    )
  )

  # =========================
  # PROMETHEE
  # =========================
  res_prom <- do.call(
    fuzzy_promethee,
    c(
      list(
        decision_matrix   = decision_matrix,
        criteria_type     = criteria_type,
        preference_params = preference_params
      ),
      if (!is.null(weights))   list(weights = weights),
      if (!is.null(bwm_best))  list(bwm_best = bwm_best),
      if (!is.null(bwm_worst)) list(bwm_worst = bwm_worst)
    )
  )

  # =========================
  # MATRIX RANKING
  # =========================
  rank_matrix <- cbind(
    res_vikor$results$ranking,
    res_topsis$results$ranking,
    res_prom$results$ranking
  )
  colnames(rank_matrix) <- c("VIKOR", "TOPSIS", "PROMETHEE")

  # Meta-ranking methods
  rank_sum <- rank(rowSums(rank_matrix), ties.method = "first")
  rank_dom <- calculate_dominance_ranking(rank_matrix)

  # Rank aggregation
  n_alt <- nrow(decision_matrix)
  ra_input <- t(apply(rank_matrix, 2, order))

  if (n_alt <= 10) {
    ra <- RankAggreg::BruteAggreg(ra_input, n_alt, distance = "Spearman")
  } else {
    ra <- RankAggreg::RankAggreg(
      ra_input,
      n_alt,
      method = "GA",
      distance = "Spearman",
      verbose = FALSE
    )
  }

  rank_ra <- numeric(n_alt)
  top_list <- ra$top.list

  for (i in seq_len(n_alt)) {
    rank_ra[top_list[i]] <- i
  }

  # =========================
  # OUTPUT
  # =========================
  porownanie <- data.frame(
    Alternatywa    = res_vikor$results$alternative_id,
    R_VIKOR        = rank_matrix[, 1],
    R_TOPSIS       = rank_matrix[, 2],
    R_PROMETHEE    = rank_matrix[, 3],
    Meta_Suma      = rank_sum,
    Meta_Dominacja = rank_dom,
    Meta_Agregacja = rank_ra
  )

  return(list(
    porownanie = porownanie,
    korelacje  = cor(rank_matrix, method = "spearman")
  ))
}
