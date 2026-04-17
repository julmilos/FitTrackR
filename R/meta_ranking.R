# Internal helper: dominance-based ranking
.calculate_dominance_ranking <- function(rank_matrix) {

  n <- nrow(rank_matrix)
  final_rank <- rep(0, n)
  available <- rep(TRUE, n)

  for (pos in 1:n) {

    current <- rank_matrix
    current[!available, ] <- Inf

    best_candidates <- apply(current, 2, which.min)
    freq <- table(best_candidates)

    max_votes <- max(freq)
    winners <- as.numeric(names(freq)[freq == max_votes])

    if (length(winners) == 1) {
      winner <- winners
    } else {
      sums <- rowSums(rank_matrix[winners, , drop = FALSE])
      winner <- winners[which.min(sums)]
    }

    final_rank[winner] <- pos
    available[winner] <- FALSE
  }

  return(final_rank)
}


#' Fuzzy Meta-Ranking (VIKOR + TOPSIS)
#'
#' Aggregates rankings from multiple MCDA methods using consensus logic.
#'
#' @export
fuzzy_meta_ranking <- function(
    decision_matrix,
    criteria_direction,
    weights = NULL,
    v = 0.5
) {

  res_vikor <- fuzzy_vikor(
    decision_matrix,
    criteria_direction,
    strategy_weight = v,
    weights = weights
  )

  res_topsis <- fuzzy_topsis(
    decision_matrix,
    criteria_direction,
    weights = weights
  )

  rank_matrix <- cbind(
    res_vikor$results$ranking_pozycja,
    res_topsis$results$ranking_position
  )

  colnames(rank_matrix) <- c("VIKOR", "TOPSIS")

  rank_sum <- rank(rowSums(rank_matrix), ties.method = "first")
  rank_dom <- .calculate_dominance_ranking(rank_matrix)

  comparison <- data.frame(
    Alternative = seq_len(nrow(decision_matrix)),
    VIKOR = rank_matrix[,1],
    TOPSIS = rank_matrix[,2],
    Meta_Sum = rank_sum,
    Meta_Dominance = rank_dom
  )

  return(list(
    comparison = comparison,
    correlation = cor(comparison[,2:3], method = "spearman")
  ))
}
