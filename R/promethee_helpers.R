# Internal helper: preference functions for PROMETHEE II
# Supported types: "I", "II", "III", "IV", "V", "VI"
.oblicz_preferencje_promethee <- function(diff_matrix, type, q, p, s) {
  n <- nrow(diff_matrix)
  pref <- matrix(0, n, n)

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      d <- diff_matrix[i, j]
      pref[i, j] <- switch(type,

                           # Typ I: Usual – każda różnica daje preferencję
                           "I" = {
                             if (d > 0) 1 else 0
                           },

                           # Typ II: U-shape – preferencja tylko powyżej progu q
                           "II" = {
                             if (d <= q) 0 else 1
                           },

                           # Typ III: V-shape – liniowa do progu p
                           "III" = {
                             if (d <= 0)     0
                             else if (d >= p) 1
                             else            d / p
                           },

                           # Typ IV: Level – skok przy q, pełna preferencja przy p
                           "IV" = {
                             if (d <= q)      0
                             else if (d >= p) 1
                             else             0.5
                           },

                           # Typ V: Linear with indifference – liniowa między q a p
                           "V" = {
                             if (d <= q)      0
                             else if (d >= p) 1
                             else            (d - q) / (p - q)
                           },

                           # Typ VI: Gaussian – krzywa Gaussa z parametrem s
                           "VI" = {
                             if (d <= 0) 0
                             else        1 - exp(-(d^2) / (2 * s^2))
                           },

                           # Domyślnie: V-shape
                           {
                             if (d <= q)      0
                             else if (d >= p) 1
                             else            (d - q) / (p - q)
                           }
      )
    }
  }
  return(pref)
}
