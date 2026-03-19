#' @title Analiza Wrazliwosci Leave-One-Out
#' @description Usuwa iteracyjnie jedna alternatywe i bada korelacje Spearmana.
#' @export
analiza_wrazliwosci_loo <- function(macierz_decyzyjna, typy_kryteriow, metoda_wag = "equal", ...) {
  n_alt <- nrow(macierz_decyzyjna)
  
  # Wynik referencyjny (pelna macierz)
  baza <- meta_ranking(macierz_decyzyjna, typy_kryteriow, metoda_wag, ...)
  oryginalny_ranking <- baza$wyniki$Meta_Ranking
  
  # Przygotowanie struktur wyjsciowych
  macierz_rang <- matrix(NA, nrow = n_alt, ncol = n_alt + 1)
  rownames(macierz_rang) <- rownames(macierz_decyzyjna)
  colnames(macierz_rang) <- c("Oryginalny", paste0("Bez_Alt_", 1:n_alt))
  macierz_rang[, 1] <- oryginalny_ranking
  
  korelacje <- numeric(n_alt)
  
  # Iteracja Leave-One-Out
  for(i in 1:n_alt) {
    # Usuniecie i-tego wiersza
    sub_mat <- macierz_decyzyjna[-i, , drop = FALSE]
    sub_res <- meta_ranking(sub_mat, typy_kryteriow, metoda_wag, ...)
    sub_ranking <- sub_res$wyniki$Meta_Ranking
    
    # Zapisanie rankingów (uzupelnienie luki)
    nowa_kolumna <- rep(NA, n_alt)
    nowa_kolumna[-i] <- sub_ranking
    macierz_rang[, i + 1] <- nowa_kolumna
    
    # Obliczenie korelacji dla "ocalałych" alternatyw
    korelacje[i] <- cor(oryginalny_ranking[-i], sub_ranking, method = "spearman")
  }
  
  return(list(
    Macierz_Rankingow = macierz_rang,
    Korelacje_Spearmana = setNames(korelacje, colnames(macierz_rang)[-1])
  ))
}