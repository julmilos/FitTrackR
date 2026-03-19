#' @title Konsensus Teoria Dominacji
#' @keywords internal
oblicz_ranking_dominacji <- function(macierz_rang) {
  n <- nrow(macierz_rang)
  finalny_ranking <- rep(0, n)
  dostepne <- rep(TRUE, n)

  for (poz in 1:n) {
    obecna_macierz <- macierz_rang
    obecna_macierz[!dostepne, ] <- Inf
    
    kandydaci <- apply(obecna_macierz, 2, which.min)
    glosy <- table(kandydaci)
    zwyciezcy <- as.numeric(names(glosy)[glosy == max(glosy)])
    
    if (length(zwyciezcy) == 1) {
      idx <- zwyciezcy
    } else {
      # Tie-breaker: najmniejsza suma rang
      sumy <- rowSums(macierz_rang[zwyciezcy, , drop = FALSE])
      idx <- zwyciezcy[which.min(sumy)]
    }
    
    finalny_ranking[idx] <- poz
    dostepne[idx] <- FALSE
  }
  return(finalny_ranking)
}

#' @title Glowny Meta Ranking z wyposazeniem wagi dynamiczne
#' @param macierz_decyzyjna Rozmyta macierz decyzyjna.
#' @param typy_kryteriow Wektor ról kryteriów ("max" lub "min").
#' @param metoda_wag String wskazujący metodę wyznaczania wag ("equal", "sd", "gini", "bwm").
#' @param parametry_promethee Opcjonalna ramka danych (Type, q, p, s, Role) dla PROMETHEE. 
#' @param ... Parametry dodatkowe (np. 'najlepsze', 'najgorsze' dla BWM).
#' @export
meta_ranking <- function(macierz_decyzyjna, typy_kryteriow, metoda_wag = "equal", parametry_promethee = NULL, ...) {
  
  # 1. Obliczenie wag zadaną przez użytkownika metodą
  wagi <- oblicz_wagi(macierz_decyzyjna, metoda = metoda_wag, ...)
  
  # 2. Bezpiecznik dla PROMETHEE - domyślne funkcje preferencji jeśli ich brak
  if (is.null(parametry_promethee)) {
    n_kryt <- ncol(macierz_decyzyjna) / 3
    parametry_promethee <- data.frame(
      Type = rep("linear", n_kryt), # Domyślnie funkcja liniowa V-shape z progiem obojętności
      q = rep(0, n_kryt),
      p = rep(2, n_kryt), # Umowny próg preferencji, użytkownik zawsze może podać własny DF
      s = rep(NA, n_kryt),
      Role = typy_kryteriow
    )
  }
  
  # 3. Wywołanie 4 algorytmów rozmytych
  r1 <- metoda_topsis(macierz_decyzyjna, typy_kryteriow, wagi)
  r2 <- metoda_vikor(macierz_decyzyjna, typy_kryteriow, wagi)
  r3 <- metoda_waspas(macierz_decyzyjna, typy_kryteriow, wagi)
  r4 <- metoda_promethee(macierz_decyzyjna, parametry_promethee, wagi)
  
  # 4. Agregacja rankingów
  macierz_rang <- cbind(
    TOPSIS = r1$Ranking,
    VIKOR = r2$Ranking,
    WASPAS = r3$Ranking,
    PROMETHEE = r4$Ranking
  )
  
  meta_r <- oblicz_ranking_dominacji(macierz_rang)
  
  wynik <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    macierz_rang,
    Meta_Ranking = meta_r
  )
  
  return(list(wyniki = wynik, korelacje = cor(macierz_rang, method = "spearman")))
}