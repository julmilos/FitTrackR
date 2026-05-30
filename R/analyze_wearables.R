#' @title Automatyczna analiza urządzeń wearable
#' @description Uruchamia pełny pipeline MCDA dla urządzeń wearable.
#' @param macierz Rozmyta macierz decyzyjna z przygotuj_dane_mcda().
#' @param typy_kryteriow Wektor typów kryteriów ("max"/"min").
#' @param profile Profil użytkownika: "athlete", "beginner", "casual".
#' @param wagi Opcjonalny wektor wag (długość = liczba kryteriów).
#' @param preference_params Parametry funkcji preferencji dla PROMETHEE.
#' @return Lista z wynikami TOPSIS, VIKOR, PROMETHEE i META.
#' @export
analyze_wearables <- function(macierz,
                              typy_kryteriow = NULL,
                              profile = NULL,
                              wagi = NULL,
                              preference_params = NULL) {

  if (is.null(attr(macierz, "nazwy_kryteriow"))) {
    stop("Brak atrybutu 'nazwy_kryteriow'. Uzyj przygotuj_dane_mcda().")
  }

  nazwy <- attr(macierz, "nazwy_kryteriow")
  n_kryteriow <- length(nazwy)

  if (is.null(typy_kryteriow)) {
    typy_kryteriow <- ifelse(
      grepl("Cena", nazwy, ignore.case = TRUE), "min", "max"
    )
  }

  if (!is.null(profile)) {
    prof <- .get_lifestyle_profile(profile)
    wagi_profilu <- prof$weights

    wagi <- sapply(nazwy, function(n) {
      if (grepl("Dokladnosc",     n, ignore.case = TRUE)) return(wagi_profilu["Dokladnosc"])
      if (grepl("Bateria",        n, ignore.case = TRUE)) return(wagi_profilu["Bateria"])
      if (grepl("Funkcjonalnosc", n, ignore.case = TRUE)) return(wagi_profilu["Funkcjonalnosc"])
      if (grepl("Kompatybilnosc", n, ignore.case = TRUE)) return(wagi_profilu["Kompatybilnosc"])
      if (grepl("Komfort",        n, ignore.case = TRUE)) return(wagi_profilu["Komfort"])
      if (grepl("Odpornosc",      n, ignore.case = TRUE)) return(wagi_profilu["Odpornosc"])
      if (grepl("Cena",           n, ignore.case = TRUE)) return(wagi_profilu["Cena"])
      return(1 / n_kryteriow)
    })
    wagi <- as.numeric(wagi)
    wagi <- wagi / sum(wagi, na.rm = TRUE)
  }

  if (is.null(wagi)) {
    warning("Brak profilu oraz wag. Uzyte zostaly rowne wagi.")
    wagi <- rep(1 / n_kryteriow, n_kryteriow)
  }

  wagi_fuzzy <- rep(wagi, each = 3)

  if (is.null(preference_params)) {
    preference_params <- data.frame(
      Type = rep("V", n_kryteriow),
      q    = rep(0,   n_kryteriow),
      p    = rep(1,   n_kryteriow),
      s    = rep(0.5, n_kryteriow)
    )
  }

  wynik_vikor <- fuzzy_vikor(
    decision_matrix = macierz,
    criteria_type   = typy_kryteriow,
    weights         = wagi_fuzzy
  )

  wynik_topsis <- fuzzy_topsis(
    decision_matrix = macierz,
    criteria_type   = typy_kryteriow,
    weights         = wagi_fuzzy
  )

  wynik_promethee <- fuzzy_promethee(
    decision_matrix   = macierz,
    criteria_type     = typy_kryteriow,
    preference_params = preference_params,
    weights           = wagi_fuzzy
  )

  rank_matrix <- cbind(
    VIKOR     = wynik_vikor$results$ranking,
    TOPSIS    = wynik_topsis$results$ranking,
    PROMETHEE = wynik_promethee$results$ranking
  )

  meta_scores  <- rowMeans(rank_matrix)
  ranking_meta <- rank(meta_scores, ties.method = "first")

  if (!is.null(rownames(macierz))) {
    names(ranking_meta) <- rownames(macierz)
  }

  list(
    TOPSIS    = wynik_topsis,
    VIKOR     = wynik_vikor,
    PROMETHEE = wynik_promethee,
    META      = ranking_meta,
    wagi      = wagi,
    profile   = profile
  )
}
