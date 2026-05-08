analyze_wearables <- function(macierz,
                              typy_kryteriow = NULL,
                              profile = NULL,
                              wagi = NULL,
                              preference_params = NULL) {

  if (is.null(attr(macierz, "nazwy_kryteriow"))) {
    stop("Brak atrybutu 'nazwy_kryteriow'. Użyj przygotuj_dane_mcda.")
  }

  nazwy <- attr(macierz, "nazwy_kryteriow")

  if (is.null(typy_kryteriow)) {
    typy_kryteriow <- ifelse(
      grepl("Cena", nazwy, ignore.case = TRUE),
      "min",
      "max"
    )
  }

  if (!is.null(profile)) {
    prof <- .get_lifestyle_profile(profile)
    wagi <- prof$weights
  }

  if (is.null(wagi)) {
    warning("Brak wag – użyto równych wag.")
    wagi <- rep(1, length(nazwy))
    names(wagi) <- nazwy
  }

  # CRISP weights per criterion
  wagi_rozszerzone <- sapply(nazwy, function(n) {

    if (grepl("Dokladnosc", n)) return(wagi["Dokladnosc"])
    if (grepl("Bateria", n)) return(wagi["Bateria"])
    if (grepl("Funkcje", n)) return(wagi["Funkcje"])
    if (grepl("Kompatybilnosc", n)) return(wagi["Kompatybilnosc"])
    if (grepl("Wygoda|Jakosc", n)) return(wagi["Wygoda"])
    if (grepl("Wodoodpornosc|Wytrzymalosc", n)) return(wagi["Odpornosc"])
    if (grepl("Cena", n)) return(wagi["Cena"])

    return(1)
  })

  wagi_rozszerzone <- wagi_rozszerzone / sum(wagi_rozszerzone)
  wagi_fuzzy <- rep(wagi_rozszerzone, each = 3)

  if (is.null(preference_params)) {
    preference_params <- list(
      Type = rep("V", length(nazwy)),
      q = rep(0.1, length(nazwy)),
      p = rep(0.3, length(nazwy)),
      s = rep(0, length(nazwy))
    )
  }

  wynik_topsis <- rozmyty_topsis(macierz, typy_kryteriow, wagi = wagi_fuzzy)
  wynik_vikor  <- rozmyty_vikor(macierz, typy_kryteriow, wagi = wagi_fuzzy)

  wynik_promethee <- fuzzy_promethee(
    decision_matrix = macierz,
    criteria_type = typy_kryteriow,
    preference_params = preference_params,
    weights = wagi_rozszerzone
  )

  rank_matrix <- cbind(
    TOPSIS = wynik_topsis$results$ranking,
    VIKOR = wynik_vikor$results$ranking,
    PROMETHEE = wynik_promethee$results$ranking
  )

  meta_scores <- rowMeans(rank_matrix)
  ranking_meta <- rank(meta_scores, ties.method = "first")

  names(ranking_meta) <- rownames(rank_matrix)

  list(
    TOPSIS = wynik_topsis,
    VIKOR = wynik_vikor,
    PROMETHEE = wynik_promethee,
    META = ranking_meta,
    profile = profile
  )
}
