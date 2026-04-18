analyze_wearables <- function(macierz,
                              typy_kryteriow = NULL,
                              profile = NULL,
                              wagi = NULL) {


  if (is.null(attr(macierz, "nazwy_kryteriow"))) {
    stop("Brak atrybutu 'nazwy_kryteriow'. Użyj funkcji przygotuj_dane_mcda.")
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
    warning("Brak wag – rozważ użycie profilu lub Entropii.")
    wagi <- rep(1, length(nazwy))
  }


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

  wynik_topsis <- rozmyty_topsis(macierz, typy_kryteriow, wagi = wagi_fuzzy)
  wynik_vikor  <- rozmyty_vikor(macierz, typy_kryteriow, wagi = wagi_fuzzy)

  rank_matrix <- cbind(
    TOPSIS = wynik_topsis$wyniki$Ranking,
    VIKOR  = wynik_vikor$wyniki$Ranking
  )

  ranking_meta <- rank(rowSums(rank_matrix), ties.method = "first")

  return(list(
    TOPSIS = wynik_topsis,
    VIKOR = wynik_vikor,
    META = ranking_meta,
    profile = profile
  ))
}
