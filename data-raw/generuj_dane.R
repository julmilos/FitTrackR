# data-raw/generuj_dane.R
# ============================================
# Dane symulacyjne ekspertów – skale Likerta
# ============================================

set.seed(123)

alternatywy <- c("Samsung Galaxy Watch", "AppleWatch", "Garmin")
liczba_ekspertow <- 15
n <- liczba_ekspertow * length(alternatywy)

mcda_dane_surowe <- data.frame(
  # --- Identyfikatory ---
  EkspertID = rep(1:liczba_ekspertow, each = length(alternatywy)),
  Alternatywa = rep(alternatywy, times = liczba_ekspertow),

  # === Kryterium 1: Dokładność pomiarów (Likert 1–10) ===
  Tetno_Dokladnosc  = sample(6:10, n, replace = TRUE),
  Kroki_Dokladnosc  = sample(6:10, n, replace = TRUE),
  Kalorie_Dokladnosc = sample(5:10, n, replace = TRUE),

  # === Kryterium 2: Bateria (Likert 1–7) ===
  Bateria_Wydajnosc = sample(2:7, n, replace = TRUE),

  # === Kryterium 3: Funkcjonalność (Likert 1–7) ===
  Funkcje_Zdrowotne = sample(3:7, n, replace = TRUE),
  Funkcje_Sportowe  = sample(3:7, n, replace = TRUE),
  Funkcje_Smart     = sample(2:7, n, replace = TRUE),

  # === Kryterium 4: Kompatybilność (Likert 1–5) ===
  Kompatybilnosc_Android = sample(1:5, n, replace = TRUE),
  Kompatybilnosc_iOS     = sample(1:5, n, replace = TRUE),

  # === Kryterium 5: Komfort i jakość (Likert 1–7) ===
  Wygoda_Noszenia  = sample(3:7, n, replace = TRUE),
  Jakosc_Wykonania = sample(4:7, n, replace = TRUE),

  # === Kryterium 6: Odporność (Likert 1–7) ===
  Wodoodpornosc = sample(3:7, n, replace = TRUE),
  Wytrzymalosc  = sample(3:7, n, replace = TRUE),

  # === Kryterium 7: Cena (kryterium kosztowe) ===
  Cena_PLN = c(
    runif(liczba_ekspertow, 150, 300),      # Samsung Galaxy Watch
    runif(liczba_ekspertow, 1800, 2500),    # AppleWatch
    runif(liczba_ekspertow, 900, 1800)      # Garmin
  )
)

# zapis danych do pakietu
usethis::use_data(mcda_dane_surowe, overwrite = TRUE)
