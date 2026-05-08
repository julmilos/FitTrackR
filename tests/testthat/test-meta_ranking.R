test_that("fuzzy_meta_ranking works end-to-end", {

  library(FitTrackR)

  data("mcda_dane_surowe")

  skladnia_modelu <- "
  A =~ a1 + a2;
  B =~ b1 + b2;
  C =~ c1 + c2
  "

  macierz <- przygotuj_dane_mcda(
    dane = mcda_dane_surowe,
    skladnia = skladnia_modelu,
    kolumna_alternatyw = "Alternatywa"
  )

  criteria_type <- c("max","max","min")

  bwm_best  <- c(1,3,4)
  bwm_worst <- c(3,2,1)

  meta <- fuzzy_meta_ranking(
    decision_matrix = macierz,
    criteria_type   = criteria_type,
    bwm_best        = bwm_best,
    bwm_worst       = bwm_worst
  )

  # 1. struktura
  expect_true(is.data.frame(meta$porownanie))

  # 2. wymagane kolumny
  expect_true(all(c(
    "R_VIKOR",
    "R_TOPSIS",
    "R_PROMETHEE",
    "Meta_Suma",
    "Meta_Dominacja",
    "Meta_Agregacja"
  ) %in% names(meta$porownanie)))

  # 3. brak NA w rankingach
  expect_false(any(is.na(meta$porownanie$Meta_Suma)))

  # 4. korelacje istnieją
  expect_true(is.matrix(meta$korelacje))
})
