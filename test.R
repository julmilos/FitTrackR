# ==============================================================================
# KOMPLEKSOWY SKRYPT TESTOWY PAKIETU MCDA
# Uruchom po: devtools::document(); devtools::install(); devtools::load_all()
# ==============================================================================

# Wymagane biblioteki zewnętrzne do testów
library(ggplot2)
library(flextable)
library(Rglpk)    # Do metody BWM
library(rempsyc)  # Do tabel APA

cat("=== ROZPOCZĘCIE TESTÓW PAKIETU MCDA ===\n\n")

# ------------------------------------------------------------------------------
# KROK 1: GENEROWANIE DANYCH (Zapewnienie powtarzalności)
# ------------------------------------------------------------------------------
cat("[1/8] Generowanie danych testowych...\n")
set.seed(2026) # Seed dla odtwarzalności wyników

# Symulujemy ankietę od 5 ekspertów dla 4 różnych smartwatchy
dane_surowe <- data.frame(
  ID_Eksperta = rep(1:5, each = 4),
  Alternatywa = rep(c("AppleWatch", "Garmin", "Samsung", "Polar"), times = 5),
  
  # Zmienne dla kryterium "Funkcje" (Zysk, 1-10)
  f_sport = sample(5:10, 20, replace = TRUE),
  f_smart = sample(3:10, 20, replace = TRUE),
  
  # Zmienne dla kryterium "Bateria" (Zysk, 1-7)
  bat_czas = sample(2:7, 20, replace = TRUE),
  
  # Zmienne dla kryterium "Koszt" (Koszt, ciągłe - cena w PLN)
  # Eksperci szacują rynkową wartość / opłacalność (im więcej tym gorzej dla kosztu)
  cena_est = sample(1000:4000, 20, replace = TRUE)
)

# ------------------------------------------------------------------------------
# KROK 2: PRZYGOTOWANIE DANYCH (Składnia i Fuzzification)
# ------------------------------------------------------------------------------
cat("[2/8] Przygotowanie rozmytej macierzy decyzyjnej...\n")

skladnia_modelu <- "
  Funkcje =~ f_sport + f_smart;
  Bateria =~ bat_czas;
  Koszty  =~ cena_est
"

macierz_rozmyta <- przygotuj_dane_mcda(
  dane = dane_surowe, 
  skladnia = skladnia_modelu, 
  kolumna_alternatyw = "Alternatywa", 
  funkcja_agregacji = mean
)

# Definiujemy role dla 3 wygenerowanych kryteriów (Funkcje, Bateria, Koszty)
typy_kryt <- c("max", "max", "min")

print(head(macierz_rozmyta))


# ------------------------------------------------------------------------------
# KROK 3: TESTOWANIE METOD WAG
# ------------------------------------------------------------------------------
cat("\n[3/8] Testowanie metod wyznaczania wag...\n")

# A. Wagi Equal
wagi_eq <- oblicz_wagi(macierz_rozmyta, metoda = "equal")
cat("- Wagi Equal obliczone.\n")

# B. Wagi SD
wagi_sd <- oblicz_wagi(macierz_rozmyta, metoda = "sd")
cat("- Wagi SD obliczone.\n")

# C. Wagi Gini
wagi_gi <- oblicz_wagi(macierz_rozmyta, metoda = "gini")
cat("- Wagi Gini obliczone.\n")

# D. Wagi BWM (Best-Worst Method)
# Załóżmy, że "Funkcje" (Kryt 1) jest Najlepsze, a "Koszty" (Kryt 3) Najgorsze
# Skala 1-9: 
# Najlepsze do reszty: Funkcje do Funkcji(1), Funkcje do Baterii(3), Funkcje do Kosztow(5)
wektor_najlepsze <- c(1, 3, 5) 
# Reszta do Najgorszego: Funkcje do Kosztow(5), Bateria do Kosztow(2), Koszty do Kosztow(1)
wektor_najgorsze <- c(5, 2, 1)

wagi_bwm <- oblicz_wagi(
  macierz_rozmyta, 
  metoda = "bwm", 
  najlepsze = wektor_najlepsze, 
  najgorsze = wektor_najgorsze
)
cat("- Wagi BWM obliczone.\n")

p_wagi <- plot_wagi(wagi_sd, attr(macierz_rozmyta, "nazwy_kryteriow"))
print(p_wagi) 

# ------------------------------------------------------------------------------
# KROK 4: TESTOWANIE INDYWIDUALNYCH METOD MCDA
# ------------------------------------------------------------------------------
cat("\n[4/8] Testowanie algorytmów Fuzzy MCDA...\n")

# Użyjemy wag BWM do testów
wynik_topsis <- metoda_topsis(macierz_rozmyta, typy_kryt, wagi_bwm)
wynik_vikor <- metoda_vikor(macierz_rozmyta, typy_kryt, wagi_bwm, v = 0.5)
wynik_waspas <- metoda_waspas(macierz_rozmyta, typy_kryt, wagi_bwm, lambda = 0.5)

# Konfiguracja specyficzna dla PROMETHEE II
parametry_prom <- data.frame(
  Type = c("v-shape", "usual", "linear"),
  q = c(0, 0, 1), # Próg obojętności
  p = c(2, 0, 3), # Próg preferencji
  s = c(NA, NA, NA),
  Role = typy_kryt
)
wynik_promethee <- metoda_promethee(macierz_rozmyta, parametry_prom, wagi_bwm)

cat("- TOPSIS, VIKOR, WASPAS, PROMETHEE II przeliczone bez błędów.\n")


# ------------------------------------------------------------------------------
# KROK 5: TEST META-RANKINGU
# ------------------------------------------------------------------------------
cat("\n[5/8] Generowanie Meta-Rankingu...\n")

wynik_meta <- meta_ranking(
  macierz_decyzyjna = macierz_rozmyta, 
  typy_kryteriow = typy_kryt, 
  metoda_wag = "gini", # Zmieniamy wagi dla testu zintegrowanego
  parametry_promethee = parametry_prom
)

print(wynik_meta$wyniki)
print(wynik_meta$korelacje)


# ------------------------------------------------------------------------------
# KROK 6: TEST ANALIZY WRAŻLIWOŚCI (Leave-One-Out)
# ------------------------------------------------------------------------------
cat("\n[6/8] Przeprowadzanie Analizy Wrażliwości (Leave-One-Out)...\n")

wynik_wrazliwosci <- analiza_wrazliwosci_loo(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = typy_kryt,
  metoda_wag = "equal"
)

print(wynik_wrazliwosci$Korelacje_Spearmana)


# ------------------------------------------------------------------------------
# KROK 7: TESTOWANIE WIZUALIZACJI (Generowanie i zapis)
# ------------------------------------------------------------------------------
cat("\n[7/8] Generowanie wykresów...\n")

# Zapisujemy wykresy do plików, żeby udowodnić, że działają
dir.create("test_output", showWarnings = FALSE)

p_wagi <- plot_wagi(wagi_bwm, attr(macierz_rozmyta, "nazwy_kryteriow"))
ggsave("test_output/plot_wagi.png", p_wagi, width = 8, height = 6)

# Musimy sztucznie dodać nazwy klas, ponieważ nie robimy tego w funkcjach metody_*. 
# Dobra praktyka: dodaj class(wynik) <- "rozmyty_topsis_wynik" wewnątrz rozmyte_mcda.R
# Na potrzeby testu dodajemy je tutaj:
wynik_topsis_obj <- list(wyniki = wynik_topsis); class(wynik_topsis_obj) <- "rozmyty_topsis_wynik"
wynik_vikor_obj <- list(wyniki = wynik_vikor); class(wynik_vikor_obj) <- "rozmyty_vikor_wynik"
wynik_waspas_obj <- list(wyniki = wynik_waspas); class(wynik_waspas_obj) <- "rozmyty_waspas_wynik"
wynik_prom_obj <- list(wyniki = wynik_promethee); class(wynik_prom_obj) <- "rozmyty_promethee_wynik"

# Wydobycie czystych danych (data.frame) z TOPSIS
surowe_dane_topsis <- wynik_topsis_obj$wyniki
print(surowe_dane_topsis)

# Wydobycie czystych danych z Metarankingu
surowe_dane_meta <- wynik_meta$wyniki
print(surowe_dane_meta)

# Wydobycie samej macierzy korelacji
korelacje_meta <- wynik_meta$korelacje
print(korelacje_meta)

p_topsis <- plot(wynik_topsis_obj)
ggsave("test_output/plot_topsis.png", p_topsis, width = 8, height = 6)

p_vikor <- plot(wynik_vikor_obj)
ggsave("test_output/plot_vikor.png", p_vikor, width = 8, height = 6)

p_waspas <- plot(wynik_waspas_obj)
ggsave("test_output/plot_waspas.png", p_waspas, width = 8, height = 6)

p_promethee <- plot(wynik_prom_obj)
ggsave("test_output/plot_promethee.png", p_promethee, width = 8, height = 6)

cat("- Wykresy zapisane w folderze 'test_output/'.\n")


# ------------------------------------------------------------------------------
# KROK 8: TESTOWANIE TABEL APA (Zapis do Worda)
# ------------------------------------------------------------------------------
cat("\n[8/8] Zapisywanie Tabel APA do pliku Word (DOCX)...\n")

tab_topsis <- tabela_apa(wynik_topsis_obj)
tab_vikor <- tabela_apa(wynik_vikor_obj)
tab_waspas <- tabela_apa(wynik_waspas_obj)
tab_prom <- tabela_apa(wynik_prom_obj)
tab_meta <- tabela_apa(wynik_meta) # meta_ranking od razu zwraca poprawny obiekt

# Tworzymy jeden zbiorczy dokument Word
dokument <- flextable::save_as_docx(
  "Tabela 1: TOPSIS" = tab_topsis,
  "Tabela 2: VIKOR" = tab_vikor,
  "Tabela 3: WASPAS" = tab_waspas,
  "Tabela 4: PROMETHEE" = tab_prom,
  "Tabela 5: Meta-Ranking" = tab_meta,
  path = "test_output/Raport_MCDA_APA.docx"
)

cat("- Plik 'Raport_MCDA_APA.docx' wygenerowany.\n")
cat("\n=== TESTY ZAKOŃCZONE SUKCESEM ===\n")