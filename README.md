
# FitTrackR

<!-- badges: start -->

<!-- badges: end -->

*FitTrackR* to pakiet języka **R** przeznaczony do wspomagania decyzji
w ramach **wielokryterialnej analizy decyzyjnej (MCDA)** przy wyborze
urządzeń do monitorowania aktywności fizycznej, takich jak
**smartwatche i opaski fitness**.

Pakiet łączy **logikę rozmytą (TFN)** z zaawansowanymi metodami MCDA,
umożliwiając obiektywne porównywanie urządzeń na podstawie
parametrów technicznych, funkcjonalnych i kosztowych.

---

## Funkcje pakietu

- *Przygotowanie danych rozmytych* z ocen ekspertów i danych liczbowych
- *Fuzzy MCDA*: implementacja metod **Fuzzy VIKOR** oraz **Fuzzy PROMETHEE**
- *Best–Worst Method (BWM)*: wyznaczanie wag kryteriów na podstawie
  porównań eksperckich
- *Meta-ranking*: agregacja wyników z wielu metod w jeden stabilny
  ranking konsensusu
- *Wizualizacja S3*: intuicyjne wykresy i mapy decyzyjne
- *Eksport wyników*: tabele wynikowe w stylu APA

---

## Instalacja

Możesz zainstalować wersję deweloperską z GitHub:

```r
# install.packages("devtools")
devtools::install_github("julmilos/FitTrackR")
```
## Szybki start
Oto podstawowy przykład użycia pakietu z wykorzystaniem danych symulowanych zgodnych z założeniami pracy:

3 alternatywy(Samsung Galaxy Watch, Apple Watch, Garmin)

15 ekspertów

7 kryteriów
{r example}
library(FitTrackR)

# 1. Wczytaj dane
data("mcda_dane_surowe")
head(mcda_dane_surowe)

# 2. Przygotuj macierz decyzyjną
skladnia <- "
Dokladnosc =~ Tetno_Dokladnosc + Kroki_Dokladnosc + Kalorie_Dokladnosc
Funkcjonalnosc =~ Funkcje_Zdrowotne + Funkcje_Sportowe + Funkcje_Smart
Komfort =~ Wygoda_Noszenia + Jakosc_Wykonania
Odpornosc =~ Wodoodpornosc + Wytrzymalosc
"

macierz <- prepare_mcdm_data(
  dane = mcda_dane_surowe,
  skladnia = skladnia,
  kolumna_alternatyw = "Alternatywa"
)

# 3. Oblicz ranking metodą Fuzzy VIKOR z wagami BWM
wynik_vikor <- fuzzy_vikor(
  macierz,
  typy_kryteriow = c("max", "max", "max", "max", "max", "max", "min"),
  bwm_najlepsze = c(8, 7, 6, 5, 4, 3, 1),
  bwm_najgorsze = c(1, 2, 3, 4, 5, 6, 8)
)

# 4. Wyświetl wynik
print(wynik_vikor$wyniki)

## Wizualizacja
Pakiet oferuje profesjonalne wizualizacje wyników MCDA:
plot(wynik_vikor)
<img src="man/figures/README-visualization-1.png" width="100%" />

## Meta-ranking
Agreguj wyniki z wielu metod, aby uzyskać stabilny ranking konsensusu:
meta <- meta_ranking(
  macierz,
  typy_kryteriow = c("max", "max", "max", "max", "max", "max", "min"),
  bwm_najlepsze = c(8, 7, 6, 5, 4, 3, 1),
  bwm_najgorsze = c(1, 2, 3, 4, 5, 6, 8)
)

head(meta$porownanie[order(meta$porownanie$Meta_Agregacja), ], 3)

## Dokumentacja
Więcej informacji:
Vignette: vignette("fittrackr_mcda", package = "FitTrackR")
Pomoc dla funkcji: ?fuzzy_vikor, ?rozmyty_promethee,
?meta_ranking, ?prepare_mcdm_data

## Autor
Julia Miłoś

## Licencja
GPL-3
