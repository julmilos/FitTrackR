
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
```r
library(FitTrackR)

# 1. Wczytaj dane
data("mcda_dane_surowe")
head(mcda_dane_surowe)
#> EkspertID Alternatywa Tetno_Dokladnosc Kroki_Dokladnosc Kalorie_Dokladnosc Bateria_Wydajnosc Funkcje_Zdrowotne Funkcje_Sportowe Funkcje_Smart Kompatybilnosc_Android Kompatybilnosc_iOS Wygoda_Noszenia Jakosc_Wykonania Wodoodpornosc Wytrzymalosc Cena_PLN
#> 1 1 Samsung Galaxy Watch 9 6 7 5 5 3 3 3 1 4 7 5 5 152.16
#> 2 1 AppleWatch 8 7 6 4 6 7 4 4 5 7 4 6 6 2245.23
#> 3 1 Garmin 7 9 8 6 5 4 6 2 2 5 5 3 5 920.32
#> 4 2 Samsung Galaxy Watch 6 8 7 5 6 6 3 5 3 5 6 5 4 167.11
#> 5 2 AppleWatch 9 7 6 6 5 7 3 2 4 6 5 4 7 1890.44
#> 6 2 Garmin 8 8 7 4 4 6 4 3 5 6 6 6 4 1600.55

# 2. Przygotuj macierz decyzyjną
skladnia <- "
Dokladnosc =~ Tetno_Dokladnosc + Kroki_Dokladnosc + Kalorie_Dokladnosc
Bateria =~ Bateria_Wydajnosc
Funkcjonalnosc =~ Funkcje_Zdrowotne + Funkcje_Sportowe + Funkcje_Smart
Komfort =~ Wygoda_Noszenia + Jakosc_Wykonania
Odpornosc =~ Wodoodpornosc + Wytrzymalosc
Cena =~ Cena_PLN
"

macierz <- przygotuj_dane_mcda(
  dane = mcda_dane_surowe,
  skladnia = skladnia,
  kolumna_alternatyw = "Alternatywa"
)
macierz

#> Alternatywa Dokladnosc Bateria Funkcjonalnosc Komfort Odpornosc Cena
#> 1 Samsung Galaxy Watch 7.5 5 4.5 5 5 160
#> 2 AppleWatch 8.0 5 5.0 5 5 2000
#> 3 Garmin 7.5 5 4.5 5 5 1200

# 3. Oblicz ranking metodą Fuzzy VIKOR z wagami BWM
wynik_vikor <- fuzzy_vikor(
  macierz,
  typy_kryteriow = c("max", "max", "max", "max", "max", "max", "min"),
  bwm_najlepsze = c(8, 7, 6, 5, 4, 3, 1),
  bwm_najgorsze = c(1, 2, 3, 4, 5, 6, 8)
)
#> Alternatywa Def_S Def_R Def_Q Ranking
#> 1 Samsung Galaxy Watch 0.14515360 0.1599259 0.3743246 2
#> 2 AppleWatch 0.13007834 0.1900849 0.3903515 3
#> 3 Garmin 0.08210813 0.1421481 0.3421120 1

# 4. Wyświetl wynik
print(wynik_vikor$wyniki)
```
## Wizualizacja
Pakiet oferuje profesjonalne wizualizacje wyników MCDA:
```r
plot(wynik_vikor)
```
## Meta-ranking
Agreguj wyniki z wielu metod, aby uzyskać stabilny ranking konsensusu:
```r
meta <- meta_ranking(
  macierz,
  typy_kryteriow = c("max", "max", "max", "max", "max", "max", "min"),
  bwm_najlepsze = c(8, 7, 6, 5, 4, 3, 1),
  bwm_najgorsze = c(1, 2, 3, 4, 5, 6, 8)
)

head(meta$porownanie[order(meta$porownanie$Meta_Agregacja), ], 3)

#> Alternatywa Meta_Agregacja
#> 1 Garmin 0.342
#> 2 Samsung Galaxy Watch 0.374
#> 3 AppleWatch 0.390
```
## Dokumentacja
Więcej informacji:
Vignette: vignette("fittrackr_mcda", package = "FitTrackR")
Pomoc dla funkcji: ?fuzzy_vikor, ?rozmyty_promethee,
?meta_ranking, ?przygotuj_dane_mcda

## Autor
Julia Miłoś

## Licencja
GPL-3
