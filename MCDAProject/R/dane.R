#' Przykładowe dane do analizy wielokryterialnej MCDA
#'
#' Zbiór danych zawiera symulowane oceny ekspertów dotyczące
#' urządzeń do monitorowania aktywności fizycznej (smartwatchy/opasek).
#' Dane zostały wygenerowane w oparciu o skale Likerta i stanowią
#' wejście do metod Fuzzy TOPSIS oraz Fuzzy VIKOR.
#'
#' Zbiór przeznaczony do użycia z funkcją `prepare_mcdm_data()`.
#'
#' @format Ramka danych (data frame) z ocenami ekspertów dla 3 alternatyw:
#' \describe{
#'   \item{EkspertID}{Identyfikator eksperta oceniającego urządzenie}
#'   \item{Alternatywa}{Nazwa urządzenia (MiBand, AppleWatch, Garmin)}
#'
#'   \item{Tetno_Dokladnosc}{Dokładność pomiaru tętna (skala Likerta 1–10)}
#'   \item{Kroki_Dokladnosc}{Dokładność zliczania kroków (skala Likerta 1–10)}
#'   \item{Kalorie_Dokladnosc}{Dokładność zliczania spalonych kalorii (skala Likerta 1–10)}
#'
#'   \item{Bateria_Wydajnosc}{Wydajność baterii (skala Likerta 1–7)}
#'
#'   \item{Funkcje_Zdrowotne}{Zakres funkcji zdrowotnych (skala Likerta 1–7)}
#'   \item{Funkcje_Sportowe}{Zakres funkcji sportowych (skala Likerta 1–7)}
#'   \item{Funkcje_Smart}{Funkcje inteligentne (powiadomienia, NFC) (1–7)}
#'
#'   \item{Kompatybilnosc_Android}{Kompatybilność z Androidem (skala 1–5)}
#'   \item{Kompatybilnosc_iOS}{Kompatybilność z iOS (skala 1–5)}
#'
#'   \item{Wygoda_Noszenia}{Komfort noszenia urządzenia (skala Likerta 1–7)}
#'   \item{Jakosc_Wykonania}{Jakość materiałów i wykonania (skala 1–7)}
#'
#'   \item{Wodoodpornosc}{Odporność na wodę (skala Likerta 1–7)}
#'   \item{Wytrzymalosc}{Odporność mechaniczna urządzenia (skala 1–7)}
#'
#'   \item{Cena_PLN}{Cena urządzenia w złotych (kryterium kosztowe)}
#' }
#'
#' @usage data(mcda_dane_surowe)
#'
#' @name mcda_dane_surowe
#'
NULL
