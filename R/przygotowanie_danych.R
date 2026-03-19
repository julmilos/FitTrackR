#' @title Wewnętrzny parser składni
#' @description Funkcja pomocnicza do interpretowania modelu (stringa).
#' Obsługuje podział linii za pomocą średników (;) lub znaków nowej linii (\n).
#' @keywords internal
.parsuj_skladnie_mcda <- function(skladnia) {
  # Usuwamy znaki powrotu karetki (typowe dla Windows) i dzielimy po \n lub ;
  czysta_skladnia <- gsub("\r", "", skladnia)
  linie <- unlist(strsplit(czysta_skladnia, "[\n;]+"))
  
  mapowanie <- list()

  for (linia in linie) {
    if (trimws(linia) == "") next # Pomin puste linie
    
    # Dzielimy wg operatora "=~"
    czesci <- strsplit(linia, "=~")[[1]]
    if (length(czesci) == 2) {
      nazwa_kryterium <- trimws(czesci[1])
      # Dzielimy zmienne skladowe wg "+"
      elementy <- trimws(strsplit(czesci[2], "\\+")[[1]])
      mapowanie[[nazwa_kryterium]] <- elementy
    }
  }
  
  if (length(mapowanie) == 0) {
    stop("Składnia jest pusta lub niepoprawna. Użyj formatu: 'Kryterium =~ zm1 + zm2'.")
  }
  
  return(mapowanie)
}

#' @title Wewnętrzny Skaler Saaty'ego
#' @description Przekształca dowolną skalę (np. Likert 1-5) na skalę Saaty'ego 1-9.
#' @keywords internal
.skaluj_do_saaty <- function(wektor) {
  # Wymuszenie typu numerycznego
  wektor <- as.numeric(wektor)
  
  if (any(wektor < 0, na.rm = TRUE)) stop("Wykryto wartości ujemne w danych wejściowych.")

  # Obsluga kodow bledow (np. 99) i brakow danych (NA) -> zamiana na 0
  wektor[is.na(wektor) | wektor == 99] <- 0

  maska_poprawne <- wektor > 0
  wartosci <- wektor[maska_poprawne]

  if (length(wartosci) == 0) return(wektor)

  min_v <- min(wartosci)
  max_v <- max(wartosci)

  # Skalowanie liniowe do przedzialu [1, 9]
  if (min_v == max_v) {
    wektor[maska_poprawne] <- 1
  } else {
    # Wzor: 1 + (x - min) * (8 / (max - min))
    wektor[maska_poprawne] <- 1 + (wartosci - min_v) * (8 / (max_v - min_v))
  }
  return(wektor)
}

#' @title Wewnętrzna funkcja rozmywająca (Fuzzifier)
#' @description Zamienia liczbę rzeczywistą na Trójkątną Liczbę Rozmytą (TFN).
#' @keywords internal
.rozmyj_wektor <- function(wektor) {
  # Dolna granica (lower), min to 1
  l <- pmax(1, wektor - 1)
  # Srodek (middle)
  m <- wektor
  # Gorna granica (upper), max to 9
  u <- pmin(9, wektor + 1)

  # Obsluga zer (brakow danych) - pozostają zerami
  jest_zerem <- (wektor == 0)
  l[jest_zerem] <- 0; m[jest_zerem] <- 0; u[jest_zerem] <- 0

  return(cbind(l, m, u))
}

#' Przygotowanie Danych do Rozmytej Analizy MCDA
#'
#' @description Funkcja przekształca surowe dane w rozmytą macierz decyzyjną.
#' Skaluje zmienne do przedziału 1-9, agreguje opinie i dokonuje rozmycia.
#'
#' @param dane Ramka danych (data frame) zawierająca surowe zmienne.
#' @param skladnia Ciąg znaków definiujący kryteria (np. "Koszt =~ k1 + k2").
#' @param kolumna_alternatyw Nazwa kolumny identyfikującej alternatywy.
#' @param funkcja_agregacji Funkcja używana do scalania opinii (domyślnie: mean).
#' @return Macierz o wymiarach ($m \times 3n$).
#' @export
przygotuj_dane_mcda <- function(dane, skladnia, kolumna_alternatyw = NULL, funkcja_agregacji = mean) {

  if (!is.data.frame(dane)) stop("Argument 'dane' musi być ramką danych (data frame).")

  mapowanie <- .parsuj_skladnie_mcda(skladnia)
  nazwy_kryteriow <- names(mapowanie)

  # Zabezpieczenie danych wejściowych
  tymczasowe_wyniki <- data.frame(row_id = 1:nrow(dane))

  for (kryt in nazwy_kryteriow) {
    zmienne <- mapowanie[[kryt]]
    
    brakujace <- zmienne[!zmienne %in% names(dane)]
    if (length(brakujace) > 0) {
      stop(paste("Brakuje zmiennych w danych dla kryterium", kryt, ":", paste(brakujace, collapse=", ")))
    }

    # Wymuszenie konwersji na numeryczne dla bezpieczeństwa
    dane_numeryczne <- as.data.frame(sapply(dane[, zmienne, drop = FALSE], as.numeric))

    if (length(zmienne) > 1) {
      surowy_wynik <- rowMeans(dane_numeryczne, na.rm = TRUE)
    } else {
      surowy_wynik <- dane_numeryczne[[1]]
    }

    tymczasowe_wyniki[[kryt]] <- .skaluj_do_saaty(surowy_wynik)
  }

  if (!is.null(kolumna_alternatyw)) {
    if (!kolumna_alternatyw %in% names(dane)) stop("Nie znaleziono kolumny alternatyw w danych.")

    tymczasowe_wyniki$ID_Alternatywy <- dane[[kolumna_alternatyw]]
    # Agregacja
    dane_zagregowane <- aggregate(. ~ ID_Alternatywy, data = tymczasowe_wyniki[, -1], FUN = funkcja_agregacji)
    
    dane_zagregowane <- dane_zagregowane[order(dane_zagregowane$ID_Alternatywy), ]
    nazwy_wierszy <- dane_zagregowane$ID_Alternatywy
    macierz_wynikow <- as.matrix(dane_zagregowane[, nazwy_kryteriow])

  } else {
    macierz_wynikow <- as.matrix(tymczasowe_wyniki[, nazwy_kryteriow])
    nazwy_wierszy <- 1:nrow(macierz_wynikow)
  }

  lista_decyzyjna <- list()
  for (i in seq_along(nazwy_kryteriow)) {
    lista_decyzyjna[[nazwy_kryteriow[i]]] <- .rozmyj_wektor(macierz_wynikow[, i])
  }

  finalna_macierz <- do.call(cbind, lista_decyzyjna)
  rownames(finalna_macierz) <- nazwy_wierszy
  attr(finalna_macierz, "nazwy_kryteriow") <- nazwy_kryteriow

  return(finalna_macierz)
}