#' @title Generowanie Tabeli APA
#' @export
tabela_apa <- function(x, tytul = NULL) { UseMethod("tabela_apa") }

#' @export
tabela_apa.data.frame <- function(x, tytul = "Tabela Wyników MCDA") {
  rempsyc::nice_table(x, title = c("Tabela", tytul))
}

#' @export
tabela_apa.rozmyty_topsis_wynik <- function(x, tytul = "Wyniki metody Fuzzy TOPSIS") {
  df <- x$wyniki
  names(df) <- c("Alternatywa", "D+ (Wzorzec)", "D- (Anty-wzorzec)", "Wynik (CC)", "Ranking")
  df$`D+ (Wzorzec)` <- round(df$`D+ (Wzorzec)`, 4)
  df$`D- (Anty-wzorzec)` <- round(df$`D- (Anty-wzorzec)`, 4)
  df$`Wynik (CC)` <- round(df$`Wynik (CC)`, 4)
  rempsyc::nice_table(df, title = c("Tabela", tytul), note = c("Uwaga. CC - Coefficient of Closeness."))
}

#' @export
tabela_apa.rozmyty_vikor_wynik <- function(x, tytul = "Wyniki metody Fuzzy VIKOR") {
  # Wybieramy tylko unikalne kolumny, ignorujemy zduplikowany "Wynik"
  df <- x$wyniki[, c("Alternatywa", "Def_S", "Def_R", "Def_Q", "Ranking")]
  names(df) <- c("Alternatywa", "S (Wydajnosc)", "R (Zal)", "Q (Kompromis)", "Ranking")
  df$`S (Wydajnosc)` <- round(df$`S (Wydajnosc)`, 4)
  df$`R (Zal)` <- round(df$`R (Zal)`, 4)
  df$`Q (Kompromis)` <- round(df$`Q (Kompromis)`, 4)
  rempsyc::nice_table(df, title = c("Tabela", tytul), note = c("Uwaga. Q: indeks kompromisu."))
}

#' @export
tabela_apa.rozmyty_waspas_wynik <- function(x, tytul = "Wyniki metody Fuzzy WASPAS") {
  df <- x$wyniki
  names(df) <- c("Alternatywa", "WSM", "WPM", "Wynik (Q)", "Ranking")
  df$WSM <- round(df$WSM, 4); df$WPM <- round(df$WPM, 4); df$`Wynik (Q)` <- round(df$`Wynik (Q)`, 4)
  rempsyc::nice_table(df, title = c("Tabela", tytul), note = c("Uwaga. Wynik Q to połączenie modeli WSM i WPM."))
}

#' @export
tabela_apa.rozmyty_promethee_wynik <- function(x, tytul = "Wyniki metody Fuzzy PROMETHEE II") {
  # Wybieramy tylko konkretne kolumny
  df <- x$wyniki[, c("Alternatywa", "Phi_Plus", "Phi_Minus", "Phi_Net", "Ranking")]
  names(df) <- c("Alternatywa", "Phi+", "Phi-", "Phi Netto", "Ranking")
  df$`Phi+` <- round(df$`Phi+`, 4); df$`Phi-` <- round(df$`Phi-`, 4); df$`Phi Netto` <- round(df$`Phi Netto`, 4)
  rempsyc::nice_table(df, title = c("Tabela", tytul), note = c("Uwaga. Phi: przepływy siły i słabości."))
}

#' @export
tabela_apa.list <- function(x, tytul = "Zestawienie Meta-Rankingu") {
  if(is.null(x$wyniki)) stop("To nie jest poprawny obiekt meta-rankingu.")
  df <- x$wyniki
  names(df) <- gsub("_", " ", names(df))
  rempsyc::nice_table(df, title = c("Tabela", tytul), note = c("Uwaga. Meta Ranking oparty na Teorii Dominacji."))
}