# ---------------------------------------------------------------------------
# Moduł wizualizacji pakietu MCDA
# Zawiera ujednolicony motyw oraz dedykowane wykresy dla metod:
# Wagi, TOPSIS, VIKOR, WASPAS, PROMETHEE II.
# ---------------------------------------------------------------------------

#' @title Wewnętrzny motyw graficzny MCDA
#' @description Ujednolicony styl wykresów ggplot2 dla całego pakietu.
#' @import ggplot2
#' @keywords internal
.motyw_mcda <- function() {
  list(
    theme_light(base_size = 12),
    # scale_fill_gradient(low = "#9bbacaff", high = "#237a28ff"), 
    scale_fill_gradientn(colors = c("#9a1919ff", "#a1941fff", "#622487ff")), 
    scale_size_continuous(range = c(3, 12)),
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "grey40", size = 10),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "right",
      axis.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "#f2f2f2"),
      strip.text = element_text(color = "black", face = "bold")
    )
  )
}

# Fix dla ostrzeżeń R CMD check o zmiennych globalnych w ggplot2
utils::globalVariables(c("Def_S", "Def_R", "D_plus", "D_minus", "Wynik", "WSM", "WPM", 
                         "Wydajnosc", "Rozmiar", "OdlegloscWizualna", "Spojnosc", 
                         "Alternatywa", "Phi_Net", "Phi_Plus", "Phi_Minus", "Alt_Nazwa",
                         "Waga_Def", "Kryterium", "L", "U"))


#' Wizualizacja Wag Kryteriów
#' 
#' @description Pokazuje defuzyfikowane wagi kryteriów wraz z rozpiętością rozmytą (L, U).
#' @param wagi_rozmyte Wektor wag (format 3n: L, M, U dla każdego kryterium).
#' @param nazwy_kryteriow Opcjonalny wektor z nazwami kryteriów.
#' @import ggplot2
#' @export
plot_wagi <- function(wagi_rozmyte, nazwy_kryteriow = NULL) {
  
  n_kryt <- length(wagi_rozmyte) / 3
  
  if (is.null(nazwy_kryteriow)) {
    nazwy_kryteriow <- paste0("C", 1:n_kryt)
  }
  
  # Przygotowanie danych (Defuzyfikacja GMIR dla słupka)
  df_wagi <- data.frame(
    Kryterium = factor(nazwy_kryteriow, levels = nazwy_kryteriow),
    L = wagi_rozmyte[seq(1, length(wagi_rozmyte), 3)],
    M = wagi_rozmyte[seq(2, length(wagi_rozmyte), 3)],
    U = wagi_rozmyte[seq(3, length(wagi_rozmyte), 3)]
  )
  
  df_wagi$Waga_Def <- (df_wagi$L + 4*df_wagi$M + df_wagi$U) / 6
  
  ggplot(df_wagi, aes(x = Kryterium, y = Waga_Def)) +
    geom_bar(stat = "identity", fill = "#487f9aff", color = "black", alpha = 0.8, width = 0.7) +
    # Wąsy pokazujące rozpiętość rozmytą (niepewność)
    geom_errorbar(aes(ymin = L, ymax = U), width = 0.2, color = "black") +
    geom_point(aes(y = M), shape = 21, fill = "white", size = 2) +
    coord_flip() +
    .motyw_mcda() +
    theme(panel.grid.major.y = element_blank()) +
    labs(
      title = "Wagi Kryteriów (GMIR)",
      subtitle = "Słupki: waga ostra. Wąsy: rozpiętość liczby rozmytej (L, U). Kropka: M.",
      x = NULL, y = "Znormalizowana Waga"
    )
}


#' Mapa Strategiczna VIKOR
#' @description Wizualizacja typu IPMA. Oś X: Wydajność (S). Oś Y: Ryzyko (R). Wielkość: Kompromis (Q).
#' @param x Obiekt klasy `rozmyty_vikor_wynik`.
#' @param ... Dodatkowe argumenty (ignorowane).
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_vikor_wynik <- function(x, ...) {
  df <- x$wyniki

  # Matematyka wykresu: Normalizacja S i Q dla wizualizacji
  s_min <- min(df$Def_S); s_max <- max(df$Def_S)
  # X: im mniejsze S, tym większa "wydajność"
  df$Wydajnosc <- ((s_max - df$Def_S) / (s_max - s_min + 1e-9)) * 100
  
  # Rozmiar: im mniejsze Q, tym lepszy kompromis (większy bąbel)
  q_norm <- (df$Def_Q - min(df$Def_Q)) / (max(df$Def_Q) - min(df$Def_Q) + 1e-9)
  df$Rozmiar <- (1.1 - q_norm)^3 

  # Środki ćwiartek
  srodek_x <- median(df$Wydajnosc, na.rm=TRUE)
  srodek_y <- median(df$Def_R, na.rm=TRUE)

  ggplot(df, aes(x = Wydajnosc, y = Def_R)) +
    # Tło strefy Lidera
    annotate("rect", xmin=srodek_x, xmax=Inf, ymin=-Inf, ymax=srodek_y, fill="#2E7D32", alpha=0.2) +
    # POPRAWKA BŁĘDU (zamieniono drugie xmax na ymax)
    annotate("rect", xmin=-Inf, xmax=srodek_x, ymin=srodek_y, ymax=Inf, fill="#941212ff", alpha=0.1) +

    geom_vline(xintercept = srodek_x, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = srodek_y, linetype = "dashed", color = "grey50") +

    # Bąble
    geom_point(aes(size = Rozmiar, fill = Wydajnosc), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.5) +

    labs(
      title = "Mapa Strategiczna Fuzzy VIKOR",
      subtitle = "Prawa-dolna ćwiartka: wysoka wydajność, niskie ryzyko żalu.",
      x = "Indeks Wydajności Grupy (odwrócone S, 0-100)",
      y = "Indeks Ryzyka / Żalu (Def_R)",
      size = "Siła Kompromisu (1-Q)",
      fill = "Wydajność"
    ) +
    .motyw_mcda()
}


#' Mapa Efektywności TOPSIS
#' @description Oś X: Dystans od Anty-wzorca (D-). Oś Y: Dystans do Wzorca (D+).
#' @param x Obiekt klasy `rozmyty_topsis_wynik`.
#' @param ... Dodatkowe argumenty.
#' @export
plot.rozmyty_topsis_wynik <- function(x, ...) {
  df <- x$wyniki
  df$Rozmiar <- (df$Wynik)^3 

  # Punkt Idealny wizualny (Target)
  cel_x <- max(df$D_minus) * 1.05
  cel_y <- min(df$D_plus) * 0.95

  ggplot(df, aes(x = D_minus, y = D_plus)) +
    # Strefa celu
    annotate("point", x = cel_x, y = cel_y, shape=18, size=8, color="#929019ff", alpha=0.5) +
    # Linie pomocnicze
    geom_segment(aes(xend = cel_x, yend = cel_y), linetype = "dotted", color = "grey70") +

    # Bąble
    geom_point(aes(size = Rozmiar, fill = Wynik), shape = 21, color = "black", alpha = 0.9) +
    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.6) +

    labs(
      title = "Mapa Efektywności Fuzzy TOPSIS",
      subtitle = "Cel: Prawy-dolny róg (daleko od najgorszego, blisko ideału).",
      x = "Dystans od Anty-Wzorca (D-)",
      y = "Dystans do Wzorca (D+)",
      size = "Wynik CC^3",
      fill = "Wynik (CC)"
    ) +
    .motyw_mcda()
}


#' Mapa Spójności WASPAS
#' @description Porównuje WSM (addytwność) z WPM (multiplikatywność).
#' @param x Obiekt klasy `rozmyty_waspas_wynik`.
#' @param ... Dodatkowe argumenty.
#' @export
plot.rozmyty_waspas_wynik <- function(x, ...) {
  df <- x$wyniki

  # Obliczenie spójności (różnica między modelami)
  df$Odchylenie <- abs(df$WSM - df$WPM)
  df$Spojnosc <- 1 - (df$Odchylenie / (max(df$Odchylenie) + 1e-9))

  ggplot(df, aes(x = WSM, y = WPM)) +
    # Przekątna idealnej spójności
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +

    geom_point(aes(size = Wynik^2, fill = Spojnosc), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.5) +

    labs(
      title = "Mapa Spójności Fuzzy WASPAS",
      subtitle = "Im bliżej przekątnej, tym modele WSM i WPM są bardziej zgodne.",
      x = "Model Addytywny (WSM)",
      y = "Model Multiplikatywny (WPM)",
      size = "Wynik Q^2",
      fill = "Spójność modeli"
    ) +
    .motyw_mcda()
}


#' Mapa Przepływów PROMETHEE II
#' @description Oś X: Przepływ wyjściowy (siła). Oś Y: Przepływ wejściowy (słabość).
#' @param x Obiekt klasy `rozmyty_promethee_wynik`.
#' @param ... Dodatkowe argumenty.
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_promethee_wynik <- function(x, ...) {
  df <- x$wyniki
  
  # Przygotowanie rozmiaru bąbla na podstawie Net Flow (CC)
  # Normalizacja CC do zakresu 0-1 dla rozmiaru
  cc_norm <- (df$Phi_Net - min(df$Phi_Net)) / (max(df$Phi_Net) - min(df$Phi_Net) + 1e-9)
  df$Rozmiar <- (cc_norm + 0.2)^2
  
  # Środki do wyznaczenia ćwiartek
  srodek_x <- median(df$Phi_Plus, na.rm=TRUE)
  srodek_y <- median(df$Phi_Minus, na.rm=TRUE)

  ggplot(df, aes(x = Phi_Plus, y = Phi_Minus)) +
    # Tło dla strefy Lidera (Duża siła, Mała słabość -> Prawy Dolny Róg)
    annotate("rect", xmin=srodek_x, xmax=Inf, ymin=-Inf, ymax=srodek_y, fill="#7d2e63ff", alpha=0.2) +
    
    # Linie podziału
    geom_vline(xintercept = srodek_x, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = srodek_y, linetype = "dashed", color = "grey50") +
    
    # Bąble
    geom_point(aes(size = Rozmiar, fill = Phi_Net), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.5) +
    
    # Etykiety osi
    annotate("text", x = max(df$Phi_Plus), y = min(df$Phi_Minus), label = "LIDER", 
             hjust=1, vjust=0, size=4, fontface="bold", color="darkgreen") +

    labs(
      title = "Mapa Przepływów Fuzzy PROMETHEE II",
      subtitle = "Prawy-dolny róg: duża siła przebicia (Phi+), mała podatność (Phi-).",
      x = "Przepływ Wyjściowy / Siła (Phi+)",
      y = "Przepływ Wejściowy / Słabość (Phi-)",
      size = "Przepływ Netto^2",
      fill = "Phi Netto"
    ) +
    .motyw_mcda()
}