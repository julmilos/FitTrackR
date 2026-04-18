#' @title Wewnętrzny motyw graficzny
#' @description Ujednolicony styl wykresów dla całego pakietu.
#' @import ggplot2
#' @keywords internal
.motyw_mcda <- function() {
  list(
    theme_light(base_size = 12),
    scale_fill_gradient(low = "#C8E6C9", high = "#2E7D32"),
    scale_size_continuous(range = c(4, 16)),
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "right",
      axis.title = element_text(face = "bold")
    )
  )
}

#' Mapa Strategiczna VIKOR
#' @param x Obiekt klasy `rozmyty_vikor_wynik`.
#' @param ... Dodatkowe argumenty.
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_vikor_wynik <- function(x, ...) {

  df <- x$wyniki

  s_min <- min(df$Def_S); s_max <- max(df$Def_S)

  df$Wydajnosc <- ((s_max - df$Def_S) / (s_max - s_min)) * 100

  q_inv <- 1 - ((df$Def_Q - min(df$Def_Q)) / (max(df$Def_Q) - min(df$Def_Q)))
  df$Rozmiar <- (q_inv + 0.1)^3

  srodek_perf <- median(df$Wydajnosc, na.rm=TRUE)
  srodek_ryzyko <- median(df$Def_R, na.rm=TRUE)

  ggplot(df, aes(x = Wydajnosc, y = Def_R)) +
    annotate("rect", xmin=srodek_perf, xmax=Inf, ymin=-Inf, ymax=srodek_ryzyko,
             fill="#E8F5E9", alpha=0.5) +
    geom_vline(xintercept = srodek_perf, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = srodek_ryzyko, linetype = "dashed", color = "grey50") +
    annotate("text", x = max(df$Wydajnosc), y = min(df$Def_R),
             label = "STABILNY LIDER", hjust=1, vjust=0,
             size=3, fontface="bold", color="#1B5E20") +
    annotate("text", x = min(df$Wydajnosc), y = max(df$Def_R),
             label = "UNIKAĆ", hjust=0, vjust=1,
             size=3, color="#2E7D32") +
    geom_point(aes(size = Rozmiar, fill = Wydajnosc),
               shape = 21, color = "#1B5E20", alpha = 0.8) +
    geom_text_repel(aes(label = paste0("Alt ", Alternatywa))) +
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    labs(
      title = "Mapa Strategiczna VIKOR",
      x = "Efektywność grupowa",
      y = "Ryzyko (R)",
      size = "Znaczenie",
      fill = "Wynik"
    ) +
    .motyw_mcda()
}

#' Mapa Efektywności TOPSIS
#' @param x Obiekt klasy `rozmyty_topsis_wynik`.
#' @param ... Dodatkowe argumenty.
#' @export
plot.rozmyty_topsis_wynik <- function(x, ...) {

  df <- x$wyniki
  df$Rozmiar <- (df$Wynik)^4

  cel_x <- max(df$D_minus) * 1.02
  cel_y <- min(df$D_plus) * 0.98

  df$OdlegloscWizualna <- sqrt((df$D_minus - cel_x)^2 + (df$D_plus - cel_y)^2)

  ggplot(df, aes(x = D_minus, y = D_plus)) +
    geom_segment(aes(xend = cel_x, yend = cel_y),
                 linetype = "dotted", color = "grey50") +
    geom_label(aes(x = (D_minus + cel_x) / 2,
                   y = (D_plus + cel_y) / 2,
                   label = sprintf("%.3f", OdlegloscWizualna)),
               size = 2.5, color = "grey30", label.size = 0, alpha = 0.7) +
    geom_point(aes(size = Rozmiar, fill = Wynik),
               shape = 21, color = "#1B5E20", alpha = 0.9) +
    geom_text_repel(aes(label = paste0("Alt ", Alternatywa))) +
    annotate("point", x = cel_x, y = cel_y,
             shape=18, size=6, color="#66BB6A") +
    annotate("text", x = cel_x, y = cel_y,
             label="IDEAŁ", vjust=2, size=3.5, fontface="bold") +
    labs(
      title = "Mapa Efektywności TOPSIS",
      x = "Odległość od anty-wzorca",
      y = "Odległość do wzorca",
      size = "Bliskość",
      fill = "Wynik"
    ) +
    .motyw_mcda()
}

utils::globalVariables(c(
  "Def_S","Def_R","D_plus","D_minus","Wynik",
  "Wydajnosc","Rozmiar","OdlegloscWizualna","Alternatywa"
))
