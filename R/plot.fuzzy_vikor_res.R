#' Bubble plot dla wyników metody Fuzzy VIKOR
#'
#' @param x Object of class `fuzzy_vikor_res`.
#' @param ... Additional graphical parameters.
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @export
#' @method plot fuzzy_vikor_res
plot.fuzzy_vikor_res <- function(x, ...) {
  df <- x$results
  aggregated_loss <- worst_case_loss <- compromise_index <- alternative_id <- NULL
  if (!is.null(rownames(x$results))) {
    df$label <- rownames(x$results)
  } else {
    df$label <- as.character(df$alternative_id)
  }
  ggplot(df, aes(
    x = aggregated_loss,
    y = worst_case_loss,
    size = compromise_index
  )) +
    geom_point(color = "limegreen", alpha = 0.6) +
    geom_text_repel(aes(label = alternative_id), size = 3) +
    scale_size_continuous(range = c(3, 10)) +
    labs(
      title = "Fuzzy VIKOR – Mapa decyzyjna – Wykres bąbelkowy",
      x = "Użyteczność grupowa",
      y = "Indywidualny żal",
      size = "Wskaźnik kompromisowy"
    ) +
    theme_minimal()
}
