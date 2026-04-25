#' Bubble plot for fuzzy TOPSIS results
#'
#' @param x Object of class `fuzzy_topsis_res`.
#' @param ... Additional graphical parameters.
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @export
#' @method plot fuzzy_topsis_res
plot.fuzzy_topsis_res <- function(x, ...) {

  df <- x$results

  ggplot(df, aes(
    x = distance_to_ideal,
    y = distance_to_anti_ideal,
    size = closeness_coefficient
  )) +
    geom_point(color = "limegreen", alpha = 0.6) +
    geom_text_repel(aes(label = alternative_id), size = 3) +
    scale_size_continuous(range = c(3, 10)) +
    labs(
      title = "Fuzzy TOPSIS – Decision Bubble Map",
      x = "Distance to Ideal Solution",
      y = "Distance to Anti-Ideal Solution",
      size = "Closeness coefficient"
    ) +
    theme_minimal()
}
