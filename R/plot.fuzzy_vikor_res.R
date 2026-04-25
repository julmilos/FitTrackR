#' Bubble plot for fuzzy VIKOR results
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

  ggplot(df, aes(
    x = aggregated_loss,
    y = worst_case_loss,
    size = compromise_index
  )) +
    geom_point(color = "limegreen", alpha = 0.6) +
    geom_text_repel(aes(label = alternative_id), size = 3) +
    scale_size_continuous(range = c(3, 10)) +
    labs(
      title = "Fuzzy VIKOR – Decision Bubble Map",
      x = "Aggregated Loss (group utility)",
      y = "Worst-case Loss (individual regret)",
      size = "Compromise index"
    ) +
    theme_minimal()
}
