#' Bubble plot for fuzzy PROMETHEE results
#'
#' @param x Object of class `fuzzy_promethee_res`.
#' @param ... Additional graphical parameters.
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @export
#' @method plot fuzzy_promethee_res
plot.fuzzy_promethee_res <- function(x, ...) {
  df <- x$results
  phi_plus <- phi_minus <- phi_net <- alternative_id <- NULL
  if (!is.null(rownames(x$results))) {
    df$label <- rownames(x$results)
  } else {
    df$label <- as.character(df$alternative_id)
  }
  ggplot(df, aes(
    x = phi_plus,
    y = phi_minus,
    size = phi_net
  )) +
    geom_point(color = "limegreen", alpha = 0.6) +
    geom_text_repel(aes(label = alternative_id), size = 3) +
    scale_size_continuous(range = c(3, 10)) +
    labs(
      title = "Fuzzy PROMETHEE – Decision Bubble Map",
      x = "Positive Flow (Phi+)",
      y = "Negative Flow (Phi-)",
      size = "Net flow"
    ) +
    theme_minimal()
}
