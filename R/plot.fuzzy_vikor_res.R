#' Bubble plot for fuzzy VIKOR results
#'
#' @param x Object of class `fuzzy_vikor_res`.
#' @param ... Additional graphical parameters.
#'
#' @importFrom graphics plot text
#' @export
#' @method plot fuzzy_vikor_res
plot.fuzzy_vikor_res <- function(x, ...) {

  df <- x$results

  plot(
    df$aggregated_loss,
    df$worst_case_loss,
    cex = sqrt(pmax(df$compromise_index, 0)) + 0.5,
    pch = 21,
    bg = "lightblue",
    xlab = "Aggregated Loss (group utility)",
    ylab = "Worst-case Loss (individual regret)",
    main = "Fuzzy VIKOR – Decision Bubble Map",
    ...
  )

  text(
    df$aggregated_loss,
    df$worst_case_loss,
    labels = df$alternative_id,
    pos = 3,
    cex = 0.8
  )
}
