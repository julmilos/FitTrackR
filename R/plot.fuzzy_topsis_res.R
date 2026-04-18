#' Bubble plot for fuzzy TOPSIS results
#'
#' @param x Object of class `fuzzy_topsis_res`.
#' @param ... Additional graphical parameters.
#'
#' @importFrom graphics plot text
#' @export
#' @method plot fuzzy_topsis_res
plot.fuzzy_topsis_res <- function(x, ...) {

  df <- x$results

  plot(
    df$distance_to_ideal,
    df$distance_to_anti_ideal,
    cex = sqrt(pmax(df$closeness_coefficient, 0)) + 0.5,
    pch = 21,
    bg = adjustcolor("limegreen", alpha.f = 0.6),
    xlab = "Distance to Ideal Solution",
    ylab = "Distance to Anti-Ideal Solution",
    main = "Fuzzy TOPSIS – Decision Bubble Map",
    ...
  )

  text(
    df$distance_to_ideal,
    df$distance_to_anti_ideal,
    labels = df$alternative_id,
    pos = 3,
    cex = 0.8
  )
}
