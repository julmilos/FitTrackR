#' Bubble plot for fuzzy PROMETHEE results
#'
#' @param x Object of class `fuzzy_promethee_res`.
#' @param ... Additional graphical parameters.
#'
#' @importFrom graphics plot text
#' @export
#' @method plot fuzzy_promethee_res
#' @importFrom grDevices adjustcolor
plot.fuzzy_promethee_res <- function(x, ...) {

  df <- x$results

  plot(
    df$phi_plus,
    df$phi_minus,
    cex = sqrt(pmax(df$phi_net, 0)) + 0.5,
    pch = 21,
    bg = adjustcolor("limegreen", alpha.f = 0.6),
    xlab = "Positive Flow (Phi+)",
    ylab = "Negative Flow (Phi-)",
    main = "Fuzzy PROMETHEE – Decision Bubble Map",
    ...
  )

  text(
    df$phi_plus,
    df$phi_minus,
    labels = df$alternative_id,
    pos = 3,
    cex = 0.8
  )
}
