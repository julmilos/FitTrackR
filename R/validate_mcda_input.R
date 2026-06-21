#' @title Walidacja danych wejściowych
#' @description Sprawdza poprawność danych wejściowych
#' @keywords internal
#' @export
validate_mcda_input <- function(decision_matrix, criteria_type) {

  if (!is.matrix(decision_matrix)) {
    stop("decision_matrix musi byc macierza")
  }

  n_criteria <- ncol(decision_matrix) / 3

  if (length(criteria_type) != n_criteria) {
    stop("Dlogosc criteria_type musi odpowiadac ilosci kryteriow")
  }

  if (any(is.na(criteria_type))) {
    stop("criteria_type contains NA values")
  }

  if (!all(criteria_type %in% c("max", "min"))) {
    stop("criteria_type musi zawierac jedynie 'max' lub 'min'")
  }

  return(invisible(TRUE))
}
