#' @title MCDA Input Validation
#' @description Checks correctness of MCDA inputs
#' @keywords internal
#' @export
validate_mcda_input <- function(decision_matrix, criteria_type) {

  if (!is.matrix(decision_matrix)) {
    stop("decision_matrix must be a matrix")
  }

  n_criteria <- ncol(decision_matrix) / 3

  if (length(criteria_type) != n_criteria) {
    stop("criteria_type length must match number of criteria")
  }

  if (any(is.na(criteria_type))) {
    stop("criteria_type contains NA values")
  }

  if (!all(criteria_type %in% c("max", "min"))) {
    stop("criteria_type must contain only 'max' or 'min'")
  }

  return(invisible(TRUE))
}
