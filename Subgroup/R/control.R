#' Specify control parameters
#'
#' @param crossfit_folds [\code{numeric(1)}]\cr The number of crossfit folds.
#' @param mlr3superlearner_folds [\code{numeric(1)}]\cr The number of `mlr3superlearner` folds.
#'
#' @return A list of control parameters
#' @export
#'
#' @examples
#' analysis_control(crossfit_folds = 5)
analysis_control <- function(crossfit_folds = 5L,
                            mlr3superlearner_folds = 10L,
                            mlr3superlearner_discrete = FALSE) {
  checkmate::assert_number(crossfit_folds)
  checkmate::assert_number(mlr3superlearner_folds)
  checkmate::assert_logical(mlr3superlearner_discrete)

  list(
    crossfit_folds = crossfit_folds,
    mlr3superlearner_folds = mlr3superlearner_folds,
    mlr3superlearner_discrete = mlr3superlearner_discrete
  )
}
