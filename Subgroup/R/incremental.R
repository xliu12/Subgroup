#' Subgroup analysis based on incremental inclusion
#'
#'
#' @param data [\code{data.frame}]\cr
#'  A \code{data.frame} containing all necessary variables
#'  for the analysis.
#' @param treatment [\code{character(1)}]\cr
#'  The column name of the treatment variable.
#' @param subgroup [\code{character(1)}]\cr
#'  The column name of the subgroup status variable.
#' @param outcome [\code{character(1)}]\cr
#'  The column name of the outcome variable.
#' @param covariates [\code{character}]\cr
#'  A character vector containing the column names of baseline covariates to be included.
#' @param deltas [\code{numeric}]\cr
#'  A sequence of positive numbers specifying the increments to the odds of the focal subgroup.
#' @param learners [\code{character}]\cr
#'  A vector of \code{mlr3superlearner} algorithms
#'  for estimation of the outcome regressions. Default is \code{"glm"}, a main effects GLM.
#' @param control [\code{analysis_control}]\cr
#'  Control parameters for the estimation procedure. Use \code{analysis_control()} to set these values.
#'
#' @return A \code{list} object containing a \code{data.frame} named "ATE_deltas" with the following columns:
#'
#' \item{delta}{The increment odds ratio (delta) values specified by the user, for which the incremental subgroup effects are estimated.}
#' \item{estimand}{The incremental subgroup effect "ATE|Rq_delta{delta_value}" and the potential outcomes  "Y1|Rq_delta{delta_value}" and "Y0|Rq_delta{delta_value}", where "delta_value" is replaced with each of the user-specified delta values (rounded to 2 decimal places).}
#' \item{estimate}{The point estimate.}
#' \item{std.error}{The standard error estimate.}
#' \item{ci.low}{The lower bound of the 0.95 confidence interval.}
#' \item{ci.high}{The upper bound of the 0.95 confidence interval.}
#'
#'
#' @export
#'
#' @examples
#'
#' library(origami)
#' library(tidyverse)
#' library(glue)
#' library(mlr3superlearner)
#' library(mlr3extralearners)
#'
#' data(data)
#' # specify model estimation methods
#' learners <- c("glm", "ranger", "gam")
#'
#' # specify the increment odds ratio delta values
#' deltas <- c(1/10, 1/5, 1, 5, 10)
#'
#' # run
#' out <- Subgroup::incremental.inclusion(
#'   data = data,
#'   subgroup = "R",
#'   treatment = "tt",
#'   outcome = "Y",
#'   covariates = grep("^X", colnames(data), value = TRUE),
#'   deltas = deltas,
#'   learners = learners
#' )
#' out$ATE_deltas
#'



incremental.inclusion <- function(
    data,
    subgroup = "R",
    treatment = "tt",
    outcome = "Y",
    covariates,
    deltas = c(1.5, 2, 5),
    learners,
    control = analysis_control(crossfit_folds = 5)
) {

    set.seed(12345)
  data_in <- data %>%
    rename(tt = all_of(treatment)) %>%
    rename(R = all_of(subgroup)) %>%
    mutate(ttR = tt*R)

  an <- analysis_data(
    data = data_in,
    varn = variables(
      tt = "tt",
      R = "R",
      ttR = "ttR",
      Y = outcome,
      X = covariates
    ),
    tt0 = 0,
    tt1 = 1
  )

  folds <- make.fold_K(an@data, Snames = NULL, #Snames = c(an@varn@tt, an@varn@R), #
                       control$crossfit_folds)

  r_x <- estimate_r(an, folds, learners, control)
  # r_tx <- estimate_r.tx(an, folds, learners, control)
  t_rx <- estimate_t(an, folds, learners, control)
  mu <- estimate_mu(an, folds, learners, control)

  res <- list()
  if (!is.null(deltas)) {
    eif_seq <- lapply(deltas, \(delta) { eif(an, delta, t_rx, r_x, mu) })
    res_seq <- lapply(eif_seq, get.inference) %>%
        Reduce(rbind, .) %>%
        mutate(delta = rep(deltas, each = 3)) %>%
        select(delta, everything())
    res[["ATE_deltas"]] <- res_seq
  }

  # if (!is.null(delta_low) & !is.null(delta_high)) {
  #   eif_low <- eif(an, delta_low, t_rx, r_x, mu)
  #   eif_high <- eif(an, delta_high, t_rx, r_x, mu)
  #   eif_diff <- mapply(`-`, eif_high, eif_low, SIMPLIFY = FALSE)
  #   names(eif_diff) <- c("Y1_diff", "Y0_diff", "ATE_diff")
  #   res_diff <- get.inference(eif_list = c(eif_diff, eif_low, eif_high))
  #   res[["ATE_diff"]] <- res_seq
  # }

  res
}
