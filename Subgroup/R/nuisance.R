
r.x <- function(train, valid, varn, learners, control) {

    valid <- valid[sapply(valid, \(x) ncol(x) > 0)]


    r_fit <- mlr3superlearner::mlr3superlearner(
        data = train$data[, na.omit(c(varn@X, varn@R))],
        target = varn@R,
        library = learners,
        outcome_type = "binomial",
        folds = control$mlr3superlearner_folds,
        newdata = valid[c("data")],
        group = NULL,
        discrete = control$mlr3superlearner_discrete
    )

    r_preds <- r_fit$preds
    setNames(r_preds, "r_x")
}

r.tx <- function(train, valid, varn, learners, control) {

    valid <- valid[sapply(valid, \(x) ncol(x) > 0)]


    r_fit <- mlr3superlearner::mlr3superlearner(
        data = train$data[, na.omit(c(varn@X, varn@tt, varn@R))],
        target = varn@R,
        library = learners,
        outcome_type = "binomial",
        folds = control$mlr3superlearner_folds,
        newdata = valid[c("data", "data_t1", "data_t0")],
        group = NULL,
        discrete = control$mlr3superlearner_discrete
    )

    r_preds <- r_fit$preds
    setNames(r_preds, c("r_t", "r_t1", "r_t0"))
}

#' @exportS3Method


t.rx <- function(train, valid, varn, learners, control) {

    valid <- valid[sapply(valid, \(x) ncol(x) > 0)]


    t_fit <- mlr3superlearner::mlr3superlearner(
        data = train$data[, na.omit(c(varn@X, varn@tt, varn@R))],
        target = varn@tt,
        library = learners,
        outcome_type = "binomial",
        folds = control$mlr3superlearner_folds,
        newdata = valid[c("data")],
        group = NULL,
        discrete = control$mlr3superlearner_discrete
    )
    t_preds <- t_fit$preds
    setNames(t_preds, "t_rx")
}

mu.trx <- function(train, valid, varn, learners, control) {

    valid <- valid[sapply(valid, \(x) ncol(x) > 0)]

    Yfamily <- ifelse(if.binary(train$data[, varn@Y]), "binomial", "continuous")

    mu_fit <- mlr3superlearner::mlr3superlearner(
        data = train$data[, na.omit(c(varn@X, varn@tt, varn@R,varn@ttR, varn@Y))],
        target = varn@Y,
        library = learners,
        outcome_type = Yfamily,
        folds = control$mlr3superlearner_folds,
        newdata = valid,
        group = NULL,
        discrete = control$mlr3superlearner_discrete
    )

    mu_preds <- mu_fit$preds
    names(mu_preds) <- gsub("data", "mu", names(valid))
    mu_preds
}


estimate_mu <- function(an, folds, learners, control) {
    mu_list <- vector("list", control$crossfit_folds)

    i <- 1

    for (i in seq_along(mu_list)) {
        train <- training(an, folds, i)
        valid <- validation(an, folds, i)
        mu_list[[i]] <- mu.trx(train, valid, an@varn, learners, control)

    }

    combine_folds(mu_list, folds)
}

estimate_r <- function(an, folds, learners, control) {
    r_list <- vector("list", control$crossfit_folds)

    i <- 1

    for (i in seq_along(r_list)) {
        train <- training(an, folds, i)
        valid <- validation(an, folds, i)
        r_list[[i]] <- r.x(train, valid, an@varn, learners, control)

    }

    combine_folds(r_list, folds)[["r_x"]]
}

estimate_r.tx <- function(an, folds, learners, control) {
    r_list <- vector("list", control$crossfit_folds)

    i <- 1

    for (i in seq_along(r_list)) {
        train <- training(an, folds, i)
        valid <- validation(an, folds, i)
        r_list[[i]] <- r.tx(train, valid, an@varn, learners, control)

    }

    combine_folds(r_list, folds)
}

estimate_t <- function(an, folds, learners, control) {
    t_list <- vector("list", control$crossfit_folds)

    i <- 1

    for (i in seq_along(t_list)) {
        train <- training(an, folds, i)
        valid <- validation(an, folds, i)
        t_list[[i]] <- t.rx(train, valid, an@varn, learners, control)

    }

    combine_folds(t_list, folds)[["t_rx"]]
}
