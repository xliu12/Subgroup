

bound <- function(vals, tol = 0.01) {
    vals[vals < tol] <- tol
    vals[vals > 1 - tol] <- 1 - tol
    return(vals)
}


bound.ipw <- function(ipw, n) {
    # (Gruber et al., 2022) https://doi.org/10.1093/aje/kwac087
    upper <- sqrt(n)*log(n)/5
    ipw[ipw > upper] <- upper
    ipw
}

shift_data <- function(data, tt, R, t_shift, r_shift) {
    data[[tt]] <- t_shift
    data[[R]] <- r_shift
    update.interactions(data, "tt", "R", "ttR")
}
shift_data_t <- function(data, tt, t_shift) {
    data[[tt]] <- t_shift

    update.interactions(data, "tt", "R", "ttR")
}
# update interactions

#' @exportS3Method

update.interactions <- function(data, tt, R, ttR) {

    data[, ttR] <- data[[tt]] * data[[R]]

    data
}


# is_normalized <- function(x, tolerance = .Machine$double.eps^0.5) {
#   # Check if the mean is approximately 1 within the given tolerance
#   abs(mean(x) - 1) < tolerance
# }

normalize <- function(x) {
    # if (is_normalized(x)) return(x)
    x / mean(x)
}


if.binary <- function(x) {
    if ( all(unique(x) %in% c(0,1)) ) {
        TRUE
    } else {
        FALSE
    }
}
