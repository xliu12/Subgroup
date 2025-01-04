
GenData <- function(
    sim.seed = 123,
    n = 500,
    proportion_r1 = 0.5,
    Yfamily = "gaussian",
    deltas = c(1/20, 20),
    quadratic.tt = FALSE,
    quadratic.Y = FALSE,
    # interact = TRUE,
    num_x = 4
) {
  set.seed(seed = sim.seed)
  
  
  X <- rmvnorm(n, sigma = diag(1, nrow = num_x))
  
  R <- rbinom(n, 1, prob = r.gen(X, intercept = qnorm(proportion_r1)))
  
  tt <- rbinom(n, 1, prob = tt.gen(R, X, quadratic = quadratic.tt))
  
  Y <- rnorm(n, mean = y.gen(tt, R, X, quadratic = quadratic.Y), sd = 1)
  
  datobs <- data.frame(R, tt, Y, X=X)
  
  true_val <- map(deltas, ~true.val(., X, r.intercept = qnorm(proportion_r1), Yfamily, quadratic.Y)) %>% do.call(rbind, .)
  out <- mget( ls(), envir = environment() )
  
  return(out)
}



r.gen <- function(X, intercept = 0) {
  latent <- rowSums(sqrt(0.15/ncol(X)) * X) + intercept
  pnorm(latent) # %>% summary()
}
tt.gen <- function(R, X, quadratic = FALSE) {
  if (quadratic) {
    X[, 1:4] <- (X[, 1:4]^2 - 1)/sqrt(2)
    # X <- cbind(X, X[,1]*X[,2])
  }
  latent <- rowSums(sqrt(0.15/ncol(X)) * X) + 0.2*R
  pnorm(latent) # %>% summary()
}

y.gen <- function(tt, R, X, quadratic = FALSE) {
  if (quadratic) {
    X[, 1:4] <- (X[, 1:4]^2 - 1)/sqrt(2)
    # X[, 2:5] <- X[, 1]*X[, 2:5]
    
  }
  latent <- rowSums(sqrt(0.15/ncol(X)) * X) + 0.2*R + 0.2*tt*R + 0.2*tt
  latent
}


# marginalized outcome ----

true.val <- function(delta, X, r.intercept, Yfamily, quadratic.Y = FALSE) {
  
  r1_x <- r.gen(X, intercept = r.intercept)
  r0_x <- 1 - r1_x
  q_r <- list()
  q_r[["qr1"]] <- delta * r1_x / (delta * r1_x + r0_x)
  q_r[["qr0"]] <- 1 - q_r[["qr1"]]
  
  rvals <- c(0, 1)
  
  v_x <- list()
  for (tval in c(1, 0)) {
    if (Yfamily == "gaussian") {
      v_x[[ glue("Y{tval}|Rq_delta{round(delta,2)}") ]] <- lapply(rvals, \(rval) {
        y.gen(tt = tval, R = rval, X, quadratic.Y) * q_r[[glue("qr{rval}")]]
      }) %>%
        Reduce(`+`, .)
    }
    
    
  }
  
  v_x[[glue("ATE|Rq_delta{round(delta,2)}")]] <-
    v_x[[grep("Y1", names(v_x))]] - v_x[[grep("Y0", names(v_x))]]
  
  truth <- map_dbl(v_x, mean)
  
  data.frame(estimand = names(v_x), truev = truth)
}

