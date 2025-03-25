
library(mvtnorm)
library(parallel)
library(tidyverse)
library(glue)

devtools::load_all("Subgroup")


GenData <- function(
        sim.seed = 123,
        n = 500,
        proportion_r1 = 0.1,
        # b_t = 0.2, b_r = 0.2, b_tr = 0.2, 
        R2_t = 0.06, R2_r = 0.06, R2_tr = 0.06,
        deltas = c(1/20, 20),
        Yfamily = "gaussian",
        proportion_t = 0.5
) {
    set.seed(seed = sim.seed)
    
    # num_x <- 5
    
    R <- rbinom(n, 1, prob = proportion_r1)
    
    tt <- c(rep(1, each = round(n * proportion_t)), rep(0, each = round(n * (1 - proportion_t))))
    proportion_tr1 <- proportion_t * proportion_r1
    
    b_t <- sqrt((R2_t / (1-R2_t)) / (proportion_t * (1-proportion_t)))
    b_r <- sqrt((R2_r / (1-R2_r)) / (proportion_r1 * (1-proportion_r1)))
    b_tr <- sqrt((R2_tr / (1-R2_tr)) / (proportion_tr1 * (1-proportion_tr1)))
    
    # var_ey <- 1 - (b_t^2 * (proportion_t * (1-proportion_t)) + b_t^2 * (proportion_r1 * (1-proportion_r1)) + b_tr^2 * (proportion_tr1 * (1-proportion_tr1)))
    
    Y <- b_t * tt + b_t * R + b_tr * tt * R + rnorm(n)
    
    datobs <- data.frame(R, tt, Y)
    
    true_val <- map(deltas, ~true.val(., b_t, b_tr, proportion_r1)) %>% do.call(rbind, .)
    out <- mget( ls(), envir = environment() )
    
    return(out)
}



true.val <- function(delta, b_t, b_tr, proportion_r1) {
    
    r1_x <- proportion_r1
    r0_x <- 1 - r1_x
    q_r <- list()
    if (delta == Inf) {
        q_r[["qr1"]] <- 1
    }
    if (delta != Inf) {
        q_r[["qr1"]] <- delta * r1_x / (delta * r1_x + r0_x)
    }
    q_r[["qr0"]] <- 1 - q_r[["qr1"]]
    

    v_x <- list()
    
    v_x[[glue("ATE|Rq_delta{round(delta,2)}")]] <- b_t + b_tr * q_r[["qr1"]]
    
    truth <- map_dbl(v_x, mean)
    
    data.frame(estimand = names(v_x), truev = truth)
}

# Simulation----------------------------

condition_all <- data.frame(expand.grid(
    n = c(300, 500, 1000),
    proportion_r1 = c(0.1, 0.5),
    R2_t = c(0, 0.01, 0.06, 0.14),
    R2_r = c(0, 0.01, 0.06, 0.14),
    R2_tr = c(0, 0.01, 0.06, 0.14),
    Yfamily = c("gaussian")
))

condition <- condition_all %>% 
    filter(R2_t == 0.06, R2_r == 0.06, R2_tr == 0.06, n == 500, 
           proportion_r1 == 0.1)

methds_all <- data.frame(expand.grid(Fit = c("lm")))
methds <- methds_all

set.seed(12)
datseeds <- c(sample(1:1e6, 1000), sample(1:1e6, 1000))

iseed <-1
cond <- 1



OneData <- function(iseed = 1, cond = 1){
    
    deltas <- c(1/20, 1/10, 1/5, 1, 5, 10, 20)
    
    gen_data <- GenData(
        sim.seed = datseeds[iseed],
        n = condition$n[cond],
        proportion_r1 = condition$proportion_r1[cond],
        R2_t = condition$R2_t[cond], 
        R2_r = condition$R2_r[cond], 
        R2_tr = condition$R2_tr[cond],
        deltas = deltas,
        Yfamily = "gaussian",
        proportion_t = 0.5
    )
    true_values <- gen_data$true_val
    
    data <- gen_data$datobs

    # data[, covariates] <- data[, covariates]^2
    # estimators -----
    j <- 1
    one.method <- function(j) {
        
        out <- incremental.reg(
            data = data,
            subgroup = "R",
            treatment = "tt",
            outcome = "Y",
            covariates = NULL,
            deltas = c(1/20, 1/10, 1/5, 1, 5, 10, 20)
        )
        
        
        
        estimates <- out$ATE_deltas %>%
            full_join(true_values, by = "estimand")
        
        est_regcoefs <- out$reg_coefs %>% 
            mutate(truev = case_when(estimand == "ATE_focal" ~ gen_data$b_t + gen_data$b_tr, 
                                     estimand == "Interaction" ~ gen_data$b_tr),
                   delta = c("inf", "na"))
        
        rbind(estimates, est_regcoefs)
        
        estimates1 <- data.frame(methds[j, , drop=F],
                                 rbind(estimates, est_regcoefs),
                                 row.names = NULL)
        
        estimates1
    }
    
    est_allmethds <- lapply(1:nrow(methds), one.method)
    est_allmethds1 <- do.call(rbind, est_allmethds)
    
    res <- data.frame(condition[cond, ], 
                      est_allmethds1,
                      row.names = NULL)
    
    
    # res$estimate - res$truev
    
    return(res)
    
}


