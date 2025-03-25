library(broom)

incremental.reg <- function(
        data,
        subgroup = "R",
        treatment = "tt",
        outcome = "Y",
        covariates = NULL,
        deltas = c(1.5, 2, 5),
        # learners,
        control = analysis_control(crossfit_folds = 5)
) {
    df_lm <- data.frame(Y = data[[outcome]], tt = data[[treatment]], 
                        R = data[[subgroup]], covariates = data[, covariates])
    reg <- lm(Y ~ tt + R + tt:R + ., df_lm) %>% broom::tidy()
    
    hat.b_t <- reg[2, 2] %>% as.numeric()
    # hat.b_r <- reg[3, 2] %>% as.numeric()
    hat.b_tr <- reg[4, 2] %>% as.numeric()
    
    hat.p_r1 <- mean(df_lm$R) 
    
    reg_est <- map(deltas, ~ATE.delta.val(., hat.b_t, hat.b_tr, hat.p_r1))
    
    res_reg <- list()
    
    res_reg[["ATE_deltas"]] <- reg_est %>%
        Reduce(rbind, .) %>%
        mutate(delta = deltas) %>%
        select(delta, everything())
    
    res_reg[["reg_coefs"]] <- data.frame(estimand = c("ATE_focal", "Interaction"),
                                         estimate = c(hat.b_t + hat.b_tr, hat.b_tr))
    
    return(res_reg)
}




ATE.delta.val <- function(delta, b_t, b_tr, proportion_r1) {
    
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
    
    est <- unlist(v_x)
    
    data.frame(estimand = names(v_x), estimate = est)
}

