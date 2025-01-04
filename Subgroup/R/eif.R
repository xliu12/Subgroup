
eif <- function(an, delta, t_rx, r_x, mu) {

  q_r <- q.r(an, delta, r_x)
  v_x <- v.x(q_r, mu)

  tt <- an@data[[an@varn@tt]]
  R <- an@data[[an@varn@R]]
  Y <- an@data[[an@varn@Y]]

  p_r <- ifelse(R==1, r_x, 1-r_x)
  
  eifs <- list()
  tval <- 1
  for (tval in c(1, 0)) {
    p_t <- ifelse(tval == 1, t_rx, 1-t_rx)
    # p_r.tx <- (R==1)*(r_tx[[glue("r_t{tval}")]]) + (R==0)*(1-r_tx[[glue("r_t{tval}")]])
    
    # v_tx <- v_x[[glue("v_t{tval}")]]

    wt <- 1*(tt == tval) / bound(p_t, tol = 0.01)
    ipw_r1 <- 1*(R == 1) / (p_r)
    ipw_r0 <- 1*(R == 0) / (p_r)
    # ipw_r <- R / bound(p_r)
    wt <- normalize(wt) #%>% bound.ipw(., n = length(tt))
    # # wr <- q_r$qR / (p_r)
    # w <- q_r[["qR"]]*(tt == tval) / (p_r * bound(p_t)) #bound(p_r * p_t) #
    # eif_y <- w*(Y - mu$mu)
    # # # eif_r <- w*(mu$mu - v_tx)
    # eif_r1 <- wt*(R==1)*(q_r[["qR"]]/(p_r))*(mu[[glue("mu_t{tval}")]] - v_x[[glue("v_t{tval}")]])
    # eif_r0 <- wt*(R==0)*(q_r[["qR"]]/(p_r))*(mu[[glue("mu_t{tval}")]] - v_x[[glue("v_t{tval}")]])
    # eif_r <- eif_r1 + eif_r0
    # eif_sum <- eif_y + eif_r + v_x[[glue("v_t{tval}")]]
    
    phi_r1 <- (ipw_r1 * wt) * (Y - mu$mu) + mu[[glue("mu_t{tval}r1")]]
    phi_r0 <- (ipw_r0 * wt) * (Y - mu$mu) + mu[[glue("mu_t{tval}r0")]]
    eif_y <- q_r[["qr1"]] * phi_r1 + q_r[["qr0"]] * phi_r0
    mu_diff <- mu[[glue("mu_t{tval}r1")]] - mu[[glue("mu_t{tval}r0")]]
    eif_r <- q_r[["delta"]] * mu_diff * (R - r_x) / (q_r[["delta"]] * r_x + 1-r_x)^2
    eif_sum <- eif_y + eif_r
    
    # wr <- (q_r[["delta"]] * R + (1-R))/(q_r[["delta"]]*r_x + 1-r_x)
    # y_pseudo <- v_x[[glue("v_t{tval}")]]
    # v1 <- (R*(1-r_x) - (1-R)*q_r[["delta"]]*r_x) / (q_r[["delta"]] / (q_r[["delta"]] -1))
    # phi <- wr*wt* Y + wr*wt *v1* y_pseudo
    # eif_sum <- phi
    
    eifs[[glue("Y{tval}|Rq_delta{round(delta,2)}")]] <- eif_sum

  }

  eifs[[glue("ATE|Rq_delta{round(delta,2)}")]] <- eifs[[grep("Y1", names(eifs))]] - eifs[[grep("Y0", names(eifs))]]

  eifs
}

q.r <- function(an, delta, r_x) {

  R <- an@data[[an@varn@R]]
  r1_x <- r_x
  r0_x <- 1 - r1_x

  qr1 <- delta * r1_x / (delta * r1_x + r0_x)
  qr0 <- 1 - qr1

  q_r <- ifelse(R==1, qr1, qr0)

  list(qR = q_r, qr0 = qr0, qr1 = qr1, delta = delta)
}

v.x <- function(q_r, mu) {

  rvals <- c(0, 1)

  v_x <- list()
  for (tval in c(0, 1)) {

    v_x[[glue("v_t{tval}")]] <- lapply(rvals, \(rval) {
      mu[[glue("mu_t{tval}r{rval}")]] * q_r[[glue("qr{rval}")]]
    }) %>%
      Reduce(`+`, .)

  }
  v_x
}
