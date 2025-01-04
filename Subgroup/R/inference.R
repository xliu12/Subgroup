

get.inference <- function(eif_list, type = "additive") {
  n <- length(eif_list[[1]])

  if (type == "additive") {
    estimate <- map_dbl(eif_list, mean)
    std.error <- map_dbl(eif_list, ~sqrt(var(.)/n) )
    ci.low <- map_dbl(eif_list, ~mean(.)+ qnorm(0.025)*sqrt(var(.)/n) )
    ci.high <- map_dbl(eif_list, ~mean(.)+ qnorm(0.975)*sqrt(var(.)/n) )
  }


  data.frame(estimate=estimate, std.error=std.error, ci.low=ci.low, ci.high=ci.high) %>%
    rownames_to_column(var = "estimand")

}
