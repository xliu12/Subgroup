
library(origami)
library(mvtnorm)
library(parallel)
library(tidyverse)
library(glue)
library(SuperLearner)
library(mlr3superlearner)
library(mlr3extralearners)
# remotes::install_github('mlr-org/mlr3extralearners@*release')

source("simulation/_gendata.R")

devtools::load_all("Subgroup")
# Simulation----------------------------

condition_all <- data.frame(expand.grid(
  n = c(300, 500, 1000),
  proportion_r1 = c(0.05, 0.1, 0.5),
  quadratic.tt = c(0),
  quadratic.Y = c(1),
  Yfamily = c("gaussian")
))

condition <- condition_all 

methds_all <- data.frame(expand.grid(Fit = c("mlr")))
methds <- methds_all

set.seed(12)
datseeds <- c(sample(1:1e6, 1000), sample(1:1e6, 1000))

iseed <-1
cond <- 1



OneData <- function(iseed = 1, cond = 1){
  
  deltas <- c(1/20, 1/10, 1/5, 1, 5, 10, 20)
  # true values -----------------------------
  gen_largeN <- GenData(sim.seed = datseeds[iseed],
                        n = 1e4,
                        proportion_r1 = condition$proportion_r1[cond],
                        Yfamily = as.character(condition$Yfamily[cond]),
                        deltas = deltas,
                        quadratic.tt = condition$quadratic.tt[cond],
                        quadratic.Y = condition$quadratic.Y[cond]
  )
  true_values <- gen_largeN$true_val
  overall_r1 <- mean(gen_largeN$datobs$R)
  
  
  gen_data <- GenData(
    sim.seed = datseeds[iseed],
    n = condition$n[cond],
    proportion_r1 = condition$proportion_r1[cond],
    Yfamily = as.character(condition$Yfamily[cond]),
    deltas = deltas,
    quadratic.tt = condition$quadratic.tt[cond],
    quadratic.Y = condition$quadratic.Y[cond]
  )
  data <- gen_data$datobs
  # write.csv(data, "example/data_example_simulated.csv", row.names = FALSE)
  covariates <- grep("^X", colnames(data), value = TRUE)
  
  # data[, covariates] <- data[, covariates]^2
  # estimators -----
  j <- 1
  one.method <- function(j) {
    learners <- c("gam", "glm", "ranger", "earth", "svm", "lightgbm")
    mlr3superlearner_folds <- 10
    num_folds <- 5
    
    out <- incremental.inclusion(
      data = data,
      subgroup = "R",
      treatment = "tt",
      outcome = "Y",
      covariates = grep("^X", colnames(data), value = TRUE),
      deltas = c(1/20, 1/10, 1/5, 1, 5, 10, 20),
      learners = learners,
      control = analysis_control(crossfit_folds = num_folds, mlr3superlearner_folds = mlr3superlearner_folds, mlr3superlearner_discrete = FALSE)
    )
    
    estimates <- out$ATE_deltas %>%
      full_join(true_values, by = "estimand")
    
    estimates1 <- data.frame(methds[j, , drop=F],
                             estimates,
                             row.names = NULL)
    
    estimates1
  }
  
  est_allmethds <- lapply(1:nrow(methds), one.method)
  est_allmethds1 <- do.call(rbind, est_allmethds)
  
  res <- data.frame(condition[cond, ], overall_r1 = overall_r1,
                    est_allmethds1,
                    row.names = NULL)
  
  
  # res$estimate - res$truev
  
  return(res)
  
}


