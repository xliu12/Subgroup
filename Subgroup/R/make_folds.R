

# within cluster sample split and cross fit

make.fold_K <- function(data_in, Snames=NULL, cv_folds=4) {
    
    if (cv_folds <=1) {
        folds1 <- origami::make_folds(data_in,
                                      # fold_fun = origami::folds_vfold,
                                      V = 1)
        folds1[[1]]$training_set <- folds1[[1]]$validation_set
        folds <- folds1
        
        return(folds)
    }
    
    if (cv_folds > 1) {
        if (is.null(Snames)) {
            folds <- origami::make_folds(data_in,
                                         # fold_fun = origami::folds_vfold,
                                         V = cv_folds)
            return(folds) 
        }
        
        if (!is.null(Snames)) {
            if (length(Snames)>1 ) {
                data_in$S <- interaction(data_in[, Snames])
            } 
            if (length(Snames)==1 ) {
                data_in$S <- data_in[, Snames]
            } 
            K <- match(data_in[["S"]], unique(data_in[["S"]]))
            # folds <- origami::make_folds(data_in, V = cv_folds, strata_ids = K)
            
            data_in$id <- 1:nrow(data_in)
            fold_K <- lapply(unique(K), FUN = function(k=1) {
                
                if (nrow(data_in[K==k, ]) >= 1) {
                    fk <- origami::make_folds(data_in[K==k, ],
                                              #fold_fun = origami::folds_vfold,
                                              V = cv_folds)
                    fold_k <- fk
                    v <- 1
                    for(v in 1:length(fk)) {
                        fold_k[[v]]$validation_set <- data_in$id[K==k][fk[[v]]$validation_set]
                        fold_k[[v]]$training_set <- data_in$id[K==k][fk[[v]]$training_set]
                    }
                }
                
                return(fold_k)
            })
            
            folds <- origami::make_folds(data_in,
                                         fold_fun = origami::folds_vfold,
                                         V = cv_folds)
            
            for(v in 1:cv_folds) {
                folds[[v]]$validation_set <- unlist(lapply(1:length(fold_K), FUN = function(k=1) {
                    fold_K[[k]][[v]]$validation_set
                }))
                folds[[v]]$training_set <- unlist(lapply(1:length(fold_K), FUN = function(k=1) {
                    fold_K[[k]][[v]]$training_set
                }))
            }
            
            return(folds)
        }
        
    }
    
}
    
    
combine_folds <- function(pred_folds, folds) {
        ind <- Reduce(c, lapply(folds, function(x) x[["validation_set"]]))
        name1 <- names(pred_folds[[1]])[1]
        # ind[order(ind)]
        out <- list(valid_row = ind[order(ind)])
        
        for (name1 in names(pred_folds[[1]]) ) {
            vals <- Reduce(c, lapply(pred_folds, function(x) x[[name1]]))
            out[[name1]] <- vals[order(ind)]
        }
        
        out
    }
    
