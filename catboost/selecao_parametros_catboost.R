require(data.table)
require(xgboost)
require(magrittr)
## Selecionador de parâmetros xgboost


# DADOS deve ser um tata.table pronto para treino

xgb_select_params <- function(dados,
                              positivos,
                              metrica     = "logloss",
                              objetivo    = "binary:logistic",
                              niter       = 100, 
                              nfolds      = 5,
                              nrounds     = 3000,
                              nthreads    = 3,
                              early_stop  = 50, 
                              variaveis_categoricas = NULL,
                              tree_method = "hist"){
  

  cat(paste0("Criando uma amostra balanceada para as iterações \t \t \t \t ---- \n"))
  
  treino <- dados[,.SD[sample(.N, positivos,replace = T)],by = target]
  

  
  cat(paste0("Iniciando o loop  \t \t \t \t ---- \n"))
  
  best_param         <- list()
  best_metric        <- Inf
  best_metric_index  <- 0
  for (iter in 1:niter) {
    if(objetivo %in% c("multi:softmax", "multi:softprob")){
      param <- list(
        objective        = objetivo,
        eval_metric      = metrica,
        base_score       = sum(treino$target)/nrow(treino),
        max.leaves       = ceiling(runif(1,5,60)),
        #max.depth        = ceiling(runif(1,7,20)),
        eta              = runif(1, 0.002, 0.2),
        gamma            = runif(1, 0, 10),
        subsample        = runif(1, 0.3, .9),
        colsample_bytree = runif(1, 0.1, .9),
        min_child_weight = runif(1, 0, 10),
        grow.policy      = "lossguide",
        tree.method      = tree_method,
        num_class        = num_class
        #scale_pos_weight = sum(target == 0)/ sum(target == 1)
      )
    }else{
      param <- list(
        objective        = objetivo,
        eval_metric      = metrica,
        base_score       = sum(treino$target)/nrow(treino),
        max.leaves       = ceiling(runif(1,5,60)),
        #max.depth        = ceiling(runif(1,7,20)),
        eta              = runif(1, 0.002, 0.2),
        gamma            = runif(1, 0, 10),
        subsample        = runif(1, 0.3, .9),
        colsample_bytree = runif(1, 0.1, .9),
        min_child_weight = runif(1, 0, 10),
        grow.policy      = "lossguide",
        tree.method      = tree_method
        #scale_pos_weight = sum(target == 0)/ sum(target == 1)
      )
    }
    
    cv.nround = nrounds
    cv.nfold = nfolds
    
    mdcv <- xgb.cv(
      data                  = dtrain,
      params                = param,
      nthread               = nthreads,
      nfold                 = cv.nfold,
      nrounds               = cv.nround,
      verbose               = F,
      early_stopping_rounds = early_stop,
      maximize              = maximize.
    )
    
    if(maximize.){
      actual_metric       <- max(mdcv$evaluation_log[, 4][[1]])
      actual_metric_index <- which.max((mdcv$evaluation_log[, 4][[1]]))
      if (actual_metric > best_metric) {
        best_metric         <- actual_metric
        best_metric_index   <- actual_metric_index
        best_param          <- param
      }
    }else{
      actual_metric       <- min(mdcv$evaluation_log[, 4][[1]])
      actual_metric_index <- which.min((mdcv$evaluation_log[, 4][[1]]))
      if (actual_metric < best_metric) {
        best_metric         <- actual_metric
        best_metric_index   <- actual_metric_index
        best_param          <- param
      }
    }
    
    cat(paste0("Sorteio de parametros de n: ",iter," :::: best metric: ",best_metric, " Nrounds: ",best_metric_index, "\r"))
    
    gc(reset = T)
  }
  
  return(list(parametros  = best_param,
              nrounds     = best_metric_index,
              best_metric = best_metric)
  )
  
}


#parametros_select <- xgb_select_params(dados = dados, positivos = 5000, niter = 10)
