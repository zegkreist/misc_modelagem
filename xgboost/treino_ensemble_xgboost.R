require(data.table)
require(xgboost)
require(magrittr)

rnorm_t <- function(n, mu, sd, upper = Inf, lower = -Inf){
  result <- rnorm(n    = n, 
                  mean = mu,
                  sd   = sd
                  )
  result[result < lower] <- lower
  result[result > upper] <- upper
  return(result)
}

xgb_treino_ensemble <- function(dados,
                                positivos,
                              #  metrica         = "logloss",
                              #  objetivo        = "binary:logistic",
                                nthreads        = 3,
                                parametros      = NULL,
                                n_models        = 100,
                                save_importance = F,
                                folder_to_save  = NULL){
  if(is.null(folder_to_save)){
    stop("É necessário que haja uma pasta para salvar os modelos")
  }
  if(is.null(parametros)){
    stop("É necessário que haja uma lista de parâmetros")
  }

  

  if(parametros$parametros$eval_metric %in% c("auc",
                    "aucpr",
                    "ndcg",
                    "map")
  ){
    maximize. <- T
  }else{
    maximize. <- F
  }
  
  
  N_MODELS  <- n_models
  importance <- list()
  
  for(j in 1:N_MODELS){
    
    cat(paste0("\n \n \n \n \n \n \t Sessao: ",j," ===============================================================\n"))
    
    # separando teste e treino ------------------------------------------------
    cat(paste0("separando teste e treino \t \t \t \t  ---- \n"))
    
    treino <- dados[,.SD[sample(.N, positivos,replace = T)],by = target]
    
    # preparando matrizes para rede -------------------------------------------
    cat(paste0("preparando matrizes para rede \t \t \t \t ---- \n"))

    treino <- treino[, lapply(.SD, as.numeric)]
    
    dtrain <- xgb.DMatrix(
      treino[,
             colnames(treino)[colnames(treino) != "target"],
             with = F
             ] %>%
        as.matrix(),
      label = treino$target
    )
    # seleção de parametros  --------------------------------------------------
    if(parametros$parametros$objective %in% c("multi:softmax", "multi:softprob")){
      tmp_param <- list(
        objective        = parametros$parametros$objective,
        eval_metric      = parametros$parametros$eval_metric,
        base_score       = sum(treino$target)/nrow(treino),
        max.leaves       = ceiling(rnorm_t(n = 1, mu = parametros$parametros$max.leaves, sd = 2, lower = 1)),
        #max.depth        = ceiling(runif(1,7,20)),
        eta              = rnorm_t(n = 1, mu = parametros$parametros$eta, sd = 0.02, lower = 0.001),
        gamma            = rnorm_t(n = 1, mu = parametros$parametros$gamma, sd = 0.5, lower = 0),
        subsample        = rnorm_t(n = 1, mu = parametros$parametros$subsample, sd = 0.1, lower = 0.01, upper = 1),
        colsample_bytree = rnorm_t(n = 1, mu = parametros$parametros$colsample_bytree, sd = 0.1, lower = 0.01, upper = 1),
        min_child_weight = rnorm_t(n = 1, mu = parametros$parametros$min_child_weight, sd = 2, lower = 0),
        grow.policy      = parametros$parametros$grow.policy,
        tree.method      = parametros$parametros$tree.method,
        num_class        = parametros$parametros$num_class
        #scale_pos_weight = sum(target == 0)/ sum(target == 1)
      )
    }else{
      tmp_param <- list(
        objective        = parametros$parametros$objective,
        eval_metric      = parametros$parametros$eval_metric,
        base_score       = sum(treino$target)/nrow(treino),
        max.leaves       = ceiling(rnorm_t(n = 1, mu = parametros$parametros$max.leaves, sd = 2, lower = 1)),
        #max.depth        = ceiling(runif(1,7,20)),
        eta              = rnorm_t(n = 1, mu = parametros$parametros$eta, sd = 0.02, lower = 0.001),
        gamma            = rnorm_t(n = 1, mu = parametros$parametros$gamma, sd = 0.5, lower = 0),
        subsample        = rnorm_t(n = 1, mu = parametros$parametros$subsample, sd = 0.1, lower = 0.01, upper = 1),
        colsample_bytree = rnorm_t(n = 1, mu = parametros$parametros$colsample_bytree, sd = 0.1, lower = 0.01, upper = 1),
        min_child_weight = rnorm_t(n = 1, mu = parametros$parametros$min_child_weight, sd = 2, lower = 0),
        grow.policy      = parametros$parametros$grow.policy,
        tree.method      = parametros$parametros$tree.method
        #scale_pos_weight = sum(target == 0)/ sum(target == 1)
      )
    }

    
    cat(paste0("Treinando modelo \t \t \t \t ---- \n"))
    
    md <- xgboost(data = dtrain,
                  params = tmp_param,
                  nrounds = parametros$nrounds,
                  nthread = nthreads,
                  verbose = F,
                  maximize = maximize.
    )
   
    #importance[[j]] <- xgb.importance(model = md)
    
    try({
      importance[[j]] <- xgb.importance(model = md)

    })
    
    
    xgb.save(model = md,
             fname = paste0(folder_to_save,"/xgb_SESSION_", j, ".xgb")
    )

    rm(md)
    gc(reset = T)
  }
  try({
    saveRDS(importance, file = paste0(folder_to_save, "/importancia_modelos",".RDS"))
  })
  
}

