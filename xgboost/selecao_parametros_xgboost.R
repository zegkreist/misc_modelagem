require(data.table)
require(xgboost)
require(magrittr)
## Selecionador de parâmetros xgboost


# DADOS deve ser um tata.table pronto para treino

xgb_select_params <- function(dados,
                              positivos,
                              var_id      = NULL,
                              metrica     = "logloss",
                              objetivo    = "binary:logistic",
                              niter       = 100, 
                              nfolds      = 5,
                              nrounds     = 3000,
                              nthreads    = 3,
                              early_stop  = 50, 
                              tree_method = "hist"){
  
  if(!objetivo %in% c("reg:squarederror",
                      "reg:logistic",
                      "binary:logistic",
                      "binary:logitraw",
                      "binary:hinge",
                      "count:poisson",
                      "max_delta_step",
                      "survival:cox",
                      "multi:softmax",
                      "multi:softprob",
                      "rank:pairwise",
                      "rank:ndcg",
                      "rank:map",
                      "reg:gamma",
                      "reg:tweedie")
  ){
    stop(paste0("Seu objetivo não se encontra na lista abaixo: \n
                
  \t \t reg:squarederror: regressão com erro quadrático \n
  \t \t reg:logistic: regressão logística \n
  \t \t binary:logistic: regressão logística para classificação binária, a saída é probabilidade \n
  \t \t binary:logitraw: regressão logística para classificação binária, a saída é o logito antes da transformação \n
  \t \t binary:hinge: hinge loss para classificação binária. Isso faz predição das classes 1 e 0 e não retorna probabilidade \n
  \t \t count:poisson regressão poisson para contagens, a saída é a média de uma distribuição de poisson \n
  \t \t max_delta_step is set to 0.7 by default in poisson regression (used to safeguard optimization) \n
  \t \t survival:cox: Regressão Cox para análise de sobrevivência censurado à direita (Valores negativos são considerados censurados).\n A saída é hazard ratio scale (i.e., as HR = exp(marginal_prediction) in the proportional hazard function h(t) = h0(t) * HR) \n
  \t \t multi:softmax: Classificação multi-classe usando softmax, 
  \t \t multi:softprob: same as softmax, but output a vector of ndata * nclass, which can be further reshaped to ndata * nclass matrix. The result contains predicted probability of each data point belonging to each class \n
  \t \t rank:pairwise: Use LambdaMART to perform pairwise ranking where the pairwise loss is minimized \n
  \t \t rank:ndcg: Use LambdaMART to perform list-wise ranking where Normalized Discounted Cumulative Gain (NDCG) is maximized \n
  \t \t rank:map: Use LambdaMART to perform list-wise ranking where Mean Average Precision (MAP) is maximized \n
  \t \t reg:gamma: gamma regression with log-link. Output is a mean of gamma distribution. It might be useful, e.g., for modeling insurance claims severity, or for any outcome that might be gamma-distributed \n
  \t \t reg:tweedie: Tweedie regression with log-link. It might be useful, e.g., for modeling total loss in insurance, or for any outcome that might be Tweedie-distributed \n
                "))
  }

  if(!metrica %in% c("rmse",
                     "mae",
                     "logloss",
                     "error",
                     "error@t",
                     "merror",
                     "mlogloss",
                     "auc",
                     "aucpr",
                     "ndcg",
                     "map",
                     "poisson-nloglik",
                     "gamma-nloglik",
                     "cox-nloglik",
                     "cox-nloglik",
                     "gamma-deviance",
                     "tweedie-nloglik"
                     )
     ){
    stop(paste0("A métrica escolhida não está entre: \n
    \t \t rmse: root mean square error\n
    \t \t mae: mean absolute error\n
    \t \t logloss: negative log-likelihood\n
    \t \t error: Binary classification error rate. It is calculated as #(wrong cases)/#(all cases). For the predictions, the evaluation will regard the instances with prediction value larger than 0.5 as positive instances, and the others as negative instances \n
    \t \t merror: Multiclass classification error rate. It is calculated as #(wrong cases)/#(all cases) \n
    \t \t mlogloss: Multiclass logloss \n
    \t \t auc: Area under the curve \n
    \t \t aucpr: Area under the PR curve \n
    \t \t ndcg: Normalized Discounted Cumulative Gain \n
    \t \t map: Mean Average Precision \n
    \t \t poisson-nloglik: negative log-likelihood for Poisson regression \n
    \t \t gamma-nloglik: negative log-likelihood for gamma regression \n
    \t \t cox-nloglik: negative partial log-likelihood for Cox proportional hazards regression \n
    \t \t gamma-deviance: residual deviance for gamma regression \n
    \t \t tweedie-nloglik: negative log-likelihood for Tweedie regression (at a specified value of the tweedie_variance_power parameter) \n
    ")
         )
   }
  cat(paste0("Criando uma amostra balanceada para as iterações \t \t \t \t ---- \n"))
  
  
  treino <- dados[,.SD[sample(.N, positivos,replace = T)],by = target]
  if(!is.null(var_id)){
    eval(parse(text = paste0("treino[,", var_id," := NULL]")))  
  }
  
  if(objetivo %in% c("multi:softmax", "multi:softprob")){
    num_class <- length(unique(treino$target))
  }
  if(metrica %in% c("auc",
                    "aucpr",
                    "ndcg",
                    "map")
  ){
    maximize. <- T
  }else{
    maximize. <- F
  }
  
  cat(paste0("Transformando as colunas para numeric \t \t \t \t ---- \n"))
  treino <- treino[, lapply(.SD, as.numeric)]
  
  
  cat(paste0("Criando a xgb matrix \t \t \t \t ---- \n"))
  dtrain <- xgb.DMatrix(
    treino[,
           colnames(treino)[colnames(treino) != "target"],
           with = F
           ] %>%
      as.matrix(),
    label = treino$target
  )
  
  
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
