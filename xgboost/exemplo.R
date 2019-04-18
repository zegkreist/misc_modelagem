

library(xgboost)
library(data.table)

data(iris)
species = iris$Species
label = as.integer(iris$Species)-1
iris$Species = NULL

n = nrow(iris)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(iris[train.index,])
train.label = label[train.index]

dados <- as.data.table(train.data)
dados[, target := label[train.index]]

source("predict_ensemble_xgboost.R")
source("selecao_parametros_xgboost.R")
source("treino_ensemble_xgboost.R")

parametros <- xgb_select_params(dados = dados,
                                positivos = 20,
                                objetivo = "multi:softmax",
                                metrica = "mlogloss",
                                niter = 10
                                )



xgb_treino_ensemble(dados = dados,
                    positivos = 10, 
                    parametros = parametros,
                    save_importance = T,
                    n_models = 100,
                    folder_to_save = "teste_models"
                    )


predito_final <- xgb_predict_ensemble(folder = "teste_models",
                                      newdata = dados,
                                      prob = F
                                      )




