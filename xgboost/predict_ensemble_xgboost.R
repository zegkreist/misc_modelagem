require(data.table)
require(xgboost)
require(magrittr)

reldtMtd <- function(dados) {
  melt(dados, id.vars="id")[, 
                        .N, by=.(id, value)][,
                                             value[which.max(N)], by=.(id)] 
}


xgb_predict_ensemble <- function(folder, newdata, prob = T){
  modelos_xgboost <- list()
  
  lista_modelos <- list.files(folder, 
                              pattern    = "*.xgb$",
                              all.files  = F,
                              full.names = F)
  
  for(i in 1:length(lista_modelos)){
    modelos_xgboost[[i]] <- xgb.load(paste0(folder,"/",lista_modelos))
  }
  
  
  multiple_models_dados <- c()
  for(i in 1:length(modelos_xgboost)){
    cat(paste0("Modelo: ", i," de ", length(modelos_xgboost),". \r"))
    code <- paste0("V", i,"<- predict(modelos_xgboost[[i]],  
                 newdata = newdata[,
               colnames(newdata)[!colnames(newdata) %in% c('target', 'fold')],
               with = F
               ] %>%
               as.matrix())"
    )
    eval(parse(text = code))
    if(i %%10 == 0){
      gc(reset = T)
    }
    code1 <- paste0("multiple_models_dados <- cbind(multiple_models_dados, V", i,") ; rm(V",i,") ")
    eval(parse(text = code1))
    # multiple_models_dados <- cbind(multiple_models_dados, tmp)    
  }
  if(prob){
    multiple_models <- as.matrix(multiple_models_dados) %>% 
      apply(1, FUN = median) %>%
      as.data.table()
    colnames(multiple_models) <-  c("predicao")
  }else{
    multiple_models <- as.data.table(as.matrix(multiple_models_dados))
    multiple_models[, id := 1:nrow(multiple_models)]
    multiple_models <- reldtMtd(dados = multiple_models)
    multiple_models[, id := NULL]
    colnames(multiple_models) <- c("predicao")
  }
  #return(cbind(multiple_models_dados, newdata))

  return(multiple_models)
  
}
