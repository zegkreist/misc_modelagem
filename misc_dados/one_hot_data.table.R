
# dados <- data.table(id = 1:10,
#                     fator_ = sample(letters[1:5], size = 10, replace = T))
# 

require(data.table)

one_hot <- function(dados, id_var, coded_var){
  tmp <- eval(parse(text = paste0("dados[, .(", id_var, ",", coded_var, ")]")))
  
  if(length(unique(eval(parse(text = paste0("dados$",id_var))))) < nrow(dados)){
    cat(paste0("\t \t O numero de id_var unico é menor que nrow(dados) será
               \t \t feito  tmp <- tmp[!duplicated(tmp)], em que, 
               \t \t  tmp <- ",paste0("dados[, .(", id_var, ",", coded_var, ")] \n")))
    tmp <- tmp[!duplicated(tmp)]
  }
  melted  <- data.table::melt(tmp, id = id_var, value.factor = F, na.rm=TRUE)
  one_hot_ <- data.table::dcast(melted, eval(parse(text = paste0(id_var, " ~ value")) ),
                               drop = T, fun.aggregate = length)
  return(one_hot_)
}



## EXEMPLO 


# tmp <- treino[, .(IdBeneficiario, CID)]
# tmp[is.na(CID), CID := "nenhum"]
# tmp <- tmp[!duplicated(tmp)]
# a <- one_hot(dados = treino,
#              id_var = "IdBeneficiario",
#              coded_var = "CID")