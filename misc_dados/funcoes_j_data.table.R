


j_count <- function(coluna_alvo,
                    vetor_niveis_unicos,
                    nome_colunas){
  if(length(coluna_alvo) != 1 | !is.character(coluna_alvo)){
    stop("Coluna_alvo deve ser o nome, em character, da coluna que pretende calcular algo.")
  }
  calculo <- paste0("sum( ", coluna_alvo, " == ", vetor_niveis_unicos, ")")
  string <- paste( ".(",paste(nome_colunas, 
                              calculo,
                              sep = " = ",
                              collapse = ", "), ")")
  return(parse(text = string))
  }


pull_right <- function(columns,
                       exceptions = NULL){
  if(is.null(exceptions)){
    stop("Exceptions deve ser as colunas usadas como Key na tabela da direita")
  }
  string <- paste( "`:=`(",paste(columns, columns, sep = "=", collapse = ","), ")")
  return(parse(text =string))
}