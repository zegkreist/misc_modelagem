#include <Rcpp.h>
#include <unordered_set>
#include <map>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
#include <array>
#include <fstream> 

using namespace Rcpp;
using namespace std;

void vector_to_matrix(std::vector<float>& A,
                      std::vector<float>& B,
                      std::vector<float>& C,
                      NumericMatrix& result ){
  unsigned n = A.size();
  
  for(unsigned i=0; i<n; i++){
    result(i,0) = A[i];
    result(i,1) = B[i];
    result(i,2) = C[i];
  }
}

//[[Rcpp::export]]
void split(const std::string& s, char delim,std::vector<std::string>& v) {
  int i = 0;
  std::string::size_type pos = s.find(delim);
  if(pos == std::string::npos){
    v.push_back(s);
  }
  while (pos != std::string::npos) {
    v.push_back(s.substr(i, pos-i));
    i = ++pos;
    pos = s.find(delim, pos);
    
    if (pos == std::string::npos)
      v.push_back(s.substr(i, s.length()));
  }
}
//[[Rcpp::export]]
void addVector_up(std::vector<double>& a, std::vector<double>& b) {
  for (int i = 0; i < a.size(); i++) {
    a[i] += b[i];
  }
}
//[[Rcpp::export]]
void normalizador(NumericMatrix& A){
  for(int i = 0; i< A.nrow();i++){
    float modulo = ::sqrt( std::inner_product(A(i,_).begin(), A(i,_).end(), A(i,_).begin(),0.0f));
    for(int j= 0; j <A.ncol();j++){
      A(i,j) = A(i,j)/(modulo);
    }
  }
}
//[[Rcpp::export]]
std::map<std::string, std::vector<double> > creating_map_token_w2v(StringVector& tokens, NumericMatrix& dicio){
  std::map<std::string, std::vector<double> > dic;
  int nwv = dicio.ncol();
  std::string tmp ;
  std::vector<std::string> token_split;
  for(int i = 0; i< tokens.size();i++){
    for(int j= 0; j <nwv;j++){
      tmp = tokens[i];
      dic[tmp].push_back(dicio(i,j));
    }
  }
  return(dic);
}
//[[Rcpp::export]]
std::vector<bool> vector_string_comp( StringVector& x, std::string& y, std::vector<bool>& r){
  std::string s;
  for( int i=0; i<x.size(); i++){
    s = x[i];
    if( x[i] == y){
      r.push_back(true) ;
    }else{
      r.push_back(false);
    }
  }
  return(r);
}
void string_vec_subset(StringVector& x, std::vector<bool>& y, std::vector<std::string>& result){
  
  for(int i = 0; i < x.size(); i++){
    if(y[i]){
      result.push_back(Rcpp::as<std::string>(x[i]));
    }
  }
}

void col_means( NumericMatrix& X ,  NumericMatrix& result, int l) {
  
  int nCols = X.ncol();
  float dem;
  dem = X.nrow();
  float tmp_m;
  for( int j=0; j < nCols; j++ ) {
    tmp_m = 0;
    for(int i = 0; i < X.size(); i++){
      tmp_m =  tmp_m + X(i,j);
    }
    tmp_m = tmp_m/ dem;
    result(l,j) = tmp_m;
  }
  
}
//[[Rcpp::export]]
NumericMatrix centroid_cat(StringVector& descricao, StringVector& categoria, StringVector& cat_unique,int n, 
                           StringVector& tokens, NumericMatrix& dicio){
  // Esta funcao tem como objetivo de calcular os centroids para cada categoria
  // o Vetor descricao sao os exemplos descricoes disponiveis
  // o Vetor categoria e a categoria correspondente a cada descricao do vetor anterior
  // o vetor cat_unique sao as categorias unicas, os centroids serao dispostos na ordem presenta na cat_unique
  // n e o numero de palavras consideradas
  // tokens sao as tokens unicas encontradas nas descricoes, usada para construir o dicionario
  // dicio e o w2v resultante do fastTExt
  std::map<std::string, std::vector<double> > dic;
  int nwv = dicio.ncol();
  double n_tmp = n;
  std::string tmp ;
  std::vector<std::string> token_split;
  for(int i = 0; i< tokens.size();i++){
    for(int j= 0; j <nwv;j++){
      tmp = tokens[i];
      dic[tmp].push_back(dicio(i,j));
    }
  }
  std::vector<std::string> subset_descricao;
  NumericMatrix resultado(cat_unique.size(),dicio.ncol());
  std::string cat_atual;
  std::vector<bool> filter;
  for(int cat = 0; cat < cat_unique.size(); cat++){
    std::cout <<"Parte "<<cat+1<<" de "<<cat_unique.size() <<"\r";
    cat_atual = cat_unique[cat];
    filter.clear();
    subset_descricao.clear();
    vector_string_comp(categoria, cat_atual,filter);
    string_vec_subset(descricao,filter,subset_descricao);
    //NumericMatrix temp_toda_cat(subset_descricao.size(),nwv);
    std::string s;
    float dem;
    dem = subset_descricao.size();
    for(int i = 0; i < subset_descricao.size();i++){
      //  if( i % 100000 == 0 ){
      //}
      s = subset_descricao[i];
      token_split.clear();
      split(s,' ',token_split);
      int size = token_split.size();
      std::vector<double> temporario(nwv);
      for(int j = 0; j < n; j++){
        if(size <= (j)){
          break;
        }
        if(dic.count( token_split[j] ) == 0){
          std::cout << "Token nao encontrada no mapa. \n";
        }
        
        if(j == 0){
          for(int k =0; k < nwv; k++){
            temporario[k] = dic[token_split[j]][k];
          }
        }else{
          addVector_up(temporario,dic[token_split[j]]);
        }
      }
      for(int k =0; k < nwv; k++){
        temporario[k] = temporario[k] * (1.0/n_tmp);
        //temp_toda_cat(i,k) = temporario[k];
        resultado(cat,k) += temporario[k];
      }
      //for(int k = 0; k< nwv; k++){
      // resultado(cat,k) += temp_toda_cat(i,k);
      //}
      //Rcout <<"SO UM TESTE "<< temp_toda_cat(i,0)<<"\n";
    }
    for(int k = 0; k < nwv; k++){
      resultado(cat,k) = resultado(cat,k)/dem;
    }
    //col_means(temp_toda_cat,resultado,cat);
    
    //  resultado(i,_) = Rcpp::colMeans(temp_toda_cat);
  }
  return resultado;
}
//[[Rcpp::export]]
NumericMatrix cosine_C(NumericMatrix A){
  NumericMatrix result(A.nrow(),A.nrow());
  NumericVector tmp;
  std::vector<float> modulos(A.nrow());
  for(int i=0; i < A.nrow();i++){
    modulos[i] = ::sqrt( std::inner_product(A(i,_).begin(), A(i,_).end(), A(i,_).begin(),0.0f));
  }
  for(int i=1; i < A.nrow();i++){
    if(i % 100 == 0){
      printf("Parte 1: Linha %i \n",i);
    }
    for(int j=0; j < i; j++){
      result(i,j) = 1- std::inner_product(A(i,_).begin(), A(i,_).end(), A(j,_).begin(),0.0f)/ (modulos[i]* modulos[j]);
      result(j,i) = result(i,j);
    }
  }
  for( int i = 0; i < result.nrow(); i++ )
    result(i,i) = 0.0;
  
  
  return result;
}
//[[Rcpp::export]]
IntegerVector max_col_C(NumericMatrix A, int n){
  if(n > A.ncol()){
    std::cout << "Nth maior que numero de colunas. \n";
    return 0;
  }
  std::vector< float >  B;
  std::vector< float >  C;
  Rcpp::IntegerVector result(A.nrow());
  for(int i = 0 ; i < A.nrow();i++){
    B.clear();
    C.clear();
    for(int j = 0 ; j < A.ncol(); j++){
      B.push_back(A(i,j));
      C.push_back(A(i,j));
    }
    std::nth_element(B.begin(), B.begin()+n-1,
                     B.end(), std::greater<float>());
    float nth= *(B.begin()+n-1);
    
    int t = std::distance(C.begin(), std::find(C.begin(), C.end(), nth));
    result(i) = t+1;
  }
  return result;
}
//[[Rcpp::export]]
NumericVector max_num_C(NumericMatrix A, int n){
  if(n > A.ncol()){
    std::cout << "Nth maior que numero de colunas. \n";
    return 0;
  }
  std::vector< float >  B;
  std::vector< float >  C;
  Rcpp::NumericVector result(A.nrow());
  for(int i = 0 ; i < A.nrow();i++){
    B.clear();
    C.clear();
    for(int j = 0 ; j < A.ncol(); j++){
      B.push_back(A(i,j));
      C.push_back(A(i,j));
    }
    std::nth_element(B.begin(), B.begin()+n-1,
                     B.end(), std::greater<float>());
    result(i) = *(B.begin()+n-1);
  }
  return result;
}
//[[Rcpp::export]]
NumericMatrix cosine_C_V2(NumericMatrix A){
  NumericMatrix result(A.nrow(),A.nrow());
  
  std::vector<std::vector<float> > normalizados(A.nrow());
  
  for(int i=0; i < A.nrow();i++){
    float modulo = ::sqrt( std::inner_product(A(i,_).begin(), A(i,_).end(), A(i,_).begin(),0.0f));
    
    normalizados[i].reserve(A.ncol());
    
    for(int j=0; j < A.ncol(); j++)
      normalizados[i].push_back(A(i,j) / modulo);
  }
  
  for(int i=1; i < A.nrow();i++){
    if(i % 100 == 0){
      printf("Parte 1: Linha %i \n",i);
    }
    for(int j=0; j < i; j++){
      result(i,j) = 1 - std::inner_product(normalizados[i].begin(), normalizados[i].end(), normalizados[j].begin(), 0.0f);
      result(j,i) = result(i,j);
    }
  }
  for( int i = 0; i < result.nrow(); i++ )
    result(i,i) = 0.0;
  
  
  return result;
}
//[[Rcpp::export]]
NumericMatrix cosine_C_A_B(NumericMatrix A,NumericMatrix B){
  NumericMatrix result(A.nrow(),B.nrow());
  
  std::vector<std::vector<float> > normalizados(A.nrow());
  std::vector<std::vector<float> > normalizados_B(B.nrow());
  // normalizando os vetores da matriz A
  for(int i=0; i < A.nrow();i++){
    float modulo = ::sqrt( std::inner_product(A(i,_).begin(), A(i,_).end(), A(i,_).begin(),0.0f));
    
    normalizados[i].reserve(A.ncol());
    
    for(int j=0; j < A.ncol(); j++)
      normalizados[i].push_back(A(i,j) / modulo);
  }
  // normalizando os vetores da matriz B
  for(int i=0; i < B.nrow();i++){
    float modulo = ::sqrt( std::inner_product(B(i,_).begin(), B(i,_).end(), B(i,_).begin(),0.0f));
    
    normalizados_B[i].reserve(B.ncol());
    
    for(int j=0; j < B.ncol(); j++)
      normalizados_B[i].push_back(B(i,j) / modulo);
  }
  // calculando a distancia
  for(int i=0; i < A.nrow();i++){
    if(i % 10000 == 0){
      printf("Parte 1: Linha %i \r",i);
    }
    for(int j=0; j < B.nrow(); j++){
      result(i,j) = 1 - std::inner_product(normalizados[i].begin(), normalizados[i].end(), normalizados_B[j].begin(), 0.0f);
      //result(j,i) = result(i,j);
    }
  }
  // for( int i = 0; i < result.nrow(); i++ )
  // result(i,i) = 0.0;
  
  
  return result;
}

//[[Rcpp::export]]
NumericMatrix similarity_cosine_A_B(NumericMatrix A,NumericMatrix B, float threshold){
  
  // std::vector<std::vector<float> > result;
  std::vector<std::vector<float> > normalizados(A.nrow());
  std::vector<std::vector<float> > normalizados_B(B.nrow());
  std::vector<float> tmp_a;
  std::vector<float> tmp_b;
  std::vector<float> tmp_c;
  float nota;
  // normalizando os vetores da matriz A
  for(int i=0; i < A.nrow();i++){
    float modulo = ::sqrt( std::inner_product(A(i,_).begin(), A(i,_).end(), A(i,_).begin(),0.0f));
    
    normalizados[i].reserve(A.ncol());
    
    for(int j=0; j < A.ncol(); j++)
      normalizados[i].push_back(A(i,j) / modulo);
  }
  // normalizando os vetores da matriz B
  for(int i=0; i < B.nrow();i++){
    float modulo = ::sqrt( std::inner_product(B(i,_).begin(), B(i,_).end(), B(i,_).begin(),0.0f));
    
    normalizados_B[i].reserve(B.ncol());
    
    for(int j=0; j < B.ncol(); j++)
      normalizados_B[i].push_back(B(i,j) / modulo);
  }
  // calculando a distancia
  for(int i=0; i < A.nrow();i++){
    if(i % 10000 == 0){
      printf("Parte 1: Linha %i \r",i);
    }
    for(int j=0; j < B.nrow(); j++){
      nota = 1 - std::inner_product(normalizados[i].begin(), normalizados[i].end(), normalizados_B[j].begin(), 0.0f);
      //result(j,i) = result(i,j);
      if(nota <= threshold){
        tmp_a.push_back(i+1);
        tmp_b.push_back(j+1);
        tmp_c.push_back(nota);
      }
    }
  }
  NumericMatrix result(tmp_a.size(),3);
  vector_to_matrix(tmp_a,tmp_b,tmp_c, result);
  // for( int i = 0; i < result.nrow(); i++ )
  // result(i,i) = 0.0;
  
  
  return result;
}
//[[Rcpp::export]]
StringVector substr_word(StringVector x, NumericVector ini,NumericVector fim) {
  int tmp_ini;
  int tmp_fim;
  std::string s ;
  std::string delimiter = " ";
  StringVector result(x.size());
  std::string tmp;
  std::vector<std::string> token;
  for( int i=0; i < x.size(); i++ ){
    tmp_ini = ini[i];
    tmp_fim = fim[i];
    if(tmp_ini == 999999 || tmp_fim == 999999 ){
      result[i] = " ";
    } else{
      s = Rcpp::as< std::string >(x[i]);
      token.clear();
      split(s,' ',token);
      if((tmp_ini+1) > token.size() || (tmp_fim+1) > token.size()){
        result[i] = " ";
      }else{
        if(tmp_ini > tmp_fim){
          result[i] = " ";
        }else{
          tmp = token[tmp_ini];
          for(int j = (tmp_ini +1 ); j <= tmp_fim; j++ ){
            tmp.append(" ");
            tmp.append(token[j]);
          }
          result[i] = tmp;
        }
      }
    }
  }
  return result;
  
}
//[[Rcpp::export]]
StringVector substr_word_out(StringVector x, NumericVector ini,NumericVector fim) {
  int tmp_ini;
  int tmp_fim;
  std::string s ;
  std::string delimiter = " ";
  StringVector result(x.size());
  std::string tmp;
  std::vector<std::string> token;
  for( int i=0; i < x.size(); i++ ){
    tmp_ini = ini[i];
    tmp_fim = fim[i];
    if(tmp_ini == 999999 || tmp_fim == 999999 ){
      result[i] = " ";
    } else{
      s = Rcpp::as< std::string >(x[i]);
      token.clear();
      split(s,' ',token);
      tmp = "";
      for(int j = 0; j < token.size(); j++ ){
        if(j < tmp_ini || j > tmp_fim){
          if(j == 0){
            tmp.append(token[j]);
          }else{
            tmp.append(" ");
            tmp.append(token[j]);
          }
        }
        
      }
      result[i] = tmp;
      
      
    }
  }
  return result;
  
}
//[[Rcpp::export]]
StringVector remove_target(StringVector x, NumericVector ini,NumericVector fim) {
  int tmp_ini;
  int tmp_fim;
  std::string s ;
  std::string delimiter = " ";
  StringVector result(x.size());
  std::string tmp;
  std::vector<std::string> token;
  int inicializador;
  for( int i=0; i < x.size(); i++ ){
    tmp_ini = ini[i];
    tmp_fim = fim[i];
    if(tmp_ini == 999999 || tmp_fim == 999999 ){
      result[i] = " ";
    } else{
      s = Rcpp::as< std::string >(x[i]);
      token.clear();
      split(s,' ',token);
      if(tmp_ini == 0 ){
        tmp = token[1];
        inicializador = 2;
      }else{
        tmp = token[0];
        inicializador = 1;
      }
      for(int j = inicializador; j < token.size(); j++ ){
        if(j <= tmp_fim && j >= tmp_ini){
          continue;
        }else{
          tmp.append(" ");
          tmp.append(token[j]);
        }
      }
      result[i] = tmp;
      
      
    }
  }
  return result;
  
}

//[[Rcpp::export]]
StringVector substr_word_complemento(StringVector x,StringVector dicio, NumericVector ini,NumericVector fim) {
  std::unordered_set<Rcpp::String> dic;
  for (int i = 0; i < dicio.size(); i++){
    dic.insert(dicio[i]);
  }
  
  int tmp_ini;
  int tmp_fim;
  std::string s ;
  std::string delimiter = " ";
  StringVector result(x.size());
  std::string tmp;
  std::vector<std::string> token;
  for( int i=0; i < x.size(); i++ ){
    tmp_ini = ini[i];
    tmp_fim = fim[i];
    if( (tmp_ini == 999999 && tmp_fim == 999999) || (tmp_ini != 999999 && tmp_fim != 999999) ){
      result[i] = " ";
    } else{
      if((tmp_ini != 999999 && tmp_fim == 999999) ){
        s = Rcpp::as< std::string >(x[i]);
        token.clear();
        split(s,' ',token);
        tmp = token[tmp_ini];
        if(dic.count(tmp)>0){
          result[i] = tmp;
        }else{
          result[i] = " ";
        }
      }else{
        s = Rcpp::as< std::string >(x[i]);
        token.clear();
        split(s,' ',token);
        tmp = token[tmp_fim];
        if(dic.count(tmp)>0){
          result[i] = tmp;
        }else{
          result[i] = " ";
        }
      }
      
    }
  }
  return result;
}

//[[Rcpp::export]]
NumericMatrix sentence_to_vec(StringVector tokens, NumericMatrix dicio, StringVector desc, int n, int nwv){
  NumericMatrix result(desc.size(),n*nwv);
  std::map<Rcpp::String, Rcpp::NumericVector > dic;
  std::string s ;
  std::vector<std::string> token_split;
  for(int i = 0; i< tokens.size();i++){
    for(int j= 0; j <nwv;j++){
      dic[tokens[i]].push_back(dicio(i,j));
    }
  }
  for(int i = 0; i < desc.size();i++){
    s = desc[i];
    token_split.clear();
    split(s,' ',token_split);
    int interador = 0;
    int size = token_split.size();
    for(int j = 0; j < n; j++){
      if(size <= (j)){
        break;
      }
      for(int k =0; k < nwv; k++){
        result(i,interador) = dic[token_split[j]][k];
        interador++;
      }
    }
    
  }
  std::cout << "ACABOU \n";
  return result;
}
//[[Rcpp::export]]
void sentence_to_vec_V2(StringVector tokens, NumericMatrix dicio, StringVector desc, int n, int nwv, NumericMatrix& result){
  std::map<std::string, std::vector<double> > dic;
  std::string s ;
  std::string tmp ;
  std::vector<std::string> token_split;
  for(int i = 0; i< tokens.size();i++){
    for(int j= 0; j <nwv;j++){
      tmp = tokens[i];
      dic[tmp].push_back(dicio(i,j));
    }
  }
  for(int i = 0; i < desc.size();i++){
    //  if( i % 100000 == 0 ){
    //}
    s = desc[i];
    token_split.clear();
    split(s,' ',token_split);
    int interador = 0;
    int size = token_split.size();
    for(int j = 0; j < n; j++){
      if(size <= (j)){
        break;
      }
      if(dic.count( token_split[j] ) == 0){
        std::cout << "Token nao encontrada no mapa. \n";
        return;
      }
      for(int k =0; k < nwv; k++){
        result(i,interador) = dic[token_split[j]][k];
        interador++;
      }
    }
  }
}
//[[Rcpp::export]]
void sentence_to_vec_V2_all_sentence(StringVector& tokens, NumericMatrix& dicio, StringVector& desc, int n, int nwv, NumericMatrix& result){
  std::map<std::string, std::vector<double> > dic;
  double n_tmp = n;
  std::string s ;
  std::string tmp ;
  std::vector<std::string> token_split;
  for(int i = 0; i< tokens.size();i++){
    for(int j= 0; j <nwv;j++){
      tmp = tokens[i];
      dic[tmp].push_back(dicio(i,j));
    }
  }
  for(int i = 0; i < desc.size();i++){
    //  if( i % 100000 == 0 ){
    //}
    s = desc[i];
    token_split.clear();
    split(s,' ',token_split);
    int size = token_split.size();
    std::vector<double> temporario(nwv);
    for(int j = 0; j < n; j++){
      if(size <= (j)){
        break;
      }
      if(dic.count( token_split[j] ) == 0){
        std::cout << "Token nao encontrada no mapa. \n";
        return;
      }
      
      if(j == 0){
        for(int k =0; k < nwv; k++){
          temporario[k] = dic[token_split[j]][k];
        }
      }else{
        addVector_up(temporario,dic[token_split[j]]);
      }
    }
    for(int k =0; k < nwv; k++){
      temporario[k] = temporario[k] * (1.0/n_tmp);
      result(i,k) = temporario[k];
    }
  }
}


//[[Rcpp::export]]
void sentence_to_vec_V2_PRINT(StringVector tokens, NumericMatrix dicio, StringVector desc, int n, int nwv, NumericMatrix& result, std::string path){
  std::map<Rcpp::String, Rcpp::NumericVector > dic;
  std::ofstream out(path);
  std::string s ;
  std::vector<std::string> token_split;
  for(int i = 0; i< tokens.size();i++){
    for(int j= 0; j <nwv;j++){
      dic[tokens[i]].push_back(dicio(i,j));
    }
  }
  for(int i = 0; i < desc.size();i++){
    s = desc[i];
    token_split.clear();
    split(s,' ',token_split);
    int interador = 0;
    int size = token_split.size();
    for(int j = 0; j < n; j++){
      if(size <= (j)){
        break;
      }
      for(int k =0; k < nwv; k++){
        result(i,interador) = dic[token_split[j]][k];
        interador++;
        
      }
    }
    for(int column = 0; column < n*nwv; column++){
      out << result(i,column) << ',';
    }
    out << '\n';
  }
  out.close();
  std::cout << "ACABOU \n";
}
//[[Rcpp::export]]
void de_para_presencial(IntegerVector& id_matriz, IntegerVector& id_estabelecimento,
                        std::vector<std::string> de, std::vector<std::string> para,
                        IntegerVector& id_matriz_presencial, IntegerVector& id_estabelecimento_presencial,
                        std::vector<std::string> desc_presencial ,StringVector& result){
  
  std::map< int, std::map<std::string, std::string> > dicionario_matriz;
  std::map< int, std::map<std::string, std::string> > dicionario_estabelecimento;
  for( int i = 0; i < id_matriz.size(); i++){
    if(IntegerVector::is_na(id_estabelecimento[i])){
      dicionario_matriz[id_matriz[i]][de[i]] = para[i];
    }else{
      dicionario_estabelecimento[id_estabelecimento[i]][de[i]] = para[i];
    }
  }
  for(int i = 0; i < desc_presencial.size(); i++){
    if(desc_presencial[i].compare("") == 0 || desc_presencial[i].compare("") == 0){
      continue;
    }
    if(id_estabelecimento_presencial[i] != 2656){
      result(i) = dicionario_matriz[id_matriz_presencial[i]][desc_presencial[i]];
    }else{
      result(i) = dicionario_estabelecimento[id_estabelecimento_presencial[i]][desc_presencial[i]];
    }
  }
  
  std::cout << "ACABOU \n";
}
//[[Rcpp::export]]
std::vector<std::string> in_dicio(std::vector<std::string> label_categoria,
                                  std::vector<std::string> de,
                                  std::vector<std::string> para,
                                  std::vector<std::string> descricao,
                                  std::vector<std::string> categoria_obs){
  std::map< std::string, std::map<std::string, std::string> > dicio_de_para;
  std::vector<std::string> result;
  std::vector<std::string> token_split;
  std::string s;
  for( int i = 0; i < label_categoria.size(); i++){
    dicio_de_para[label_categoria[i]][de[i]] = para[i];
  }
  std::cout << "Iniciando descricoes" << std::endl;
  for(int i=0; i < descricao.size();i++){
    s = descricao[i];
    token_split.clear();
    split(s,' ',token_split);
    if( i % 100000 == 0 ){
      std::cout<< "Linha " << i << " de "<< descricao.size() << "\r";
    }
    result.push_back("");
    for(int j =0; j < token_split.size();j++){
      
      if(dicio_de_para[categoria_obs[i]][token_split[j]].size() != 0){
        result[i] += dicio_de_para[categoria_obs[i]][token_split[j]];
        result[i] += "|";
      }
      
    }
    if(result[i].size()>0){
      result[i].resize(result[i].size()-1);
    }
  }
  return result;
}

//[[Rcpp::export]]
bool charCoringa(char x){
  if(x == '?'){
    return true;
  }else{
    return false;
  }
}
//[[Rcpp::export]]
int abreviacao(std::string p, std::string g) {
  if (p.size() > g.size())
    return 0;
  
  if ((p.size() == 1) && (charCoringa(p[0])))
    return 0;
  
  if ((p[0] != g[0]) || (charCoringa(p[0])) || (charCoringa(g[0])))
    return 0;
  
  int j = 0;
  
  for(int i = 0; i < p.size(); i++) {
    while((j < g.size()) && (p[i] != g[j]) && (!charCoringa(p[i])) && (!charCoringa(g[j]))){
      j++;
    }
    if (j >= g.size())
      return 0;
    
    j++;
  }
  
  return 1;
}
//[[Rcpp::export]]
NumericMatrix matrix_abreviacao(StringVector x){
  NumericMatrix result(x.size(),x.size());
  std::string p;
  std::string g;
  for(int i = 0; i<x.size(); i++){
    for(int j = 0; j<x.size();j++){
      p = x[i];
      g = x[j];
      result(i,j) = abreviacao(p,g);
    }
  }
  return result;
}
//[[Rcpp::export]]
std::vector<std::string> substr_C(std::vector<std::string> string_r, int ini, int comprimento){
  std::vector<std::string> result(string_r.size());
  ini = ini -1;
  for(int i = 0; i < string_r.size(); i++ ){
    result[i] = string_r[i].substr(ini,comprimento);
  }
  return result;
}
//[[Rcpp::export]]
std::vector< std::string > aplicando_cat(std::vector<std::string> desc_original, std::vector<std::string> desc_dic, std::vector<std::string> cat_dic) {
  std::map<std::string, std::string > dic;
  std::vector< std::string > result(desc_original.size());
  for(int i = 0; i < desc_dic.size(); i++){
    dic[desc_dic[i]] = cat_dic[i];
  }
  for(int i = 0; i < desc_original.size(); i++){
    result[i] = dic[desc_original[i]];
  }
  return result;
}

// //[[Rcpp::export]]
// IntegerMatrix padding_list_of_vectors(map<int, std::vector<int> > dic_p,
//                                       unsigned& max_size,
//                                       IntegerVector& chave_pessoas_unicas){
//   IntegerMatrix result(dic_p.size(),max_size);
//   int tmp;
//   int size_v;
//   for(int i =0; i < chave_pessoas_unicas.size(); i++){
//     tmp = chave_pessoas_unicas[i];
//     size_v = dic_p[tmp].size();
//     for(int j = 0; j <size_v ;j++){
//       result(i,j) = dic_p[tmp][j];
//     }
//   }
//   return result;
// }

//[[Rcpp::export]]
IntegerMatrix procedimento_to_matrix(IntegerVector& chave_pessoa, IntegerVector& procedimento,
                                     IntegerVector& chave_pessoas_unicas, int limitador){
  
  std::map<int, std::vector<int> > dic_pessoas;
  for(int i = 0; i< chave_pessoa.size();i++){
    dic_pessoas[chave_pessoa[i]].push_back(procedimento[i]);
  }
  
  if(limitador > 0){
    IntegerMatrix result(dic_pessoas.size(),limitador);
    for(int i =0; i < chave_pessoas_unicas.size(); i++){
      int tmp_min;
      int v_size = dic_pessoas[chave_pessoas_unicas[i]].size();
      tmp_min = std::min(limitador,v_size);
      for(int j = 0; j < tmp_min ;j++){
        result(i,j) = dic_pessoas[chave_pessoas_unicas[i]][j];
      }
    }
    return result;
  }else{
    std::vector<unsigned> tamanhos(chave_pessoas_unicas.size());
    for(int i = 0; i< chave_pessoas_unicas.size();i++){
      tamanhos[i] = dic_pessoas[chave_pessoas_unicas[i]].size();
    }
    unsigned max_size = *max_element(tamanhos.begin(),tamanhos.end());
    std::cout<<"Tamanho do maior vetor " << max_size <<"\n" << std::flush;
    IntegerMatrix result(dic_pessoas.size(),max_size);
    
    for(int i =0; i < chave_pessoas_unicas.size(); i++){
      for(int j = 0; j < dic_pessoas[chave_pessoas_unicas[i]].size() ;j++){
        result(i,j) = dic_pessoas[chave_pessoas_unicas[i]][j];
      }
    }
    return result;
  }
}


int pos_char(std::vector<std::string>& vetor, std::string s){
  auto it = std::find(vetor.begin(), vetor.end(), s);
  
  int result = std::distance(vetor.begin(), it);
  return result;
}
//[[Rcpp::export]]
NumericMatrix transition_matrix(std::vector<std::string>& procedimentos,
                                std::vector<std::string>& label){
  // Declaracao de variaveis
  NumericMatrix result(label.size(), label.size());
  std::vector<int> row_sum(label.size());
  
  // Fazendo as frequencias
  for(int i = 0; i < (procedimentos.size()-1); i++){
    result(pos_char(label, procedimentos[i]), pos_char(label, procedimentos[i+1])) += 1;
  }
  
  // Calculando a soma das linhas e dividindo
  for(int i = 0; i < label.size(); i++){
    for(int j = 0; j < label.size(); j++){
      row_sum[i] += result(i,j);
    }
    for(int j = 0; j < label.size(); j++){
      result(i,j) = result(i,j) / row_sum[i];
    }
  }
  
  return result;
  
}


//[[Rcpp::export]]
void depara_tree_check(CharacterMatrix& tree_procedimentos,
                       CharacterMatrix& preditos){
  
  std::unordered_map<std::string, std::string> PriDesc_to_Cap;
  std::unordered_map<std::string, std::string> SegDesc_to_Pri;
  std::unordered_map<std::string, std::string> DescPro_to_SegDesc;

  std::string tmp1;
  std::string tmp2;
  std::string tmp3;
  std::string tmp4;
  
  for(int i = 0; i < tree_procedimentos.nrow(); i++){
    tmp1 = tree_procedimentos(i,0);
    tmp2 = tree_procedimentos(i,1);
    tmp3 = tree_procedimentos(i,2);
    tmp4 = tree_procedimentos(i,3);
    PriDesc_to_Cap[tmp2] = tmp1;
    SegDesc_to_Pri[tmp3] = tmp2;
    DescPro_to_SegDesc[tmp4] = tmp3;
  }
  for(int i = 0; i < preditos.nrow(); i++){
    tmp1 = preditos(i,0);
    tmp2 = preditos(i,1);
    tmp3 = preditos(i,2);
    tmp4 = preditos(i,3);

    if(PriDesc_to_Cap[tmp2] != tmp1){
      preditos(i,3) = "NA";
      preditos(i,2) = "NA";
      preditos(i,1) = "NA";
    }
    if(SegDesc_to_Pri[tmp3] != tmp2){
      preditos(i,3) = "NA";
      preditos(i,2) = "NA";
    }
    if(DescPro_to_SegDesc[tmp4] != tmp3){
      preditos(i,3) = "NA";
    }
  }
}

//[[Rcpp::export]]
NumericVector criador_binario(std::vector<int>& x,
                              int& proc,
                              bool cid){
  NumericVector result(x.size());
  if(!cid){
    for(int i = 0; i < x.size(); i++){
      if(x[i] == proc){
        result[i] = 1;
      }
    }
  }else{
    for(int i = 0; i < x.size(); i++){
      if(x[i] == proc){
        for(int j = 0; j < x.size(); j++){
          result[i] = 1;
        }
        break;
      }
    }
  }
  return result;
}


