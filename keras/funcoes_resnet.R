require(keras)
stack_residual <- function(x, nb_channels, strides.){
  # Esta função performa um stack de mesma topologia em cima de uma camada X
  conv1 <- x %>% 
    layer_conv_1d(filters = nb_channels,kernel_size = c(3), strides = strides., padding = "same")
  conv2 <- x %>% 
    layer_conv_1d(filters = nb_channels,kernel_size = c(3), strides = strides., padding = "same")
  conv3 <- x %>% 
    layer_conv_1d(filters = nb_channels,kernel_size = c(3), strides = strides., padding = "same")
  conv4 <- x %>% 
    layer_conv_1d(filters = nb_channels,kernel_size = c(3), strides = strides., padding = "same")
  conv <- layer_add(list(conv1, conv2, conv3, conv4))
  return(conv)
}
batch_leaky <- function(x){
  # Esta função aplica batch normalization e leaky relu numa camada X
  x <- x %>% 
    layer_batch_normalization() %>% 
    layer_activation_leaky_relu()
  return(x)
}
residual_block <- function(y,
                           nb_channels_in,
                           nb_channels_out,
                           strides.= c(1),
                           project_shortcut. = F){
  shortcut <- y
  
  y <- y %>% 
    layer_conv_1d(nb_channels_in, kernel_size=c(1), strides=c(1), padding='same') %>% 
    batch_leaky()
  
  y <- stack_residual(y,nb_channels = nb_channels_in,strides. = strides.) %>% 
    batch_leaky()
  
  y <- y %>% 
    layer_conv_1d(nb_channels_out, kernel_size=c(1), strides=c(1), padding='same') %>% 
    layer_batch_normalization()
  
  if(project_shortcut. | strides. != c(1)){
    shortcut <- shortcut %>% 
      layer_conv_1d(nb_channels_out, kernel_size=c(1), strides = strides., padding='same') %>% 
      layer_batch_normalization()
  }
  
  y <- layer_add(list(shortcut, y)) %>% 
    layer_activation_leaky_relu()
  return(y)
}

resnet <- function(input, nb_output,activation. = "softmax"){
  
  mod_input <- input %>% 
    layer_conv_1d(64, kernel_size=c(7), strides=c(2), padding='same') %>% 
    batch_leaky()
  mod_input <- mod_input %>% 
    layer_max_pooling_1d(pool_size = c(2), strides = c(2), padding='same')
  
  for(i in 1:4){
    if(i == 1){
      project <- T
    }else{
      project <- F
    }
    mod_input <- residual_block(mod_input, 64, 64, project_shortcut. = project)
  }
  for(i in 1:4){
    if(i == 1){
      strides. <- c(2)
    }else{
      strides. <- c(1)
    }
    mod_input <- residual_block(mod_input, 32, 64, strides. = strides.)
  }
  
  for(i in 1:4){
    if(i == 1){
      strides. <- c(1)
    }else{
      strides. <- c(1)
    }
    mod_input <- residual_block(mod_input, 64, 64, strides. = strides.)
  }
  
  for(i in 1:4){
    if(i == 1){
      strides. <- c(1)
    }else{
      strides. <- c(1)
    }
    mod_input <- residual_block(mod_input, 64, 64, strides. = strides.)
  }
  
  mod_input <- mod_input %>% 
    layer_global_average_pooling_1d()
  output <- mod_input %>% 
    layer_dense(nb_output, activation = activation.)
}

