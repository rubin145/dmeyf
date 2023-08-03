#limpio la memoria
rm(list=ls())
gc()
options(scipen=999)

require("data.table")
require("xgboost")

split_fraction <- '02'
n_split_seeds <- 100


if (split_fraction != ''){
  split_fraction_path <- paste0('splits_',split_fraction,'/')
  outfile_suff <- paste0('_',split_fraction)
} else {
  split_fraction_path <- ''
  outfile_suff <- ''
}

default_threshold <- 0.025

alumno_apellido  <- "rubinstein"

ganancia_puntual <- function(test_record,pred_record,threshold=default_threshold){
  if (pred_record > threshold) {
    if (test_record == 1){
      return(78000)
    } else {
      return(-2000)
    }
  } else {
    return(0)
  }
}

ganancia_total <- function(test_array,pred_array, threshold=default_threshold){
  ganancias <- c()
  pares <- cbind(test_array,pred_array)
  colnames( pares )  <- c( "test", "pred" )
  for ( index in  1:nrow(pares)) {
    ganancias <- append(ganancias,  ganancia_puntual(pares[index,'test'],pares[index,'pred'],threshold=threshold) )
  }
  return(sum(ganancias))
}




#cargo los datasets
setwd( "~/buckets/b2/recuperatorio_2/uba2022/")

dataset_generacion  <- fread( paste0( alumno_apellido, "-mayo_generacion.txt.gz") )
#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]
dataset_generacion[, clase := NULL]


n <- c()
gains <- c()


for (split_seed in 1:n_split_seeds){
    
  test_ids <- fread(paste0(split_fraction_path,"test_ids_",split_seed,".csv"),col.names="numero_de_cliente")
  test_data <- dataset_generacion[numero_de_cliente %in% test_ids$numero_de_cliente, ]
  train_data <- dataset_generacion[!numero_de_cliente %in% test_ids$numero_de_cliente, ]
  
  dtrain  <- xgb.DMatrix( data=  data.matrix( train_data[ , !c('numero_de_cliente','clase01'), with=FALSE]),
                          label= train_data[ , clase01 ]
  )
  
  #llamo al XGBoost,  notar lo frugal de los hiperparametros
  set.seed( 102191  ) #mi querida random seed, para que las corridas sean reproducibles # mi propia semilla 539141
  
  base_score <- mean(train_data[,clase01])
  
  modelo  <- xgb.train(data= dtrain,
                       objective= "binary:logistic",
                       tree_method= "hist",
                       max_bin= 31,
                       base_score= base_score,
                       eta= 0.04,
                       nrounds= 300,
                       colsample_bytree= 0.6 )
  
  #aplico a los datos de aplicacion, que NO TIENE CLASE
  dtest  <- xgb.DMatrix( data= data.matrix( test_data[ , !c('numero_de_cliente','clase01'), with=FALSE]) )
  
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, dtest )
  
  #uno las columnas de numero_de_cliente y la probabilidad recien calculada
  prediccion_final  <- cbind(  test_data[ ,"numero_de_cliente"], aplicacion_prediccion )
  colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo" )
  
  
  n <- append(n, nrow( prediccion_final[ prob_positivo > 0.025  , "numero_de_cliente" ]  ) )
  gains <- append(gains, ganancia_total(test_data[ , clase01 ] , prediccion_final [,prob_positivo] ) )
}

results <- cbind(gains,n, split_seed = 1:n_split_seeds)
colnames(results) <- c('gain', 'n', 'split_seed')

results <- as.data.frame(results)

write.csv(results, file = paste0("baseline_gains_fixed_threshold",outfile_suff,".csv"), row.names=FALSE)
