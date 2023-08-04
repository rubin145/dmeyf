#!/usr/bin/env Rscript

#limpio la memoria
rm(list=ls())
gc()
options(scipen=999)

t1 <- Sys.time()
print(format(t1, "%Y-%m-%d %H:%M:%S"))

library("argparser")
library("data.table")
library("xgboost")

p <- arg_parser("CalcBaselines")

p <- add_argument(p, "--split_fraction", default = '02', help = "The split fraction size: 015/02/03/05")
p <- add_argument(p, "--n_split_seeds", default = 100, type='integer', help = "Number of split seeds.")
p <- add_argument(p, "--fixed", default = "threshold", help = "threshold or n (top N probas)")
p <- add_argument(p, "--threshold", default = 0.025, type = "float", help = "only with fixed = threshold")
p <- add_argument(p, "--n_target", default = 0.205333333333333, type = "float", help = "only with fixed = n")
p <- add_argument(p, "--seed", default = 102191, type='integer', help = "Seed for model.")
argv <- parse_args(p)

split_fraction <- argv$split_fraction
n_split_seeds <- argv$n_split_seeds
fixed <- argv$fixed
default_threshold <- argv$threshold
n_target <- argv$n_target
seed <- argv$seed


print(split_fraction)
print(n_split_seeds)
print(fixed)

print(paste0("starting_exp00 with | split_fraction ",split_fraction, " | n_split_seeds ", n_split_seeds, " | fixed ", fixed))


if (split_fraction != ''){
  split_fraction_path <- paste0('splits_',split_fraction,'/')
  outfile_suff <- paste0('_',split_fraction)
} else {
  split_fraction_path <- ''
  outfile_suff <- ''
}


alumno_apellido  <- "rubinstein"

ganancia_topn <- function(vector_of_test_values){
  true <- sum(vector_of_test_values == 1)
  false <- sum(vector_of_test_values == 0)
  gain <- true * 78000 + false * -2000
  return (gain)
}

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


n <- c() #mejor pre-alocar memoria con numeric(n_split_seeds)
gains <- c()
thresholds <- c()

for (split_seed in 1:n_split_seeds){
  
  test_ids <- fread(paste0(split_fraction_path,"test_ids_",split_seed,".csv"),col.names="numero_de_cliente")
  test_data <- dataset_generacion[numero_de_cliente %in% test_ids$numero_de_cliente, ]
  train_data <- dataset_generacion[!numero_de_cliente %in% test_ids$numero_de_cliente, ]
  
  dtrain  <- xgb.DMatrix( data=  data.matrix( train_data[ , !c('numero_de_cliente','clase01'), with=FALSE]),
                          label= train_data[ , clase01 ]
  )
  
  #llamo al XGBoost,  notar lo frugal de los hiperparametros
  set.seed( seed  ) #mi querida random seed, para que las corridas sean reproducibles # mi propia semilla 539141
  
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
  prediccion_final  <- cbind(test_data[,"numero_de_cliente"], 
                             aplicacion_prediccion,
                             test_data[,"clase01"]
  )
  colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo","clase01" )
  
  if (fixed == "n"){
    prediccion_final <- prediccion_final[order(prediccion_final$prob_positivo,decreasing=TRUE),]
    
    n <- round(n_target * nrow(test_data))
    top_n_predictions <- prediccion_final[1:n,]
    thresholds <- append(thresholds, top_n_predictions[n,prob_positivo])
    gains <- append(gains, ganancia_topn(top_n_predictions[ , "clase01" ] ) )
  } else if (fixed == "threshold"){
    n <- append(n, nrow( prediccion_final[ prob_positivo > default_threshold  , "numero_de_cliente" ]  ) )
    gains <- append(gains, ganancia_total(test_data[ , clase01 ] , prediccion_final [,prob_positivo] ) )
    thresholds <- append(thresholds,default_threshold)
  }
}

results <- cbind(gains,thresholds, n, split_seed = 1:n_split_seeds)
colnames(results) <- c('gain','threshold', 'n', 'split_seed')

results <- as.data.frame(results)

write.csv(results, file =  paste0("baseline_gains_fixed_",fixed, outfile_suff,".csv"), row.names=FALSE)


t2 <- Sys.time()
print(format(t2, "%Y-%m-%d %H:%M:%S"))

time_diff <- as.numeric(difftime(t2, t1, units="mins"))
print(paste0("Execution time: ", round(time_diff), " minutes"))
