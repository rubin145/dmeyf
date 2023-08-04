#limpio la memoria
rm(list=ls())
gc()
options(scipen=999)

t1 <- Sys.time()
print(format(t1, "%Y-%m-%d %H:%M:%S"))

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


alumno_apellido  <- "rubinstein"

ganancia_topn <- function(vector_of_test_values){
  true <- sum(vector_of_test_values == 1)
  false <- sum(vector_of_test_values == 0)
  gain <- true * 78000 + false * -2000
  return (gain)
}


#cargo los datasets
setwd( "~/buckets/b2/recuperatorio_2/uba2022/")

dataset_generacion  <- fread( paste0( alumno_apellido, "-mayo_generacion.txt.gz") )
#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]
dataset_generacion[, clase := NULL]


n <- c()
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
  prediccion_final  <- cbind(test_data[,"numero_de_cliente"], 
                                  aplicacion_prediccion,
                                  test_data[,"clase01"]
                             )
  colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo","clase01" )
  
  prediccion_final <- prediccion_final[order(prediccion_final$prob_positivo,decreasing=TRUE),]
  
  n <- round(0.205333333333333 * nrow(test_data))
  top_n_predictions <- prediccion_final[1:n,]
  thresholds <- append(thresholds, top_n_predictions[n,prob_positivo])
  gains <- append(gains, ganancia_topn(top_n_predictions[ , "clase01" ] ) )
}

results <- cbind(gains,thresholds, n, split_seed = 1:n_split_seeds)
colnames(results) <- c('gain','threshold', 'n', 'split_seed')

results <- as.data.frame(results)

write.csv(results, file =  paste0("baseline_gains_fixed_n",outfile_suff,".csv"), row.names=FALSE)


t2 <- Sys.time()
print(format(t2, "%Y-%m-%d %H:%M:%S"))

time_diff <- as.numeric(difftime(t2, t1, units="mins"))
print(paste0("Execution time: ", round(time_diff), " minutes"))
