#limpio la memoria
rm(list=ls())
gc()
options(scipen=999)


# Load required packages
require("data.table")
require("xgboost")

exp <- '7' #bajando default_threshold

default_threshold <- 0.024


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

# Set working directory and load the old dataset
#setwd("~/buckets/b2/recuperatorio/uba2022/")
#dataset_generacion_viejo  <- fread( "rubinstein_generacion.txt.gz")

# Delete the specified columns
#dataset_generacion_viejo[, c("Master_delinquency", "Visa_delinquency") := NULL]

# Subsample "SI" values of the "clase" field
# n <- nrow(dataset_generacion_viejo)
# si_idx <- which(dataset_generacion_viejo$clase == "SI")
# sampled_si_idx <- sample(si_idx, size = round(0.0238 * n))
# dataset_generacion_viejo <- dataset_generacion_viejo[c(-(si_idx[-sampled_si_idx])), ]

# Add 15000000 to numero_de_cliente of dataset_generacion_viejo
#dataset_generacion_viejo[, numero_de_cliente := numero_de_cliente + 15000000] #para evitar duplicados...

# Set new working directory and load the new dataset
setwd("~/buckets/b2/recuperatorio_2/uba2022/")
dataset_generacion  <- fread("rubinstein-mayo_generacion.txt.gz")

# Concatenate old and new dataset
#dataset_generacion <- rbind(dataset_generacion_viejo, dataset_generacion)

#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]
dataset_generacion[, clase := NULL]


n <- c()
gains <- c()


for (split_seed in 1:10){
  
  test_ids <- fread(paste0("test_ids_",split_seed,".csv"),col.names="numero_de_cliente")
  test_data <- dataset_generacion[numero_de_cliente %in% test_ids$numero_de_cliente, ]
  train_data <- dataset_generacion[!numero_de_cliente %in% test_ids$numero_de_cliente, ]
  
  dtrain  <- xgb.DMatrix( data=  data.matrix( train_data[ , !c('numero_de_cliente','clase01'), with=FALSE]),
                          label= train_data[ , clase01 ]
  )
  
  #llamo al XGBoost,  notar lo frugal de los hiperparametros
  set.seed( 102191  ) #mi querida random seed, para que las corridas sean reproducibles # mi propia semilla 539141
  
  base_score <- mean(train_data[,clase01])
  scale_pos_weight_value <- sum(train_data$clase01 == 0) / sum(train_data$clase01 == 1)
  
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
  
  
  n <- append(n, nrow( prediccion_final[ prob_positivo > default_threshold  , "numero_de_cliente" ]  ) )
  gains <- append(gains, ganancia_total(test_data[ , clase01 ] , prediccion_final [,prob_positivo] ) )
}

prior_results <- read.csv("baseline_gains.csv")
colnames(prior_results) <- c("baseline_gain", "baseline_n", "split_seed")

# Prepare the new results data frame
results <- cbind(gains, n, split_seed = 1:10)
colnames(results) <- c('challenger_gain', 'challenger_n', 'split_seed')
results <- as.data.frame(results)

# Merge the two result data frames
results <- merge(prior_results, results, by = "split_seed")

# Add the win column
results$win <- with(results, ifelse(challenger_gain > baseline_gain, TRUE, ifelse(challenger_gain < baseline_gain, FALSE, NA)))

# Write the results back to the CSV file
write.csv(results, file = paste0("exp_",exp,"_gains.csv"), row.names=FALSE)
