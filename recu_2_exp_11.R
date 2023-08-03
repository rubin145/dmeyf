#limpio la memoria
rm(list=ls())
gc()
options(scipen=999)


# Load required packages
require("data.table")
require("xgboost")

exp <- '11' #entrenar el baseline cambiando función objetivo (y con custom eval func aunque no hace nada)

default_threshold <- 0.025


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
gains_fixed_threshold <- c()
gains_fixed_n <- c()
thresholds <- c()


for (split_seed in 1:10){
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
  
  ganancia_topn <- function(vector_of_test_values){
    true <- sum(vector_of_test_values == 1)
    false <- sum(vector_of_test_values == 0)
    gain <- true * 78000 + false * -2000
    return (gain)
  }
  
  custom_eval <- function(pred_probs, dtrain) {
    labels <- getinfo(dtrain, "label")
    # Calculate the gain using your function on the predictions and the true labels
    total_gain <- ganancia_total(labels, pred_probs)
    # Return the name of the evaluation and the score
    return(list(metric = "total_gain", value = total_gain))
  }
  
  # Custom objective function
  gain_objective <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    preds <- 1/(1 + exp(-preds))  # Convert from log odds to probability
    gain <- ifelse(preds > default_threshold, ifelse(labels == 1, 78000, -2000), 0)
    grad <- gain  # Gradient
    hess <- gain * (1 - gain)  # Hessian (second derivative)
    return(list(grad = grad, hess = hess))
  }
  
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
                       objective= gain_objective,
                       tree_method= "hist",
                       max_bin= 31,
                       base_score= base_score,
                       eta= 0.04,
                       nrounds= 300,
                       feval = custom_eval,
                       colsample_bytree= 0.6 )
  
  #aplico a los datos de aplicacion, que NO TIENE CLASE
  dtest  <- xgb.DMatrix( data= data.matrix( test_data[ , !c('numero_de_cliente','clase01'), with=FALSE]) )
  
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, dtest )
  
  #uno las columnas de numero_de_cliente y la probabilidad recien calculada
  prediccion_final  <- cbind(  test_data[ ,"numero_de_cliente"], aplicacion_prediccion, test_data[,"clase01"] )
  colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo", "clase01" )
  
  n <- append(n, nrow( prediccion_final[ prob_positivo > 0.025  , "numero_de_cliente" ]  ) )
  gains_fixed_threshold <- append(gains_fixed_threshold, ganancia_total(prediccion_final[ , clase01 ] , prediccion_final [,prob_positivo] ) )
  
  prediccion_final <- prediccion_final[order(prediccion_final$prob_positivo,decreasing=TRUE),]
  
  n_fixed <- round(0.205333333333333 * nrow(test_data))
  top_n_predictions <- prediccion_final[1:n_fixed,]
  thresholds <- append(thresholds, top_n_predictions[n_fixed,prob_positivo])
  gains_fixed_n <- append(gains_fixed_n, ganancia_topn(top_n_predictions[ , "clase01" ] ) )
}

prior_results_fixed_threshold <- read.csv("baseline_gains_fixed_threshold.csv")
colnames(prior_results_fixed_threshold) <- c("baseline_gain_ft", "baseline_n_ft", "split_seed")

prior_results_fixed_n <- read.csv("baseline_gains_fixed_n.csv")
colnames(prior_results_fixed_n) <- c("baseline_gain_fn",'baseline_threshold_fn', "baseline_n_fn", "split_seed")
prior_results_fixed_n$baseline_n_fn <- NULL


results_fixed_threshold <- cbind(gains_fixed_threshold, n, split_seed = 1:10)
colnames(results_fixed_threshold) <- c('challenger_gain_ft', 'challenger_n_ft', 'split_seed')
results_fixed_threshold <- as.data.frame(results_fixed_threshold)

results_fixed_n <- cbind(gains_fixed_n, thresholds, split_seed = 1:10)
colnames(results_fixed_n) <- c('challenger_gain_fn', 'challenger_threshold_fn', 'split_seed')
results_fixed_n <- as.data.frame(results_fixed_n)


results_fixed_threshold <- merge(prior_results_fixed_threshold, results_fixed_threshold, by = "split_seed")
results_fixed_threshold$win_ft <- with(results_fixed_threshold, ifelse(challenger_gain_ft > baseline_gain_ft, TRUE, ifelse(challenger_gain_ft < baseline_gain_ft, FALSE, NA)))

results_fixed_n <- merge(prior_results_fixed_n, results_fixed_n, by = "split_seed")
results_fixed_n$win_fn <- with(results_fixed_n, ifelse(challenger_gain_fn > baseline_gain_fn, TRUE, ifelse(challenger_gain_fn < baseline_gain_fn, FALSE, NA)))

results <- merge(results_fixed_threshold, results_fixed_n, by = "split_seed")

# Write the results back to the CSV file
write.csv(results, file = paste0("exp_",exp,"_gains.csv"), row.names=FALSE)