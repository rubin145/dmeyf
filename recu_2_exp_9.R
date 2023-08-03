#limpio la memoria
rm(list=ls())
gc()
options(scipen=999)


# Load required packages
require("data.table")
require("xgboost")

exp <- '9' #probando muchos thresholds da 

default_threshold <- NA

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


split_seed_results <- list()
thresholds <-  seq(0.0220, 0.028, by = 0.0001)

for (split_seed in 1:10){
  gains <- c()
  n <- c()
  
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
  
  for (threshold in thresholds){
    n <- append(n, nrow( prediccion_final[ prob_positivo > threshold  , "numero_de_cliente" ]  ) )
    gains <- append(gains, ganancia_total(test_data[ , clase01 ] , prediccion_final [,prob_positivo], threshold=threshold ) )
  }
  split_seed_results[[split_seed]] <- list(gains = gains, n = n)
}
split_seed_dfs <- list()

prior_results <- read.csv("baseline_gains.csv")
colnames(prior_results) <- c("baseline_gain", "baseline_n", "split_seed")


for (split_seed in 1:10){
  
  # Get the baseline gains for this split_seed
  baseline_gain <- prior_results[prior_results$split_seed == split_seed, "baseline_gain"]
  baseline_n <- prior_results[prior_results$split_seed == split_seed, "baseline_n"]
  
  # Get the computed gains and n for this split_seed
  gains <- split_seed_results[[split_seed]]$gains
  n <- split_seed_results[[split_seed]]$n
  
  # Create a data frame
  results <- data.frame(baseline_gain = baseline_gain, 
                        baseline_n = baseline_n,
                        challenger_gain = gains, 
                        challenger_n = n, 
                        split_seed = split_seed)
  
  # Add the win column
  results$win <- with(results, ifelse(challenger_gain > baseline_gain, TRUE, ifelse(challenger_gain < baseline_gain, FALSE, NA)))
  
  # Store the data frame in the list
  split_seed_dfs[[split_seed]] <- results
}

for (split_seed in 1:10){
  split_seed_dfs[[split_seed]]$threshold <- thresholds
}

results_1 <- split_seed_dfs[[1]]
results_2 <- split_seed_dfs[[2]]
results_3 <- split_seed_dfs[[3]]
results_4 <- split_seed_dfs[[4]]
results_5 <- split_seed_dfs[[5]]
results_6 <- split_seed_dfs[[6]]
results_7 <- split_seed_dfs[[7]]
results_8 <- split_seed_dfs[[8]]
results_9 <- split_seed_dfs[[9]]
results_10 <- split_seed_dfs[[10]]

threshold_agg_results <- data.frame()

# Loop through each threshold
for (threshold in thresholds) {
  
  # Initialize a counter for the sum of wins
  sum_of_wins <- 0
  
  # Loop through each split_seed data frame
  for (split_seed_df in split_seed_dfs) {
    
    # Get the win values for this threshold and sum them
    win_values <- split_seed_df[split_seed_df$threshold == threshold, "win"]
    sum_of_wins <- sum_of_wins + sum(win_values, na.rm = TRUE)
  }
  
  # Add the results for this threshold to the results data frame
  threshold_agg_results <- rbind(threshold_agg_results, data.frame(threshold = threshold, sum_of_wins = sum_of_wins))
}


# for (split_seed in 1:10){
#   write.csv(split_seed_dfs[[split_seed]], file = paste0("exp_",exp,"_gains_",split_seed,".csv"), row.names=FALSE)
# }
write.csv(threshold_agg_results, file = paste0("exp_",exp,"_gains",".csv"), row.names=FALSE)