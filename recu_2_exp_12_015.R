#limpio la memoria
rm(list=ls())
gc()
options(scipen=999)


# Load required packages
require("data.table")
require("xgboost")

exp <- '12_015' #probando muchos fraction distintos para fixed_n con el exp 2 (concatenando dataset viejo sin nada más)
split_fraction <- '015' #
n_split_seeds <- 100

fractions <- seq(0.17,0.24, by=0.001)

ganancia_topn <- function(vector_of_test_values){
  true <- sum(vector_of_test_values == 1)
  false <- sum(vector_of_test_values == 0)
  gain <- true * 78000 + false * -2000
  return (gain)
}

# Set working directory and load the old dataset
setwd("~/buckets/b2/recuperatorio/uba2022/")
dataset_generacion_viejo  <- fread( "rubinstein_generacion.txt.gz")

# Delete the specified columns
dataset_generacion_viejo[, c("Master_delinquency", "Visa_delinquency") := NULL]

# Add 15000000 to numero_de_cliente of dataset_generacion_viejo
dataset_generacion_viejo[, numero_de_cliente := numero_de_cliente + 15000000] #para evitar duplicados...

# Set new working directory and load the new dataset
setwd("~/buckets/b2/recuperatorio_2/uba2022/")
dataset_generacion  <- fread("rubinstein-mayo_generacion.txt.gz")

# Concatenate old and new dataset
dataset_generacion <- rbind(dataset_generacion_viejo, dataset_generacion)

#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]
dataset_generacion[, clase := NULL]


split_seed_results <- list()

for (split_seed in 1:n_split_seeds){
  gains <- c()
  thresholds <- c()
  
  test_ids <- fread(paste0("splits_",split_fraction,"/test_ids_",split_seed,".csv"),col.names="numero_de_cliente")
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
                       colsample_bytree= 0.6)
  
  #aplico a los datos de aplicacion, que NO TIENE CLASE
  dtest  <- xgb.DMatrix( data= data.matrix( test_data[ , !c('numero_de_cliente','clase01'), with=FALSE]) )
  
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, dtest )
  
  #uno las columnas de numero_de_cliente y la probabilidad recien calculada
  prediccion_final  <- cbind(  test_data[ ,"numero_de_cliente"], aplicacion_prediccion, test_data[ ,"clase01"] )
  colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo","clase01" )
  
  
  prediccion_final <- prediccion_final[order(prediccion_final$prob_positivo,decreasing=TRUE),]
  
  for (fraction in fractions){
    n_fixed <- round(fraction * nrow(test_data))
    top_n_predictions <- prediccion_final[1:n_fixed,]
    thresholds <- append(thresholds, top_n_predictions[n_fixed,prob_positivo] )
    gains <- append(gains, ganancia_topn(top_n_predictions[ , "clase01" ] ) )
  }
  split_seed_results[[split_seed]] <- list(gains = gains, thresholds = thresholds)
}
split_seed_dfs <- list()

prior_results_fixed_threshold <- read.csv("baseline_gains_fixed_threshold.csv")
colnames(prior_results_fixed_threshold) <- c("baseline_gain_ft", "baseline_n_ft", "split_seed")

prior_results_fixed_n <- read.csv("baseline_gains_fixed_n.csv")
colnames(prior_results_fixed_n) <- c("baseline_gain_fn",'baseline_threshold_fn', "baseline_n_fn", "split_seed")


for (split_seed in 1:n_split_seeds){
  
  # Get the baseline gains for this split_seed
  baseline_gain_ft <- prior_results_fixed_threshold[prior_results_fixed_threshold$split_seed == split_seed, "baseline_gain_ft"]
  baseline_n_ft <- prior_results_fixed_threshold[prior_results_fixed_threshold$split_seed == split_seed, "baseline_n_ft"]
  
  baseline_gain_fn <- prior_results_fixed_n[prior_results_fixed_n$split_seed == split_seed, "baseline_gain_fn"]
  baseline_n_fn <- prior_results_fixed_n[prior_results_fixed_n$split_seed == split_seed, "baseline_n_fn"]
  baseline_threshold_fn <- prior_results_fixed_n[prior_results_fixed_n$split_seed == split_seed, "baseline_threshold_fn"]
  # Get the computed gains and n for this split_seed
  gains <- split_seed_results[[split_seed]]$gains
  thresholds <- split_seed_results[[split_seed]]$thresholds
  
  # Create a data frame
  results <- data.frame(
    baseline_gain_ft = baseline_gain_ft, 
    baseline_n_ft = baseline_n_ft,
    baseline_gain_fn = baseline_gain_fn, 
    baseline_n_fn = baseline_n_fn,
    baseline_threshold_fn = baseline_threshold_fn,
    challenger_gain = gains, 
    challenger_threshold = thresholds, 
    split_seed = split_seed,
    fraction = fractions # ns value for this split_seed
  )
  
  # Add the win and diff columns
  results$win_ft <- with(results, ifelse(challenger_gain > baseline_gain_ft, TRUE, ifelse(challenger_gain < baseline_gain_ft, FALSE, NA)))
  results$diff_ft <- with(results, challenger_gain - baseline_gain_ft)
  results$win_fn <- with(results, ifelse(challenger_gain > baseline_gain_fn, TRUE, ifelse(challenger_gain < baseline_gain_fn, FALSE, NA)))
  results$diff_fn <- with(results, challenger_gain - baseline_gain_fn)
  
  # Store the data frame in the list
  split_seed_dfs[[split_seed]] <- results
}

fixed_ns_agg_results <- data.frame()

# Loop through each threshold
for (fraction in fractions) {
  
  # Initialize a counter for the sum of wins
  sum_of_wins_fn <- 0
  sum_of_wins_ft <- 0
  sum_of_diff_fn <- 0
  sum_of_diff_ft <- 0
  
  # Loop through each split_seed data frame
  for (split_seed_df in split_seed_dfs) {
    
    # Get the win values for this threshold and sum them
    win_ft_values <- split_seed_df[split_seed_df$fraction == fraction, "win_ft"]
    win_fn_values <- split_seed_df[split_seed_df$fraction == fraction, "win_fn"]
    diff_ft_values <- split_seed_df[split_seed_df$fraction == fraction, "diff_ft"]
    diff_fn_values <- split_seed_df[split_seed_df$fraction == fraction, "diff_fn"]
    sum_of_wins_ft <- sum_of_wins_ft + sum(win_ft_values, na.rm = TRUE)
    sum_of_wins_fn <- sum_of_wins_fn + sum(win_fn_values, na.rm = TRUE)
    sum_of_diff_ft <- sum_of_diff_ft + sum(diff_ft_values, na.rm = TRUE)
    sum_of_diff_fn <- sum_of_diff_fn + sum(diff_fn_values, na.rm = TRUE)
  }
  
  # Add the results for this threshold to the results data frame
  fixed_ns_agg_results <- rbind(fixed_ns_agg_results, data.frame(fraction = fraction, sum_of_wins_fn = sum_of_wins_fn, sum_of_wins_ft = sum_of_wins_ft, diff_fn = sum_of_diff_fn, diff_ft = sum_of_diff_ft))
}


write.csv(fixed_ns_agg_results, file = paste0("exp_",exp,"_gains",".csv"), row.names=FALSE)