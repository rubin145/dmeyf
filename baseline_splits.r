#limpio la memoria
rm(list=ls())
gc()
options(scipen=999)

require("data.table")
require("xgboost")
  
default_threshold <- 0.025

alumno_apellido  <- "rubinstein"

baseline_gains <- cbind(
  c(209546000,201854000,204614000,208726000,204620000,208068000,202092000,206650000,211596000,211638000),
  c(104936000,101136000,99702000,106068000,100428000,105552000,100252000,105478000,105476000,104786000)
)
colnames(baseline_gains) <- c("prop04_bl","prop02_bl")

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

props <-list('prop04'=0.4,'prop02'=0.2)
#gains <- list('prop04'=c(),'prop02'=c())
gains_04 <- c()
gains_02 <- c()
n_04 <- c()
n_02 <- c()
props_ <- list('prop04'='04prop_','prop02'='')

#cargo los datasets
setwd( "~/buckets/b2/recuperatorio/uba2022/")

dataset_generacion  <- fread( paste0( alumno_apellido, "_generacion.txt.gz") )
dataset_aplicacion  <- fread( paste0( alumno_apellido, "_aplicacion.txt.gz") )
#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]
dataset_generacion[, clase := NULL]

for (prop in c('prop04','prop02')){
  for (split_seed in 1:10){
  
    test_ids <- fread(paste0("test_ids_",props_[prop],split_seed,".csv"),col.names="numero_de_cliente")
    test_data <- dataset_generacion[numero_de_cliente %in% test_ids$numero_de_cliente, ]
    train_data <- dataset_generacion[!numero_de_cliente %in% test_ids$numero_de_cliente, ]
    
    dtrain  <- xgb.DMatrix( data=  data.matrix( train_data[ , !c("clase01"), with=FALSE]),
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
    dtest  <- xgb.DMatrix( data= data.matrix( test_data[ , !c('clase01'), with=FALSE]) )
    
    #aplico el modelo a datos nuevos
    aplicacion_prediccion  <- predict(  modelo, dtest )
    
    #uno las columnas de numero_de_cliente y la probabilidad recien calculada
    prediccion_final  <- cbind(  test_data[ ,"numero_de_cliente"], aplicacion_prediccion )
    colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo" )
    
    if (prop == 'prop02'){
      n_02 <- append(n_02, nrow( prediccion_final[ prob_positivo > 0.025  , "numero_de_cliente" ]  ) )
    } else {
      n_04 <- append(n_04, nrow( prediccion_final[ prob_positivo > 0.025  , "numero_de_cliente" ]  ) )
    }
    
    if (prop == 'prop02'){
      gains_02 <- append(gains_02, ganancia_total(test_data[ , clase01 ] , prediccion_final [,prob_positivo] ) )
    } else {
      gains_04 <- append(gains_04, ganancia_total(test_data[ , clase01 ] , prediccion_final [,prob_positivo] ) )
    }
    
  }
}
#results <- cbind(gains['prop04'],gains['prop02'])
results <- cbind(gains_04, gains_02,n_04,n_02)
colnames(results) <- c('gain_04', 'gain_02','n_04','n_02')

results <- cbind(results,baseline_gains)
results <- as.data.frame(results)
results$prop04_win <- ifelse(results$gain_04 > results$prop04_bl, TRUE, FALSE)
results$prop02_win <- ifelse(results$gain_02 > results$prop02_bl, TRUE, FALSE)

print(paste0("wins en prop02: ", sum(results$prop02_win)  ))
print(paste0("wins en prop04: ", sum(results$prop04_win)  ))

results$prop_04 <- round(results$n_04 / 42000 * 100,2)
results$prop_02 <- round(results$n_02 / 21000 * 100,2)