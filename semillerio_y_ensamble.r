#limpio la memoria
rm(list=ls())
gc()
options(scipen=999)

require("data.table")
require("xgboost")
require('lightgbm')

model_seeds <- c(10209,53813,38053,19301,74923,86029,87613,87589,77419,44971,74093,
                54409,28019,65413,77491,49433,28297,89189,99023,14009)
#model_seeds <- c(10209,1:99) ##reemplazar por 102191 #con este 10209 dio 8 en prop4

alumno_apellido  <- "rubinstein"

baseline_gains <- cbind(
  c(209546000,201854000,204614000,208726000,204620000,208068000,202092000,206650000,211596000,211638000),
  c(104936000,101136000,99702000,106068000,100428000,105552000,100252000,105478000,105476000,104786000)
)
colnames(baseline_gains) <- c("prop04_bl","prop02_bl")

ganancia_puntual <- function(test_record,pred_record,threshold=0.025){
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

ganancia_total <- function(test_array,pred_array, threshold=0.025){
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
#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]
dataset_generacion[, clase := NULL]

# dataset_generacion [, cpremiums := rowSums(.SD), .SDcols = c('cprestamos_personales','cprestamos_prendarios','cprestamos_hipotecarios','cplazo_fijo','cinversion1','cinversion2','cseguro_vida','cseguro_auto','cseguro_vivienda','cseguro_accidentes_personales','ccaja_seguridad')]
# dataset_generacion [,premiums_bool := cpremiums > 0 ]
# 
# dataset_generacion [, estables_tr := rowSums(.SD), .SDcols = c('ctarjeta_debito_transacciones','ctarjeta_visa_transacciones','ctarjeta_master_transacciones','cpayroll_trx','ctarjeta_master_debitos_automaticos','ctarjeta_visa_debitos_automaticos')]
# dataset_generacion [,estables_tr_bool := estables_tr > 0 ]
# 
# dataset_generacion [, VisaMaster_delinquency := rowSums(.SD), .SDcols = c('Visa_delinquency','Master_delinquency')]
# dataset_generacion [, VisaMaster_status := rowSums(.SD), .SDcols = c('Visa_status','Master_status')]
# dataset_generacion [, VisaMaster_mfinanciacion_limite := rowSums(.SD), .SDcols = c('Visa_mfinanciacion_limite','Master_mfinanciacion_limite')]
# dataset_generacion [, VisaMaster_msaldototal := rowSums(.SD), .SDcols = c('Visa_msaldototal','Master_msaldototal')]
# dataset_generacion [, VisaMaster_msaldopesos := rowSums(.SD), .SDcols = c('Visa_msaldopesos','Master_msaldopesos')]
# dataset_generacion [, VisaMaster_msaldodolares := rowSums(.SD), .SDcols = c('Visa_msaldodolares','Master_msaldodolares')]
# dataset_generacion [, VisaMaster_mconsumospesos := rowSums(.SD), .SDcols = c('Visa_mconsumospesos','Master_mconsumospesos')]
# dataset_generacion [, VisaMaster_mconsumosdolares := rowSums(.SD), .SDcols = c('Visa_mconsumosdolares','Master_mconsumosdolares')]
# dataset_generacion [, VisaMaster_mlimitecompra := rowSums(.SD), .SDcols = c('Visa_mlimitecompra','Master_mlimitecompra')]
# dataset_generacion [, VisaMaster_madelantopesos := rowSums(.SD), .SDcols = c('Visa_madelantopesos','Master_madelantopesos')]
# dataset_generacion [, VisaMaster_madelantodolares := rowSums(.SD), .SDcols = c('Visa_madelantodolares','Master_madelantodolares')]
# dataset_generacion [, VisaMaster_mpagado := rowSums(.SD), .SDcols = c('Visa_mpagado','Master_mpagado')]
# dataset_generacion [, VisaMaster_mpagospesos := rowSums(.SD), .SDcols = c('Visa_mpagospesos','Master_mpagospesos')]
# dataset_generacion [, VisaMaster_mpagosdolares := rowSums(.SD), .SDcols = c('Visa_mpagosdolares','Master_mpagosdolares')]
# dataset_generacion [, VisaMaster_mconsumototal := rowSums(.SD), .SDcols = c('Visa_mconsumototal','Master_mconsumototal')]
# #dataset_generacion [, VisaMaster_cconsumos := rowSums(.SD), .SDcols = c('Visa_cconsumos','Master_cconsumos')]
# dataset_generacion [, VisaMaster_cadelantosefectivo := rowSums(.SD), .SDcols = c('Visa_cadelantosefectivo','Master_cadelantosefectivo')]
# dataset_generacion [, VisaMaster_mpagominimo := rowSums(.SD), .SDcols = c('Visa_mpagominimo','Master_mpagominimo')]
# 
# dataset_generacion [, ctarjeta_VisaMaster_transacciones := rowSums(.SD), .SDcols = c('ctarjeta_visa_transacciones','ctarjeta_master_transacciones')]
# dataset_generacion [, mtarjeta_VisaMaster_consumo := rowSums(.SD), .SDcols = c('mtarjeta_visa_consumo','mtarjeta_master_consumo')]
# dataset_generacion [, ctarjeta_VisaMaster_debitos_automaticos := rowSums(.SD), .SDcols = c('ctarjeta_visa_debitos_automaticos','ctarjeta_master_debitos_automaticos')]
# dataset_generacion [, mttarjeta_VisaMaster_debitos_automaticos := rowSums(.SD), .SDcols = c('mttarjeta_visa_debitos_automaticos','mttarjeta_master_debitos_automaticos')]
# dataset_generacion [, ctarjeta_VisaMaster_descuentos := rowSums(.SD), .SDcols = c('ctarjeta_visa_descuentos','ctarjeta_master_descuentos')]
# dataset_generacion [, mtarjeta_VisaMaster_descuentos := rowSums(.SD), .SDcols = c('mtarjeta_visa_descuentos','mtarjeta_master_descuentos')]
# 
# dataset_generacion [, tpaquete3 := NULL]
# dataset_generacion [, tpaquete5 := NULL]
# dataset_generacion [, tpaquete8 := NULL]
# dataset_generacion [, ccaja_ahorro := NULL]
# dataset_generacion [, ctarjeta_master := NULL]

for (prop in c('prop04','prop02')){
  for (split_seed in 1:10){
    t1 = Sys.time()
    Sys.sleep(2)
    t2 =Sys.time()
    (t2-t1)
    all_probas <- c()
    
    
    test_ids <- fread(paste0("test_ids_",props_[prop],split_seed,".csv"),col.names="numero_de_cliente")
    test_data <- dataset_generacion[numero_de_cliente %in% test_ids$numero_de_cliente, ]
    train_data <- dataset_generacion[!numero_de_cliente %in% test_ids$numero_de_cliente, ]
    
    dtrain  <- xgb.DMatrix( data=  data.matrix( train_data[ , !c("numero_de_cliente","clase01"), with=FALSE]),
                            label= train_data[ , clase01 ]
    )
    
    #llamo al XGBoost,  notar lo frugal de los hiperparametros
    
    for (model_seed in model_seeds){
      set.seed(model_seed)
      print(paste0("split_seed: ",split_seed,"; model_seed: ",model_seed))
      
      modelo  <- xgb.train(data= dtrain,
                           objective= "binary:logistic",
                           tree_method= "hist",
                           max_bin= 31,
                           base_score= mean( getinfo(dtrain, "label") ),
                           eta= 0.04,
                           nrounds= 300,
                           colsample_bytree= 0.6 )
      
      #aplico a los datos de aplicacion, que NO TIENE CLASE
      dtest  <- xgb.DMatrix( data= data.matrix( test_data[ , !c("numero_de_cliente",'clase01'), with=FALSE]) )
      
      #aplico el modelo a datos nuevos
      aplicacion_prediccion  <- predict(  modelo, dtest )
      
      #uno las columnas de numero_de_cliente y la probabilidad recien calculada
      prediccion_final  <- cbind(  test_data[ ,"numero_de_cliente"], aplicacion_prediccion )
      colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo" )
      
      all_probas <- cbind(all_probas,prediccion_final [,prob_positivo])
      
    }
    
    dtrain  <- lgb.Dataset( data=  data.matrix( train_data[ , !c("numero_de_cliente","clase01"), with=FALSE]),
                            label= train_data[ , clase01 ]
    )
    
    modelo2 <- lgb.train( data= dtrain,
                          param= list(objective="binary",
                                       seed=10209)
    )
    
    dtest  <- data.matrix( test_data[ , !c("numero_de_cliente",'clase01'), with=FALSE])
    
    pred_lgbm  <- predict( modelo2, dtest ) 
    
    probas_final <- cbind(rowMeans(all_probas),pred_lgbm)
    
    if (prop == 'prop02'){
      n_02 <- append(n_02, sum(rowMeans(probas_final) > 0.025))
    } else {
      n_04 <- append(n_04, sum(rowMeans(probas_final) > 0.025))
    }
    
    
    
    if (prop == 'prop02'){
      gains_02 <- append(gains_02, ganancia_total(test_data[ , clase01 ] , rowMeans(probas_final) ) )
    } else {
      gains_04 <- append(gains_04, ganancia_total(test_data[ , clase01 ] , rowMeans(probas_final) ) )
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

print(results)