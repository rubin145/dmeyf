#limpio la memoria
rm(list=ls())
gc()

require("data.table")
require("xgboost")

fraction <- 0.###
model_seeds <- c(102191,1:49)
nro_entrega <- 01

setwd("~/buckets/b2/recuperatorio/uba2022/")
dataset_generacion_viejo  <- fread( "rubinstein_generacion.txt.gz")

#borrar columans que se fueron del recu 1 al recu 2
dataset_generacion_viejo[, c("Master_delinquency", "Visa_delinquency") := NULL]

# agregar 15000000 al numero_de_cliente para evitar duplicados... (esto era imprtante para los splits)
dataset_generacion_viejo[, numero_de_cliente := numero_de_cliente + 15000000]

# Set new working directory and load the new dataset
setwd("~/buckets/b2/recuperatorio_2/uba2022/")
dataset_generacion  <- fread("rubinstein-mayo_generacion.txt.gz")

# Concatenate old and new dataset
dataset_generacion <- rbind(dataset_generacion_viejo, dataset_generacion)

#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]
dataset_generacion[, clase := NULL]

dataset_aplicacion  <- fread("rubinstein-mayo_aplicacion.txt.gz")

dgeneracion  <- xgb.DMatrix( data=  data.matrix( dataset_generacion[ , !c("numero_de_cliente","clase01"), with=FALSE]),
                             label= dataset_generacion[ , clase01 ]
)

#llamo al XGBoost,  notar lo frugal de los hiperparametros
set.seed( 102191 ) #mi querida random seed, para que las corridas sean reproducibles

base_score <- mean(dataset_generacion[,clase01])
all_probas <- c()
for (model_seed in model_seeds){
  set.seed(model_seed)
  print(paste0("model_seed: ",model_seed))
  
  modelo  <- xgb.train(data= dgeneracion,
                       objective= "binary:logistic",
                       tree_method= "hist",
                       max_bin= 31,
                       base_score= base_score,
                       eta= 0.04,
                       nrounds= 300,
                       colsample_bytree= 0.6 )
  
  #aplico a los datos de aplicacion, que NO TIENE CLASE
  daplicacion  <- xgb.DMatrix( data= data.matrix( dataset_aplicacion[ , !c("numero_de_cliente"), with=FALSE]) )
  
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, daplicacion )
  
  all_probas <- cbind(all_probas,aplicacion_prediccion)
  
}

#uno las columnas de numero_de_cliente y la probabilidad recien calculada
prediccion_final  <- cbind(  dataset_aplicacion[ ,c("numero_de_cliente")], rowMeans(all_probas) )

#le doy nombre a las columnas
colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo" )

prediccion_final <- prediccion_final[order(prediccion_final$prob_positivo,decreasing=TRUE),]
n_fixed <- round(fraction * nrow(dataset_aplicacion))
top_n_predictions <- prediccion_final[1:n_fixed,]
threshold <- top_n_predictions[n_fixed,prob_positivo]


#Genero las TRES salidas
#grabo todas las probabilidad, simplemente para tenerlo
fwrite( prediccion_final, 
        file= paste0("entrega/rubinstein-mayo_recuperatorio_probas_",nro_entrega,".txt"), 
        sep= "\t", 
        eol= "\r\n")

#Ahora grabo la salida que debo entregar en la materia, que son solamente los ids
#me quedo solamente con los numero_de_cliente donde probabilidad > 0.025
fwrite( top_n_predictions[,'numero_de_cliente'], 
        file= paste0("entrega/rubinstein-mayo_recuperatorio_entrega_",nro_entrega,".txt"),
        col.names=FALSE, 
        sep= "\t", 
        eol= "\r\n")

writeLines(as.character(threshold), paste0("entrega/rubinstein-mayo_threshold_entrega_",nro_entrega,".txt"))

#grabo la importancia de las variables
write.table(  xgb.importance( model = modelo )
              , file= paste0("entrega/rubinstein-mayo_recuperatorio_importancia_",nro_entrega,".txt")
, sep= "\t"
, eol= "\r\n"
, row.names= FALSE
)
