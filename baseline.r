#limpio la memoria
rm(list=ls())
gc()

require("data.table")
require("xgboost")

#model_seeds <- c(10209,53813,38053,19301,74923,86029,87613,87589,77419,44971,
#                 74093,54409,28019,65413,77491,49433,28297,89189,99023,14009)
model_seeds <- c(10209,102191,1:98)


alumno_apellido  <- "rubinstein"

#cargo los datasets
setwd( "~/buckets/b2/recuperatorio_2/uba2022/")

dataset_generacion  <- fread( paste0( alumno_apellido, "-mayo_generacion.txt.gz") )
dataset_aplicacion  <- fread( paste0( alumno_apellido, "-mayo_aplicacion.txt.gz") )


#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]

dgeneracion  <- xgb.DMatrix( data=  data.matrix( dataset_generacion[ , !c("numero_de_cliente","clase","clase01"), with=FALSE]),
                             label= dataset_generacion[ , clase01 ]
)

all_probas <- c()
for (model_seed in model_seeds){
#llamo al XGBoost,  notar lo frugal de los hiperparametros
  set.seed( model_seed ) #mi querida random seed, para que las corridas sean reproducibles
  
  modelo  <- xgb.train(data= dgeneracion,
                       objective= "binary:logistic",
                       tree_method= "hist",
                       max_bin= 31,
                       base_score= mean( getinfo(dgeneracion, "label") ),
                       eta= 0.04,
                       nrounds= 300,
                       colsample_bytree= 0.6 )
  
  #aplico a los datos de aplicacion, que NO TIENE CLASE
  daplicacion  <- xgb.DMatrix( data= data.matrix( dataset_aplicacion[ , !c("numero_de_cliente"), with=FALSE]) )
  
  
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, daplicacion )
  
  #uno las columnas de numero_de_cliente y la probabilidad recien calculada
  prediccion_parcial  <- cbind(  dataset_aplicacion[ ,c("numero_de_cliente")], aplicacion_prediccion )
  colnames( prediccion_parcial )  <- c( "numero_de_cliente", "prob_positivo" )
  
  all_probas <- cbind(all_probas,prediccion_parcial [,prob_positivo])
  
  print(paste0("listo con seed ",model_seed))
}
prediccion_final <- cbind(dataset_aplicacion[ ,c("numero_de_cliente")], rowMeans(all_probas))
colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo" )

#grabo todas las probabilidad, simplemente para tenerlo
fwrite( prediccion_final[ order( -prob_positivo) ], 
        file= paste0( alumno_apellido, "_PROBAS.txt"), 
        sep= "\t", 
        eol= "\r\n")

#Ahora grabo la salida que debo entregar en la materia, que son solamente los ids
#me quedo solamente con los numero_de_cliente donde probabilidad > 0.025
fwrite( as.data.table( prediccion_final[ prob_positivo > 0.025  , "numero_de_cliente" ] ), 
        file= paste0( alumno_apellido, "_ENTREGA.txt"),
        col.names=FALSE, 
        sep= "\t", 
        eol= "\r\n")


batch <- prediccion_final[ order( -prob_positivo) ]


fwrite( as.data.table( batch[1:51324,numero_de_cliente] ), 
        file= paste0( alumno_apellido, "_ENTREGA_batch.txt"),
        col.names=FALSE, 
        sep= "\t", 
        eol= "\r\n")