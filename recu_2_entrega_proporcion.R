rm(list=ls())
gc()
options(scipen=999)


# Load required packages
require("data.table")

setwd("~/buckets/b2/recuperatorio_2/uba2022/")
dataset_aplicacion  <- fread("rubinstein-mayo_aplicacion.txt.gz")
entrega <- fread('rubinstein-mayo_lineademuerte_recuperatorio_entregar.txt',col.names="numero_de_cliente")
proporcion <- nrow(entrega)/nrow(dataset_aplicacion)
writeLines(as.character(proporcion), "proporcion_entrega.txt")
