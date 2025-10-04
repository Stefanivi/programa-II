library(readxl)
library(dplyr)
library(stringr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(readr)

# Es buena practica comenzar por limpiar el entorno de trabajo, cerrar
# cualquier gráfico abierto y limpiar la consola
graphics.off()
rm(list=ls())
cat("\014")
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/")
#Carga de bases de datos
?read_excel
radiobases <- read_excel("./input/Radiobases.xlsx",sheet = "2023")
telefonia_fija <- read_excel("./input/Telefonía fija.xlsx")


#Radiobases
head(radiobases)

#quitar la primera y segunda fila de NA
radiobases <- radiobases[-c(1,2),]
#Renombrar columnas
colnames(radiobases) <- c("departamento","municipio","radiobases")
head(radiobases)
#Eliminar  columnas con NA despues de radiobases
radiobases <- radiobases %>% select(departamento, municipio, radiobases)
head(radiobases)

#Sumar la columna de radiobases por departamento 
radiobases <- radiobases %>%
  group_by(departamento) %>%
  summarise(radiobases = sum(as.numeric(radiobases), na.rm = TRUE))
head(radiobases)
#Poner solo el nombre total en vez de NA de radiobases en la ultima fila de la tabla radiobases
radiobases[nrow(radiobases),1] <- "Total"

#Lineas fijas telefonicas
head(telefonia_fija)

# Hacer todas las variables del mismo




#Hacerlo como tabla, arriba están los años y para abajo los departamentos


