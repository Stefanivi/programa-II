# Es buena practica cargar las librerías que se utilizaran al inicio
library(readxl)
library("dplyr")
library("stringr")
library("magrittr")
library("tidyverse")
library(lubridate)
library(readr)



# Es buena practica comenzar por limpiar el entorno de trabajo, cerrar
# cualquier gráfico abierto y limpiar la consola
graphics.off()
rm(list=ls())
cat("\014")
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/input/")
#Carga de base Hogares

hogares <- read_excel("ENCOVI_hogares.xlsx")
equipamiento <- read_excel("Equipamiento.xlsx")


#Ver las variables
nombre_variables <- names(hogares)
nombre_variables

#Depurar solo las variables que necesito 
variables <- hogares %<>% 
    select(DEPTO, AREA, FACTOR, P01D20D, P01D20C)

head(variables)

#cambiar nombres de variables
variables <- variables |>
    rename(
        depto = DEPTO,
        area = AREA,
        factor = FACTOR,
        internet_residencial = P01D20D,
        telefono_celular = P01D20C
    )


str(variables)


#recuento de hogares con internet residencial 
variables %>% 
    summarise(recuento_si = 
                  sum(factor[internet_residencial == 1]))

#recuento de hogares SIN internet residencial 
variables %>% 
    summarise(recuento_no = 
                  sum(factor[internet_residencial == 2]))


#Tabla de resultados con recuento_si y recuento_no
tabla_resultados <- variables %>%
    group_by(depto) %>%
    summarise(
        recuento_si = sum(factor[internet_residencial == 1]),
        recuento_no = sum(factor[internet_residencial == 2])
    ) %>%
    arrange(depto)
tabla_resultados
