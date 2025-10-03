# Es buena practica cargar las librerías que se utilizaran al inicio
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(readxl)



# Es buena practica comenzar por limpiar el entorno de trabajo, cerrar
# cualquier gráfico abierto y limpiar la consola
graphics.off()
rm(list=ls())
cat("\014")
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/")
#Carga de base Hogares

hogares <- read_excel("Base_hogares.xlsx")
equipamiento <- read_excel("Equipamiento.xlsx")
View(hogares)

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
#Cambiar los numeros de depto por el nombre  


#recuento de hogares con internet residencial 
variables %>% 
    summarise(recuento_si = 
                  sum(factor[internet_residencial == 1]))

#recuento de hogares SIN internet residencial 
variables %>% 
    summarise(recuento_no = 
                  sum(factor[internet_residencial == 2]))


#recuento de hogares con internet residencial por departamento 

variables %>%
    filter(internet_residencial == 1) %>% #si
    group_by(depto) %>%
    summarise(recuento = sum(factor)) %>%
    arrange((depto))



