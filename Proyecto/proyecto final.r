#proyecto final

#Limpiando el entorno, la consola y la memoria

graphics.off()
rm(list=ls())
cat("\014")
getwd()
setwd("/Users/paulogarridogrijalva/Documents/PES/programa-II/Proyecto")


#Cargando librerias
library(dplyr)
library(stringr)
library(readxl)

#cargando datosogrijalva/PES-Programacion-2/Proyecto/
personas_encovi <- read_excel("input/ENCOVI_personas.xlsx")


#eliminando columnas innceasarias y convirtiendo na en 0
personas_encovi <- personas_encovi %>%
    select("DEPTO", "DOMINIO", "FACTOR", "PPA02" "P12A01", "P12A04", "P12A08") %>% 
 mutate(P12A01 = ifelse(is.na(P12A01), 0, P12A01),
        P12A04 = ifelse(is.na(P12A04), 0, P12A04),
        P12A08 = ifelse(is.na(P12A08), 0, P12A08)) 


#elminando filas con NA
#personas_encovi <- personas_encovi %>%
    #filter(!is.na(P12A01) & !is.na(P12A04) & !is.na(P12A08))
View(personas_encovi)
nrow(personas_encovi)
