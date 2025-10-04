# ----------------------------
# 1) Librerías y limpieza de entorno
# ----------------------------
graphics.off()
rm(list=ls())
cat("\014")


library(dplyr)
library(stringr)
library(readxl)
library(tidyverse)

# ----------------------------
# 2) Carga de datos
# ----------------------------

setwd("'/Users/paulogarridogrijalva/Documents/GitHub/programa-II/Proyecto/")
datos <- read_csv("output/personas_encovi.csv")

# ----------------------------
# 3) construyendo indicadores de acceso tecnologico por departamento
# ----------------------------

#filtrando por departamento (= 1) y uso de celular si (=1)
names(datos)

por_depto_uso_cel <- datos %>%
    filter(uso_celular == 1) %>%
    group_by(depto) %>%
    summarise(si_usan_cel = sum(factor, na.rm = TRUE), .groups = "drop") %>%
    arrange(depto)

    

    
