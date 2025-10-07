# ----------------------------
# 1) Librerías y limpieza de entorno
# ----------------------------

graphics.off()
rm(list=ls())
cat("\014")

library(readxl)
library(dplyr)
library(stringr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(readr)
library(writexl)


#setwd("/Users/paulogarridogrijalva/Documents/GitHub/programa-II/Proyecto/")
#getwd()
# ----------------------------
# 2) Cargas de datos
# ----------------------------
#leer excel con libreria readxl
datos <- read_excel("output/IDX.xlsx")
datos
# ----------------------------
# 3) Calculo de promedio ponderado del indice
# ---------------------------
ponderadores <- c(0.18, 0.10, 0.07, 0.15, 0.15, 0.1, 0.1, 0.05, 0.1)
IDTR <- datos %>%
    group_by(depto)%>%
    summarise( IDTR = sum(ponderadores*across(everything())))

# ----------------------------
# 4) exportando el Indice a un archivo de excel con nombre de pestaña
# ---------------------------

write_xlsx(IDTR, "output/IDTR.xlsx")

