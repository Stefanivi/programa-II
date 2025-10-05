library(readxl)
library(dplyr)
library(tidyverse)


graphics.off()
rm(list=ls())
cat("\014")
setwd("/Users/stefanivilleda/Desktop/Programación II/Proyecto/programa-II/Proyecto/output/")
#Carga de base

datos <- read_csv("PIB_per_capita.csv")
head(datos)

#crear otra variable en una columna a la derecha que multiplique PIB per capita por poblacion
datos <- datos %>%
    mutate(
        PIB_depto = PIB_per_capita*poblacion
    )
datos

#Calcular % de PIB por departamento
datos <- datos %>%
    mutate(
        pct_PIB = datos$PIB_depto / sum(datos$PIB_depto),
        pct_PIB= round(pct_PIB * 100, 2)   
    )
datos

#Creando variable
pct_PIB_dpto <- datos$pct_PIB
