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

#setwd("'/Users/paulogarridogrijalva/Documents/GitHub/programa-II/Proyecto/")
datos <- read_csv("output/personas_encovi.csv")

# ----------------------------
# 3) construyendo indicadores de acceso tecnologico por departamento
# ----------------------------

# Indicador 1: Proporción de personas que usan celular por departamento
#--------------------
uso_cel <- datos %>%
  group_by(depto) %>%
  summarise(
    total_resp = sum(factor, na.rm = TRUE),
    usan_cel     = sum(if_else(uso_celular == 1, factor, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_uso_cel = 100 * usan_cel / total_resp) %>%
  arrange(depto)
var_uso_cel <- uso_cel$pct_uso_cel
names(datos)
# Indicador 2: Proporción de personas que usan internet por departamento
#--------------------    
uso_intern <- datos %>%
    group_by(depto) %>%
    summarise(
        total_resp = sum(factor, na.rm = TRUE),
        usan_intern     = sum(if_else(uso_internet == 1, factor, 0), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(pct_uso_internet = 100 * usan_intern / total_resp) %>%
    arrange(depto)
var_uso_intern <- uso_intern$pct_uso_internet

# Indicador 3: Proporción de personas que usan internet por departamento y por genero
#--------------------    
uso_intern_mujeres <- datos %>%
    group_by(depto) %>%
    summarise(
        total_resp = sum(factor, na.rm = TRUE),
        usan_intern_mujeres     = sum(if_else(uso_internet == 1 & sexo == 2 , factor, 0), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(pct_uso_internet_mujeres = 100 * usan_intern_mujeres / total_resp) %>%
    arrange(depto)
var_uso_intern_mujeres <- uso_intern_mujeres$pct_uso_internet_mujeres

# Indicador 4: Acceso a telefono movil
#--------------------    
acceso_tel_mov <- datos %>%
    group_by(depto) %>%
    summarise(
        total_resp = sum(factor, na.rm = TRUE),
        pers_con_mov = sum(if_else(tiene_celular == 1 , factor, 0), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(pct_tiene_celular = 100 * pers_con_mov / total_resp) %>%
    arrange(depto)
var_uso_acceso_tel_mov <- acceso_tel_mov$pct_tiene_celular

# Indicador 5: porcentaje de población rural por departamento
#--------------------    
poblacion_rural <- datos %>%
    group_by(depto) %>%
    summarise(
        total_resp = sum(factor, na.rm = TRUE),
        poblacion_rural = sum(if_else(area == 2 , factor, 0), na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(pct_rural = 100 * poblacion_rural / total_resp) %>%
    arrange(depto)
pct_rural <- poblacion_rural$pct_rural

poblacion_rural %>%
    summarise(sum(poblacion_rural)/sum(total_resp))

# ----------------------------
# 4) Guardando variables en un data frame
# ----------------------------
df_indicadores <- data.frame(depto = uso_cel$depto,
                             var_uso_cel,
                             var_uso_intern,
                             var_uso_intern_mujeres,
                             var_uso_acceso_tel_mov,
                             pct_rural)
# ----------------------------
# 5) Exportando en documento de excel xls
# ----------------------------

write_xlsx(df_indicadores, "output/indicadores_acceso_tecnologico.xlsx")


    
