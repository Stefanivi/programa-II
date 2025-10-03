# Lista de ejercicios 3
# Introduccion a R
#
# Programacion II
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025

# Integrantes: 
# - Paulo Augusto Garrido Grijalva
# - Stefani María Villeda Guerra
# - Luis Daniel Monroy Rojas
# - David Javier García Sutuj

rm(list=ls())
graphics.off()
getwd()
setwd("C:\\Users\\Javie\\OneDrive\\Escritorio\\PROGRAMACIÓN II\\Day_5\\db_csv_\\db_csv_")

library(tidyverse)
library(janitor)
library(broom)
library(readr)
library(dplyr)
library(ggplot2)

# -----------------------------------------------
# ------------------- Inciso 1-------------------
# -----------------------------------------------

# Limpieza y visualización de datos 

datos <- data.table::fread("Persona - BDP.csv", select = c("DEPARTAMENTO","MUNICIPIO", "COD_MUNICIPIO", "PCP6", "PCP7", "ANEDUCA","PEA", "POCUPA", "PDESOC"))

# Nombre asignado a las columnas

colnames(datos) <- c("departamento", "municipio", "cod_municipio", "sexo", "edad", "años_educacion", "pea", "Pocupa", "pdesoc")

#asignarle nombre a cada departamento 
datos$departamento <- recode(datos$departamento, 
                             "1" = "Guatemala",
                             "2" = "El Progreso",
                             "3" = "Sacatepequez",
                             "4" = "Chimaltenango",
                             "5" = "Escuintla",
                             "6" = "Santa Rosa",
                             "7" = "Solola",
                             "8" = "Totonicapan",
                             "9" = "Quetzaltenango",
                             "10" = "Suchitepequez",
                             "11" = "Retalhuleu",
                             "12" = "San Marcos",
                             "13" = "Huehuetenango",
                             "14" = "Quiche",
                             "15" = "Baja Verapaz",
                             "16" = "Alta Verapaz",
                             "17" = "Petén",
                             "18" = "Izabal",
                             "19" = "Zacapa",
                             "20" = "Chiquimula",
                             "21" = "Jalapa",
                             "22" = "Jutiapa")

#quitar las datos con NA en las variables años_educacion, pea, pocupa, pdesoc 

datos_limpios <- datos |>
  filter(!is.na("años_educacion",
         !is.na("pea"),
         !is.na("Pocupa"),
         !is.na("pdesoc")))

#1. En una tabla, muestre el número de hombres y el número de mujeres por departamento en Guatemala.
#Cargar los datos

tabla_hm <- table(datos$departamento, datos$sexo)
colnames(tabla_hm) <- c("Hombre", "Mujer")
tabla_hm

#Graficar el número de hombres y mujeres por departamento

ggplot(as.data.frame(tabla_hm), 
       aes(x = reorder(Var1, -Freq), y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Número de hombres y mujeres por departamento en Guatemala",
       x = "Departamento",
       y = "Número de personas",
       fill = "Sexo") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

geom_text(aes(label = Freq), 
          position = position_dodge(width = 0.8), 
          vjust = -0.3, size = 3)
labs(
  title = "Número de hombres y mujeres por departamento en Guatemala",
  subtitle = "Fuente: ENCOVI 2023",
  x = "Departamento",
  y = "Número de personas",
  fill = "Sexo"
)

# Exportar la base de datos limpia a un archivo CSV
write.csv(datos, "datos_limpios.csv")
