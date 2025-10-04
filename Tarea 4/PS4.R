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

rm(list = ls())
graphics.off()
getwd()
setwd("C:/Users/danie/Documents/CursoProgra_2R/programa-II/Tarea 4")

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

datos <- data.table::fread(
    "Persona - BDP.csv", 
    select = c("DEPARTAMENTO",
               "MUNICIPIO",
               "COD_MUNICIPIO",
               "PCP6",
               "PCP7",
               "ANEDUCA",
               "PEA",
               "POCUPA")
)

# Nombre asignado a las columnas

colnames(datos) <- c("departamento",
                     "municipio",
                     "cod_municipio",
                     "sexo",
                     "edad",
                     "anios_educacion",
                     "pea",
                     "pocupa")

# Asignarle nombre a cada departamento 
datos$departamento <- recode(
    datos$departamento, 
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
    "22" = "Jutiapa"
)

# Quitar los datos con NA en las variables años_educacion, pea


# ----------------------------------------------------------------------------
# -----------------------Inciso 1 --------------------------------------------
# ----------------------------------------------------------------------------

# 1. En una tabla, muestre el número de hombres y el número de mujeres 
# por departamento en Guatemala.

tabla_hm <- table(datos$departamento, datos$sexo)
colnames(tabla_hm) <- c("Hombre", "Mujer")
tabla_hm

# Graficar el número de hombres y mujeres por departamento

ggplot(as.data.frame(tabla_hm), 
       aes(x = reorder(Var1, -Freq), y = Freq, fill = Var2)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    labs(title = "Número de hombres y mujeres por departamento en Guatemala",
         x = "Departamento",
         y = "Número de personas",
         fill = "Sexo") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = Freq), 
              position = position_dodge(width = 0.8), 
              vjust = -0.3, size = 3) +
    labs(
        title = "Número de hombres y mujeres por departamento en Guatemala",
        subtitle = "Fuente: ENCOVI 2023",
        x = "Departamento",
        y = "Número de personas",
        fill = "Sexo"
    )

# Exportar la base de datos limpia a un archivo CSV
write.csv(datos_sin_na, "datos_limpios.csv")

# ----------------------------------------------------------------------------
# -----------------------Inciso 2 --------------------------------------------
# ----------------------------------------------------------------------------

# Modelo de regresión lineal de tasa de desocupados por años de ocupación
# agrupando por municipios

# Revisión de los datos que contiene cada columna
distinct(datos_sin_na, pea)
distinct(datos_sin_na, pocupa)
distinct(datos_sin_na, anios_educacion) |> arrange(anios_educacion)
str(datos_sin_na$pocupa)
unique(datos_sin_na$pocupa)

# Se eliminan los datos nulos en la columna anios_de_educacion, se dejan
# los nulos en las columnas pea y pocupa
datos_sin_na <- datos |>
    filter(!is.na(anios_educacion))

# Se arma la tabla agregada por municipio con años promedio de educación
# y tasa de desocupación
df_municipios <- datos_sin_na |>
    group_by(municipio) |>
    summarise(
        #total = n(),
        td = round( sum(!is.na(pocupa)) / sum(!is.na(pea)), 4),
        educ = mean(anios_educacion)
    )
head(df_municipios, 5)


# Otra forma para comprobar el uso de na.rm
df_municipios <- datos_sin_na |>
    group_by(municipio) |>
    summarise(
        td = round( sum(pocupa == 1, na.rm = TRUE) / sum(pea == 1, na.rm = TRUE), 4),
        educ = mean(anios_educacion)
    )
head(df_municipios, 10)

# Modelo de regresión según el pdf
formula_modelo <- td ~ educ
modelo <- lm(formula_modelo, data = df_municipios)
summary(modelo)

# ----------------------------------------------------------------------------
# -----------------------Inciso 3 --------------------------------------------
# ----------------------------------------------------------------------------


# Diagrama de dispersión de años de educación y tasa de desocupados
plot_dispersion <- df_municipios |>
    ggplot(aes(x = educ, y = td)) +
    geom_point(color = "dodgerblue4", size = 2, alpha = 0.5) +
    geom_smooth(color = "firebrick3", size = 1.2) +
    scale_x_continuous(
        breaks = seq(0, 10, by = 1)
    ) +
    labs(
        title = "Regresión OLS: tasa de desocupados ~ educación",
        subtitle = "Datos agregados por municipio",
        y = "Tasa de desocupados %",
        x = "Años promedio de educación"
    ) +
    theme_minimal()+
    theme(
        plot.title       = element_text(size = 16, face = "bold", hjust=0.5),
        plot.subtitle    = element_text(size = 14, hjust = 0.5),
        axis.title.x     = element_text(size = 12),
        axis.title.y     = element_text(size = 12),
        axis.text.x      = element_text(angle = 0, hjust = 1),
        legend.position  = "top"
    )
plot_dispersion

# ----------------------------------------------------------------------------
# -----------------------Inciso 4 --------------------------------------------
# ----------------------------------------------------------------------------


# Piramide de población %. Datos a nivel nacional por edades distiguiendo entre
# hombres (1) y mujeres (2)


df_edades <- datos |> select(edad, sexo) |> filter(!is.na(edad))
total_obs = nrow(df_edades)

df_edades <- df_edades |>
    mutate(
        grupo_edad = cut(x = edad,
                         breaks = c(seq(0, 100, by = 5), Inf),
                         right = FALSE,
                         include.lowest = TRUE,
                         labels =c(paste(seq(0, 95, by = 5), 
                                         seq(4, 99, by = 5), 
                                         sep = "-"
                                         ),
                                 "100 o más" 
                             )
                        )
    ) |> group_by(grupo_edad) |>
    summarise(
        hombres = sum(sexo == 1) / total_obs,
        mujeres = sum(sexo == 2) / total_obs
    )
View(df_edades)


df_piramide <- df_edades |>
    pivot_longer(
        cols = c(hombres, mujeres),
        names_to = "sexo",
        values_to = "porcentaje"
    ) |>
    mutate(
        porcentaje = ifelse(sexo == "hombres", -porcentaje, porcentaje)  # hombres a la izquierda
    )

plot_piramide <- ggplot(df_piramide, aes(x = porcentaje, y = grupo_edad, fill = sexo)) +
    geom_bar(stat = "identity", width = 0.9) +
    scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        breaks = seq(-0.06, 0.06, by = 0.02)  # ajusta según tus proporciones
    ) +
    scale_fill_manual(values = c("hombres" = "dodgerblue2", "mujeres" = "indianred")) +
    labs(
        title = "Pirámide de población (%)",
        subtitle = "Datos a nivel nacional",
        x = NULL,
        y = NULL,
        fill = ""
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "top"
    )
plot_piramide
