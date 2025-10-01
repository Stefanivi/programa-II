# Lista de ejercicios 1
# Introduccion a R
#
# Programacion II
# Programa de Estudios Superiores
# Banco de Guatemala
# 2025

# Integrantes: 
#Paulo Augusto Garrido Grijalva
#Stefani María Villeda Guerra
#Luis Daniel Monroy Rojas
#David Javier García Sutuj


################################################################################
# 1)  Control de Lectura
################################################################################
graphics.off()
rm(list=ls())
setwd('/Users/stefanivilleda/Desktop/Programación II/Day_1/PS')

# Las siguientes preguntas estan relacionadas con la forma de realizar distintas
# operaciones en R

# Basado en lo mostrado en la clase y los resultados de los "scripts" usados,
# responda las siguientes preguntas:

# 1) ¿Que comando de R podemos usar para limpiar el escritorio?
# R. rm(list = ls())

# 2) ¿Que comando de R podemos uar para cerrar las ventanas de graficas activas?
# R. graphics.off()

# 3) ¿Como podemos realizar producto matricial?
# R. A%*%B

# 4) ¿Como podemos realizar producto punto?
# R.  Según documentación: sería la sum(A*B) o t(A)%*%B  o B%*%A 


# 5) ¿Como podemos definir una matriz 10x10 llena de ceros? 
# R. matrix(0, nrow=10, ncol=10)

# 6) ¿Como podemos  crear un vector con cien numeros equidistantes en el 
#     intervalo [0,1]? 
# R.  seq(0, 1, length.out = 100)
?seq

# 7) Mencione tres tipos de datos comunmente encontrados en R
# R. numéricos, caracteres (strings9, lógicos (booleanos)

# 8) ¿Que libreria nos permite cargar archivos de Excel en R?
# R.  "readxl"

# 9) ¿Cual es el coeficiente de correlacion entre el numero de peliculas  en que
#     aparecio Nicolas Cage y el numero de muejeres editoras de la revista 
#     "Harvard Law Review" entre 2005 y 2009?
# R. 0.8554467

NC <- c(2,3,4,1,4) 
ME <- c(9,14,19,12,19) 
?cor
cor(NC, ME)

# 10) Haga un grafico de barras ilustrando los balones de oro ganados por 
#     Cristiano, Messi, Cruyff, Iniesta y Ronaldinho.
# R. 
rm(list=ls()) 
x <- c(5,8,3,0,1) #Valores de balón de orro a Septiembre 2025
names(x) <- c("Cristiano","Messi","Cruyff","Iniesta","Ronaldinho") 
x
x11(); barplot(x)


# 11) ¿Si la probabilidad de que Falcao se lesione es 0.2, cuantos partidos 
#      podemos esperar que juegue antes de lesionarse?
# R. #5
p <- 0.2
# Incluyendo el partido de la lesión:
1/p       # 5

#Simulación con 100 partidos
n<-100
partidos<- rgeom(n,prob= p) +1 #Simulación rgeom, cuenta el numero de fracasos antes del primer exito
partidos
mean(partidos)

################################################################################
# 2) Mi primer funcion 
################################################################################

# Adjunto encontrara el archivo "CPI.xlsx" con datos mensuales del indice de 
# precios al consumidor de Guatemala (El primer dato es inventado)

# A continuacion, escriba un codigo que haga lo siguiente:
# 1) Cargue los datos del archivo CPI.xlsx (incluya los codigos necesarios para 
#    descargar y cargar un paquete, si lo necesita)
rm(list=ls()) 
graphics.off()
df <- read.csv("CPI.csv")
df
# 2) Cree una variable inflacion.mensual que contenga la inflacion mensual de 
#    Guatemala calculada usando los datos del punto anterior.

colnames(df)
n <-nrow(df)
n
#IPCt/IPCt-1*100-100

cpi_actual<- df$CPI[2:n]

cpi_anterior<-df$CPI[1:(n-1)]

inf_mensual <- (cpi_actual-cpi_anterior)/cpi_anterior*100
inf_mensual


# 3) Defina una funcion que calcule la inflacion trimestral usando el PROMEDIO 
#    de cada trimestre


inflacion_trimestral_promedio <- function(inf_mensual){
    inflacion_mensual <- inf_mensual[!is.na(inf_mensual)]   # Elimina NAs para no "contaminar" los promedios
    inf_trimestral <- c()                    # Vector donde iremos acumulando los resultados trimestrales
    for (i in seq(0, length(inflacion_mensual), by=3)) {    # 3) Recorre el vector de 3 en 3
        if (i +2 <= length(inflacion_mensual)){
            trimestre <- inflacion_mensual[i:(i+2)]     #Extrae el bloque trimestral y calcula el promedio
            inf_trimestral <- c(inf_trimestral, mean(trimestre))
        } else {
            trimestre <- inf_mensual[i:length(inf_mensual)]
            inf_trimestral <- c(inf_trimestral, mean(trimestre))
        }
    }
    return(inf_trimestral)  #Devuelve el vector de promedios trimestrales
}

inflacion_trimestral_promedio(inf_mensual)


# 4) Defina otra funcion que calcula la inflacion trimestral usando el 
#    ULTIMO MES de cada trimestre

inflacion_trimestral_final <- function(inf_mensual) {
    inflacion_mensual_new<- inf_mensual[!is.na(inf_mensual)]    #Devuelve el vector de promedios trimestrales
    inf_trimestral <- c()   #Acumulador de resultados   
        for (i in seq(0, length(inflacion_mensual_new), by=3)){ #Recorre de 3 en 3 empezando en 1
            inf_trimestral= c(inf_trimestral, inflacion_mensual_new[i+2])   #apunta al "último mes" del bloque (i+2),
        }
        return(inf_trimestral)  #Devuelve el vector de "últimos meses" trimestrales
}

inflacion_trimestral_final(inf_mensual)




# 5) Use las anteriores funciones para calcular las variables 
#    "inflacion.trimestral.promedio" y "inflacion.trimestral.findemes" 
#    correspondientes a cada metodo

inflacion.trimestral.promedio<-inflacion_trimestral_promedio(inf_mensual)
inflacion.trimestral.findemes<- inflacion_trimestral_final(inf_mensual)


inflacion.trimestral.promedio
inflacion.trimestral.findemes
# 6) En una misma figura, muestre la grafica de cada una de las dos variables
#    calculadas en el paso anterior para comparar los resultados de cada metodo
## Unir y preparar (asumiendo que ya tienes prom_trimestral y ultimo_trimestre)
?plot
plot(
    inflacion.trimestral.promedio,
    col = "red",
    type="l",
    xlab="Trimestres",
    ylab="Inflacion trimestral",
    main="Inflación trimestral: Promedio vs Último mes"
    )

    lines(inflacion.trimestral.findemes, col="blue", type="l")
#
# Pista: Los resultados deven ser iguales a los de la hoja ".csv" en el 
# mismo archivo CPI


################################################################################
# 3) Github e intereses de ustedes
################################################################################

# 1) Lea esta breve introduccion a Github
# https://conociendogithub.readthedocs.io/en/latest/

# 2) Cree una cuenta de github y escriba aqui el usuario de cada integrante
# del grupo:
# -paulogarrido16-lab
# -Stefanivi
# -Javierggg995-tech
# -ldmr-0
#
# 3) Escriba aca las areas de interes en economia de cada integrante
# (microeconomia teorica, macroeconomia aplicada, macroeconometria,
#  microeconometria, econometria, finanzas, etc... )
#Macroeconometría, macroeconomía aplicada
    #Paulo Garrido: macroeconomía aplicada
    #Stefani Guerra: macroeconometria
    #Javier Garcia: macroeconomía
    #Daniel Monroy: macroeconometria
#
# 4) Escriba aca algun topico de interes que le llamaria la atencion aprender
#    durante el curso
# aprender a construir y diseñar modelos economicos desde R, aprendiendo
# a aplicar lo aprendido hasta el momento en el Programa de Estudios Superiores (PES)   
#Proyecciones
