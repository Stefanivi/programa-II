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


# limpiando el ambiente:
graphics.off(); rm(list = ls())

#------------------------------------
#------------------------------------
#EJERCICIO 1
#------------------------------------
#------------------------------------

#Inciso 1. Cree una función:
log_verosimilitud <- function(coeficientes, datos){
    mu <- coeficientes[1]
    rho <- coeficientes[2]
    sigma2 <- coeficientes[3]
    #número de observaciones
    T<- length(datos)
    resultado<- (-0.5*log(2*pi)-0.5*log(sigma2/(1-rho^2))
        -((datos[1]-(mu/(1-rho)))^2/((2*sigma2)/(1-rho^2)))
        -((T-1)/2)*log(2*pi)-((T-1)/2)*log(sigma2)
    -sum((datos[2:T]-mu-rho*datos[1:(T-1)])^2/(2*sigma2))
    )
    return(-resultado)
}
#Cree una función que calcule la log-verosimilitud de un proceso AR(1) con media mu, varianza sigma2 y autocorrelación rho

#Inciso 2. Genere el vector εt y el valor inicial y1 y simule T = 100 observaciones del proceso yt con los parámetros µ = 1, ρ = 0.4 y σ2 = 0.5
T <- 100
mu <- 1
rho <- 0.4
sigma2 <- 0.5
set.seed(1)
#obteniendo valores de epsilon y y_1
epsilon<-c(rnorm(T, mean=0, sd=sqrt(sigma2)))
y_1 <- rnorm(1, mean= mu/(1-rho), sd=sqrt(sigma2/(1-rho^2)))
#inicializando y
y <- c(y_1)
#Simulación del proceso para obtener las observaciones de y 
for (t in 2:T){
    y[t] <- mu + rho*y[t-1] + epsilon[t]
}
y

#3:Use una función de optimización numérica para encontrar el estimador de máxima verosimilitud
#definido como el vector de parámetros que maximiza la función de log-verosimilitud dados los datos simulados
#en el punto anterior (Use como valores iniciales el vector θ = [0.5, 0.5, 0.5]′
?optim
    optim_result <- optim(par = c(0.5, 0.5, 0.5), fn = log_verosimilitud, datos = y, method = "BFGS", hessian=TRUE)
#Mostrando los resultados
optim_result


#4: Cree un ciclo que repita 1000 veces los pasos en los puntos (2) a (3) y guarde los valores de θ= µ, ρ, σ2
#obtenidos en cada iteración. Grafique el histograma de frecuencias de la distribución de el estimador de cada
#uno de estos parámetros.

num_simulaciones <- 1000
resultados <- matrix(NA, nrow = num_simulaciones, ncol = 3)
set.seed(1)  # Para reproducibilidad
for (i in 1:num_simulaciones) {
    # Simulación del proceso AR(1)
    epsilon <- rnorm(T, mean = 0, sd = sqrt(sigma2))
    y_1_MC <- rnorm(1, mean = mu/(1 - rho), sd = sqrt(sigma2/(1 - rho^2)))
    y_MC <- numeric(T)
    y_MC[1] <- y_1_MC
    for (t in 2:T) {
        y_MC[t] <- mu + rho * y[t - 1] + epsilon[t]
    }
    
    # Estimación por máxima verosimilitud
    result_optim<- optim(par = c(0.5, 0.5, 0.5), fn = log_verosimilitud, datos = y_MC, method = "BFGS", hessian=TRUE)
    resultados[i, ] <- result_optim$par
}


# Asignar nombres a las columnas
colnames(resultados) <- c("mu", "rho", "sigma2")

# Graficando histogramas
par(mfrow = c(3, 1))  # Configurar la gráfica para 3 filas y 1 columna
hist(resultados[, 1], main = "Histograma de mu", xlab = "rho", col = "blue", border = "black")
hist(resultados[, 2], main = "Histograma de rho", xlab = "sigma2", col = "green", border = "black")
hist(resultados[, 3], main = "Histograma de sigma2", xlab = "mu", col = "red", border = "black")
par(mfrow = c(1, 1))  # Restablecer la configuración gráfica



#-----------------------------------------------
#-----------------------------------------------
#EJERCICIO 2: Estimación de un AR(1), Parte II
#----------------------------------------------
#----------------------------------------------
library(numDeriv)
?hessian
#Inciso 1: Escriba una función que calcule los errores estándar del estimador de máxima verosimilitud

hessiana<- hessian(func= log_verosimilitud, x=optim_result$par , datos= y)
hessiana

J_hat<-(1/T)*hessiana
J_hat

inv_J_hat <- solve(J_hat)
inv_J_hat

ee_standard<-(sqrt(diag(inv_J_hat/T)))
ee_standard

#Funcion de ee_standard
ee_standard_func <- function(datos) {
    optimo = optim(par = c(0.5, 0.5, 0.5), fn = log_verosimilitud, datos = datos, method = "BFGS", hessian=TRUE)
    hessiana <- optimo$hessian
    T <- length(datos)
    J_hat <- (1 / T) * hessiana
    inv_J_hat <- solve(J_hat)
    ee_standard <- sqrt(diag(inv_J_hat / T))
    return(ee_standard)
}


#Inciso 2: Utilice el código anterior para encontrar el estimador de máxima verosimilitud de un AR(1) y sus errores
#estándar asociados para la serie de inflación mensual en la hoja de Excel adjunta a este taller.

setwd("/Users/stefanivilleda/Desktop/Programación II/Day_3/PS3")
inflacion_data <- read.csv("inflacion.csv")
datos <- inflacion_data$inflacion
datos

#Estimación por máxima verosimilitud

log_verosimilitud_inflacion <- optim(par = c(0.5, 0.5, 0.5), fn = log_verosimilitud, datos = datos, method = "BFGS", hessian=TRUE)
log_verosimilitud_inflacion$par
#[1] 0.1807254 0.9700525 0.5381414


#Calculando errores estándar
ee_standard_inflacion <- ee_standard_func(datos)
ee_standard_inflacion
#ee: [1] 0.08454728 0.01282605 0.03947732
