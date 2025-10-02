#Tarea 3
rm(list=ls())

#setwd("")

# -----------------------------------------------
# ------------------- Inciso 1-------------------
# -----------------------------------------------

#proceso: yt = µ + ρyt−1 + εt
#εt ∼ N(0, σ2) <- el error se distribuye normal con media 0 y varianza σ2
#ro (ρ) son los valores anteriores (rezagados) de la variable como predictores. 
#µ es la media de la serie
#yt es el valor actual de la serie
#y1 ∼ N(µ/1-ρ, σ2/(1 − ρ2)) <- el primer valor de la serie se distribuye normal con media µ/1-ρ y varianza σ2/(1 − ρ2)

#1. función de log-verosimilitud (para una muestra tamalo T)

log_verosimilitud <- function(theta, y) {
    T <- length(y)
    mu <- theta[1]
    rho <- theta[2]
    sigma2 <- theta[3]

    # sumatoria <- 0
    # for(t in 2:T) {
    #     sumatoria <- sumatoria + (y[t] - mu - rho * y[t-1])^2 / (2*sigma2)
    # }
    # print(sumatoria)

    resultado <- (-1/2 * log(2 * pi) - 1/2 * log(sigma2/(1-rho^2))
                    -(y[1] - (mu/(1-rho)))^2 / ((2*sigma2)/(1-rho^2))
                    -((T-1)/2) * log(2 * pi)
                    -((T-1)/2) * log(sigma2)
                    -sum((y[2:T] - mu - rho * y[1:(T-1)])^2) / (2*sigma2)
                 )
    return(-resultado)
}


# -----------------------------------------------
# ------------------- Inciso 2-------------------
# -----------------------------------------------

simular_y <- function() {
    T <- 100
    mu <- 1
    rho <- 0.4
    sigma2 <- 0.5

    # Ahora calculamos el vector de epsilon hasta T
    # usando distribución normal
    epsilon <- rnorm(T, mean=0, sd=sqrt(sigma2))

    # Calcular el vector yt con y1 inicial igual a cero
    y <- c()
    y[1] <- rnorm(1, mu/(1-rho), sd=sqrt(sigma2/(1-rho^2)))
    print(y[1])
    for (i in 2:T) {
        y[i] <- mu + rho * y[i-1]+epsilon[i]
    }
    return(y)
}


# -----------------------------------------------
# ---------------- Inciso 3  -------------------
# -----------------------------------------------

simular_optim <- function(y) {
    theta_inicial <- c(0.5, 0.5, 0.5)

    resultado <- optim(
                    par     = theta_inicial,
                    fn      = log_verosimilitud,
                    y       = y,
                    method  = "BFGS",
                    hessian = TRUE
                )

    return(resultado)

}

# print(paste("Resultado: mu*=", resultado$par[1], "rho*=", resultado$par[2], "sigma2*=", resultado$par[3]))



# -----------------------------------------------
# ----------------- Inciso 4  -------------------
# -----------------------------------------------

simular_n_veces <- function() {
    y <- c()
    df_resultados <- data.frame(mu = numeric(), rho=numeric(), sigma2=numeric())
    for (i in 1:100) {
        y <- simular_y()
        res <- simular_optim(y)
        df_resultados <- rbind(df_resultados,
                                data.frame(mu = res$par[1],
                                            rho = res$par[2],
                                            sigma2 = res$par[3]
                                        )
                                )
    }
    return(df_resultados)
}
df_res_simulados <- simular_n_veces()
df_res_simulados

for (col in names(df_res_simulados)) {
  hist(df_res_simulados[[col]], 
       main = paste("Histograma de", col), 
       xlab = col,
       col = "skyblue", 
       border = "white")
}


# -----------------------------------------------
# ----------------  PARTE 2  --------------------
# -----------------------------------------------

library(numDeriv)
#hessian_log <- hessian(func = log_verosimilitud, x = res$par, y= simular_y())

errores_estandares <- function(data) {

    # Obtener el Hessiano en el punto optimo
    res <- simular_optim(data)
    hessian_log <- res$hessian

    #Calculo de J1 hat
    T_len<- length(data)
    J1_hat <- (1/T_len) * hessian_log
    J1_hat

    # Para obtener J1_hat invertido usamos la función solve
    cov_hat <- solve(J1_hat) / T_len
    cov_hat

    # EE: errores estandar
    ee <- sqrt(diag(cov_hat))
    return(ee)
}

set.seed(1)
y <- simular_y()
ee_y <- errores_estandares(y)
ee_y
#RESULTADO INCISO 1:
# Errores estandar para y:  [1] 0.18159571 0.09390602 0.05645746

data_inflacion <- read.csv(file="inflacion.csv", header = TRUE, fileEncoding = "latin1")
names(data_inflacion) = c("mes", "inflacion")
data_inflacion

ee_inflacion <- errores_estandares(data_inflacion$inflacion)
ee_inflacion

#RESULTADO INCISO 2:
# Errores estandar para inflación: [1] 0.08454728 0.01282605 0.03947732
