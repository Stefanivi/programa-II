# Lista de ejercicios 2
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
# **Nota: Todas las respuestas estan al final**
#========================================================
# Ejercicio 1
#========================================================

# limpiando el ambiente:
graphics.off(); rm(list = ls())

# Cargando numDeriv (para gradiente y Hessiano numéricos).
library(numDeriv)

#-------------------------
# Parámetros del modelo
#-------------------------
eps   <- 3        # ε > 1  (curvatura de la utilidad)
alpha <- 2        # α > 0  (peso relativo del ocio)
kappa <- 0.8      # 0 < κ ≤ 1 (elasticidad del ocio)U_star <- U_L(L_star, eps, alpha, kappa, w)
w     <- 1        # salario > 0

#-------------------------
# 1) Optimizando g(L), buscando solución interior
#-------------------------
# g(L) proviene de sustituir la restricción C = wL en la utilidad original.
# Maximizamos g(L) en L ∈ (0,1).
g_fun <- function(L, eps, alpha, kappa, w){
    # Control de dominio: si L está fuera de (0,1), se devuelve NA para no evaluar
    if (any(L <= 0) || any(L >= 1)) return(NA_real_)
    # g(L) = (wL)^(1/ε) + α(1−L)^(κ/ε)
    (w*L)^(1/eps) + alpha*(1 - L)^(kappa/eps)
}

Util <- function(L, eps, alpha, kappa, w){
    ((w*L)^(1/eps) + alpha*(1 - L)^(kappa/eps))^eps
} # utilidad original U(C,L) con C = wL


#-------------------------
# 2) CPO simbólica
#-------------------------
# para mostrar la derivada simbólica g'(L), es decir su expresión analitica
L <- NULL
g_expr  <- expression( (w*L)^(1/eps) + alpha*(1 - L)^(kappa/eps) )
der_expr <- D(g_expr, "L")  # derivada simbólica respecto de L
cat("g'(L) simbólica =\n"); print(der_expr); cat("\n")

#-------------------------
# 3) g'(L) numéricamente  resolviendo g'(L)=0.
#-------------------------
# grad() calcula ∂g/∂L numéricamente en un punto L dado. (g_deriv)
g_deriv <- function(L){
    grad(function(l) g_fun(l, eps, alpha, kappa, w), x = L)
}

#-------------------------
# se busca hallar un intervalo [a, b] con cambio de signo en g'(L)
# para que uniroot() garantice una raíz (utilizando el Teorema del Valor Intermedio).
#-------------------------
L_values <- seq(0.01, 0.99, length.out = 200)  # malla fina en (0,1)
g1    <- sapply(L_values, g_deriv)          # g'(L) evaluada en la malla
sg    <- sign(g1)                           # signo de g'(L) en cada punto

# Detectamos posiciones donde cambia el signo (de + a − o de − a +):
camb_sig <- which(diff(sg) != 0)

# Tomamos el primer intervalo consecutivo donde ocurre el cambio
a <- L_values[camb_sig[1]]
b <- L_values[camb_sig[1] + 1]

#-------------------------
# 4) Resolviendo g'(L)=0 con uniroot en el intervalo encontrando [a, b]
#-------------------------
# uniroot exige f(a)*f(b) ≤ 0 (cambio de signo).
sol    <- uniroot(function(l) g_deriv(l), interval = c(a, b))
L_star <- sol$root                 # L* (óptimo interior)
C_star <- w * L_star               # C* = w·L* (consumo óptimo)
U_star <- Util(L_star, eps, alpha, kappa, w)  # utilidad óptima
#-------------------------
# 5) Resultados (reporte en consola)
#-------------------------
cat(sprintf("Parámetros: ε=%.4f, α=%.4f, κ=%.4f, w=%.4f\n", eps, alpha, kappa, w))
cat(sprintf("L*  = %.6f\n", L_star))
cat(sprintf("C*  = w*L* = %.6f\n", C_star))
cat(sprintf("g(L*) = %.6f\n", g_fun(L_star, eps, alpha, kappa, w)))


#========================================================
# Ejercicio 2
#========================================================
# limpiando el ambiente:
graphics.off(); rm(list = ls())

# Definir parámetros
# alpha <- 1.2
# kappa <- 0.6
# epsilon <- 2
# w <- 2

alpha <- 2
kappa <- 0.8
epsilon <- 3
w <- 1

# Definir la función de utilidad
U <- function(L, alpha, kappa, epsilon, w) {
    if (L <= 0 || L >= 1) return(-Inf) # penalizamos valores fuera de rango
    C <- w * L
    return(((C^(1/epsilon)) + alpha * (1 - L)^(kappa/epsilon))^epsilon)
}

# Optimización (buscando el máximo, por eso usamos -U)
opt <- optim(par = 0.5, fn = function(L) -U(L, alpha, kappa, epsilon, w),
             method = "Brent", lower = 0, upper = 1)

paste("Valor optimo de L*= ", opt$par)
paste("Valor optimo de C*= ", w * opt$par)
paste("Utilidad Máxima U*=", U(opt$par, alpha, kappa, epsilon, w))


#========================================================
# Ejercicio 3 
#========================================================
# limpiando el ambiente:
graphics.off(); rm(list = ls())
# Monte Carlo

simulacion_montecarlo <- function(num_simulaciones, p_def_frank = 0.5) {
    
    p_def_mou <- 0.95
    
    
    #Probabilidad de que Frank gane partido
    
    #contadores
    win_frank <- 0
    win_mou <- 0
    empates <- 0
    
    for (i in 1:num_simulaciones) {
        
        goles_frank = 0
        goles_mou = 0
        for (j in 1:90) {
            
            #probabilidades aleatorias de que cada uno juegue a la defensiva
            rnd_frank <- runif(1)
            rnd_mou <- runif(1)
            
            
            if (rnd_frank < p_def_frank & rnd_mou < p_def_mou) { # ambos a la defensiva
                # no hacer nada 
                next
            } else if (rnd_frank > p_def_frank & rnd_mou > p_def_mou) { # ambos a la ofensiva
                gol_frank <- runif(1)
                gol_mou <- runif(1)
                
                if (gol_frank < 0.05) goles_frank <- goles_frank + 1
                if (gol_mou < 0.05) goles_mou <- goles_mou + 1
                
            } else if (rnd_frank > p_def_frank & rnd_mou < p_def_mou) { #frank ofensiva y mou defensiva
                
                gol_frank <- runif(1)
                gol_mou <- runif(1)
                
                if (gol_frank < 0.03) goles_frank <- goles_frank + 1
                if (gol_mou < 0.01) goles_mou <- goles_mou + 1
                
            } else if (rnd_frank < p_def_frank & rnd_mou > p_def_mou) { # frank defensiva y mou ofensiva
                
                gol_frank <- runif(1)
                gol_mou <- runif(1)
                
                if (gol_frank < 0.01) goles_frank <- goles_frank + 1
                if (gol_mou < 0.03) goles_mou <- goles_mou + 1
                
            }
        }
        
        if (goles_mou > goles_frank) {
            win_mou <- win_mou + 1
        } else if (goles_frank > goles_mou) {
            win_frank <- win_frank + 1
        } else {
            empates <- empates + 1
        }
    }
    # print(paste("Partidos ganados Mou", win_mou, ", pct_win: ", round(win_mou/num_simulaciones*100, 2)))
    # print(paste("Partidos ganados frank", win_frank, ", pct_frank: ", round(win_frank/num_simulaciones*100, 2)))
    # print(paste("Partidos empatados", empates, ", pct_empates: ",  round(empates/num_simulaciones*100, 2)))
    return( round(win_frank/num_simulaciones*100, 2))
}

graficar_probabilidades_frank <- function(paso = 0.1) {
    list_prob_defensiva = seq(0, 1, by = paso)
    pct_win_frank = rep(0, length(list_prob_defensiva))
    
    
    for (i in 1:length(pct_win_frank)) {
        pct_win_frank[[i]] = simulacion_montecarlo(1000, list_prob_defensiva[[i]])
    }
    
    plot(list_prob_defensiva, pct_win_frank, type="l")
    
}

simulacion_montecarlo(10000) # Out: 56.23
#Resultado: Probabilidad de que frank gane es 56.2% con 10k simulaciones

simulacion_montecarlo(1000, 0.3) # Out: 67.8
simulacion_montecarlo(1000, 0.2) # Out: 69.1
simulacion_montecarlo(1000, 0.1) # Out: 69.5
simulacion_montecarlo(1000, 0) # Out: 74.6


# RESPUESTAS:
# Solución numérica de ecuaciones no-lineales
# 1)   ((w*L)^(1/eps) + alpha*(1 - L)^(kappa/eps))^eps
# 2)   g'(L) simbólica = (w * L)^((1/eps) - 1) * ((1/eps) * w) - alpha * ((1 - L)^((kappa/eps) - 1) * (kappa/eps))
# 3)   C*  = w*L* = 0.322162
#
# Optimización numérica
# 1)   Valor optimo de L*=  0.322154320923125
#
# Simulación Montecarlo
# 1)   Probabilidad de que frank gane es 56.2% con 10k simulaciones
# 2)   La probabilidad de jugar a la defensiva es 0% y tiene una probabilidad de ganar de 73% aprox.