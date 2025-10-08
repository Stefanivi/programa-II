# ----------------------------
# Librerías y limpieza de entorno
# ----------------------------
graphics.off()
rm(list=ls())
cat("\014")

setwd('/Users/stefanivilleda/Desktop/Programación II/Day_6/PS5/')


# ----------------------------
# Modelo de utilidad estocástica (RUM).
# ----------------------------
## utilidad CRRA para la lotería riesgosa x pero r no puede ser 1
U_x <- function(r) {
    (0.9 + 0.1 * 60^(1 - r)) / (1 - r)   # porque 1^(1-r) = 1
}

# Lotería de y segura, pero r no puee ser 1
U_y <- function(r) {
    5^(1 - r) / (1 - r)
}

# Prob(elegir x) bajo RUM Normal, pero r no puede ser 1
prob_x_rum <- function(r) {
    pnorm( (U_x(r) - U_y(r)) / sqrt(2) )
}

# a) Calcule la probabilidad de que el grupo de individuos elija la lotería riesgosa x, para
# un valor de r = 2.
r <- 2
p_a <- prob_x_rum(r)
# 0.3099
cat("--- Modelo de Utilidad Estocástica (RUM) ---\n")
cat("1(a): La probabilidad de elegir la lotería riesgosa 'x' con r =", r, "es:", round(p_a, 4), "\n\n")


#b) Grafique la anterior probabilidad para diferentes valores r ∈ [0, 10]

## Construir los datos de r en [0,10] y excluir r = 1
r_valores <- seq(0, 10, by = 0.01)
r_valores <- r_valores[r_valores != 1]   # tus U_x y U_y no valen en r = 1

## Calcular la probabilidad P(x) para cada r != 1 
probabilidad <- prob_x_rum(r_valores)    # vectorizado

## Calcular el punto en r = 1 usando el límite (forma log)
Ux1 <- 0.1 * log(60)            # = 0.9*log(1) + 0.1*log(60)
Uy1 <- log(5)
p_r1 <- pnorm( (Ux1 - Uy1) / sqrt(2) )

##Graficar la curva 
plot(r_valores, probabilidad,
     xlab = "Parámetro de aversión al riesgo (r)",
     ylab = "Probabilidad de elegir x",
     main = "Probabilidad de elegir la lotería riesgosa",
     ylim = c(0, 1))

install.packages("tinytex")

# ----------------------------
# Modelo de parámetros estocásticos (RPM)
# ----------------------------

# funciones (r no es igual 1) 
U_x <- function(r) { (0.9 + 0.1 * 60^(1 - r)) / (1 - r) }
U_y <- function(r) { 5^(1 - r) / (1 - r) }

# diferencia de utilidades
dU <- function(r) U_x(r) - U_y(r)

# (a)probabilidad de elegir x cuando r = 2
r_media <- 2

#raíz r* donde U_x(r) = U_y(r)  (está a la izquierda de 1)
r_star <- uniroot(dU, interval = c(1e-6, 0.999999))$root

# a) Probabilidad RPM de elegir x con media r = 2
prob_RPM_a <- pnorm(r_star - r_media)
#0.0348

#b) Grafique la anterior probabilidad para diferentes valores r ∈ [0, 10]
# valores de medias
mu_valores <- seq(0, 10, by = 0.1)

# prob exacta: P(x) = Phi(r* - mu)
prob <- pnorm(r_star - mu_valores)

# gráfico
plot(mu_valores, prob,
     type= "l",
     xlab = "Media de r", ylab = "Probabilidad",
     main = "Probabilidad de elegir x",
     ylim = c(0, 1)) 








