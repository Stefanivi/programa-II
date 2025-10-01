#Tarea 2
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
