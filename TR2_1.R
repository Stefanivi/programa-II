# Limpieza
rm(list = ls()); graphics.off()

# Parámetros (ajústalos si quieres)
eps   <- 2         # ε > 1
alpha <- 1.2       # α > 0
kappa <- 0.6       # 0 < κ ≤ 1
w     <- 2         # salario

# g(L) después de sustituir C = wL
g_expr  <- expression( (w*L)^(1/eps) + alpha*(1 - L)^(kappa/eps) )

# FOC interior: g'(L) = 0. Derivamos simbólicamente con respecto a L
dg_expr <- D(g_expr, "L")
dg_expr

# FOC simplificada: f(L) = 0
foc_L <- function(L, eps, alpha, kappa, w){
    w^(1/eps) * L^(1/eps - 1) - alpha*kappa * (1 - L)^(kappa/eps - 1)
}

# Utilidad reducida (por si hay solución de esquina)
U_L <- function(L, eps, alpha, kappa, w){
    ((w*L)^(1/eps) + alpha*(1 - L)^(kappa/eps))^eps
}

solve_super_frank <- function(eps, alpha, kappa, w){
    a <- 1e-8; b <- 1 - 1e-8                # mantener dominio (0,1)
    f_a <- foc_L(a, eps, alpha, kappa, w)
    f_b <- foc_L(b, eps, alpha, kappa, w)
    
    if (is.finite(f_a) && is.finite(f_b) && f_a * f_b < 0){
        # Óptimo interior
        L_star <- uniroot(foc_L, c(a,b), eps=eps, alpha=alpha, kappa=kappa, w=w)$root
        metodo <- "interior (FOC con uniroot)"
    } else {
        # Si no hay cambio de signo en la FOC, comparamos esquinas con la utilidad
        opt <- optimize(U_L, interval = c(0,1), maximum = TRUE,
                        eps=eps, alpha=alpha, kappa=kappa, w=w)
        L_star <- opt$maximum
        metodo <- "esquina (0 o 1) comparando utilidades"
    }
    
    C_star <- w * L_star
    U_star <- U_L(L_star, eps, alpha, kappa, w)
    
    # (opcional) chequeo de concavidad en L*
    d2g <- D(D(g_expr, "L"), "L")
    g2_at_Lstar <- eval(d2g, envir = list(L=L_star, eps=eps, alpha=alpha, kappa=kappa, w=w))
    
    list(L_star=L_star, C_star=C_star, U_star=U_star, metodo=metodo,
         g2_at_Lstar=g2_at_Lstar)
}

ans <- solve_super_frank(eps, alpha, kappa, w)
ans

foc_C <- function(C, eps, alpha, kappa, w){
    C^(1/eps - 1) - alpha*kappa*(1/w)*(1 - C/w)^(kappa/eps - 1)
}

solve_for_C <- function(eps, alpha, kappa, w){
    a <- 1e-8; b <- w - 1e-8
    if (foc_C(a, eps, alpha, kappa, w) * foc_C(b, eps, alpha, kappa, w) < 0){
        C_star <- uniroot(foc_C, c(a,b), eps=eps, alpha=alpha, kappa=kappa, w=w)$root
    } else {
        # Si no cambia de signo, comparamos esquinas C=0 y C=w
        U_C <- function(C) ((C)^(1/eps) + alpha*(1 - C/w)^(kappa/eps))^eps
        C_star <- optimize(U_C, c(0,w), maximum=TRUE)$maximum
    }
    L_star <- C_star / w
    list(C_star=C_star, L_star=L_star)
}
a
