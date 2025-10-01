# monte carlo

simulacion_montecarlo <- function(num_simulaciones, p_def_frank = 0.5) {

    p_def_mou <- 0.95

    
    #Probabilidad de que Frank gane partido

    win_frank <- 0
    win_mou <- 0
    empates <- 0
    
    for (i in 1:num_simulaciones) {
       
        goles_frank = 0
        goles_mou = 0
       for (j in 1:90) {
            
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
