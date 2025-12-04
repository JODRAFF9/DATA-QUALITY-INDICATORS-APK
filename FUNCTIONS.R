library(dplyr)
library(tidyr)

####################Indice de Whipple####################
indice_whipple <- function(data) {
  # Validation des données
  if (!all(c("Age", "Homme", "Femme", "Total") %in% names(data))) {
    stop("Colonnes manquantes: Age, Homme, Femme, Total")
  }
  
  # Âges se terminant par 0 ou 5 entre 23 et 62 ans
  ages_0_5 <- data$Age[data$Age >= 23 & data$Age <= 62 & (data$Age %% 5 == 0)]
  
  calc_whipple <- function(population) {
    A <- sum(population[data$Age %in% ages_0_5], na.rm = TRUE)
    B <- sum(population[data$Age >= 23 & data$Age <= 62], na.rm = TRUE)
    if (B == 0) return(0)
    return(5 * A / B)
  }
  
  result <- list(
    homme = calc_whipple(data$Homme),
    femme = calc_whipple(data$Femme),
    ensemble = calc_whipple(data$Total)
  )
  
  return(result)
}

#####################Indice de Myers####################
indice_myers <- function(data) {
  calc_myers <- function(population) {
    # Filtrer âges >= 10 ans
    ages_10plus <- data$Age >= 10
    ages <- data$Age[ages_10plus]
    eff <- population[ages_10plus]
    
    if (sum(eff, na.rm = TRUE) == 0) {
      return(list(indice = 0, Tu = rep(0, 10)))
    }
    
    # Calcul des Su (chiffres terminaux)
    Su <- sapply(0:9, function(u) {
      sum(eff[ages %% 10 == u], na.rm = TRUE)
    })
    
    # Filtrer âges >= 20 ans pour S'u
    ages_20plus <- data$Age >= 20
    ages2 <- data$Age[ages_20plus]
    eff2 <- population[ages_20plus]
    
    Su_prime <- sapply(0:9, function(u) {
      sum(eff2[ages2 %% 10 == u], na.rm = TRUE)
    })
    
    # Calcul des Tu
    Tu <- sapply(0:9, function(u) {
      (u + 1) * Su[u + 1] + (9 - u) * Su_prime[u + 1]
    })
    
    # Indice de Myers
    T_total <- sum(Tu, na.rm = TRUE)
    if (T_total == 0) return(list(indice = 0, Tu = Tu))
    
    pourcentages <- 100 * Tu / T_total
    indiceU <- abs(pourcentages - 10)
    indice <- sum(abs(indiceU))
    
    return(list(indice = indice, Tu = Tu,indiceU = indiceU))
  }
  
  return(list(
    homme = calc_myers(data$Homme),
    femme = calc_myers(data$Femme),
    ensemble = calc_myers(data$Total)
  ))
}

#####################Indice de Bachi####################
indice_bachi <- function(pop_m, pop_f) {
  calc_bachi <- function(pop) {
    # Vérifier la longueur
    if (length(pop) < 78) {
      pop <- c(pop, rep(0, 78 - length(pop)))
    }
    
    # Calcul des Au selon la méthode standard
    A0 <- sum(pop[c(31,41,51,61,71)], na.rm = TRUE)  # Âges 30,40,50,60,70
    A1 <- sum(pop[c(32,42,52,62,72)], na.rm = TRUE)  # Âges 31,41,51,61,71
    A2 <- sum(pop[c(33,43,53,63,73)], na.rm = TRUE)  # Âges 32,42,52,62,72
    A3 <- 0.5*pop[24] + sum(pop[c(34,44,54,64)], na.rm = TRUE) + 0.5*pop[74]  # 23,33,43,53,63,73
    A4 <- 0.5*pop[25] + sum(pop[c(35,45,55,65)], na.rm = TRUE) + 0.5*pop[75]  # 24,34,44,54,64,74
    A5 <- 0.5*pop[26] + sum(pop[c(36,46,56,66)], na.rm = TRUE) + 0.5*pop[76]  # 25,35,45,55,65,75
    A6 <- 0.5*pop[27] + sum(pop[c(37,47,57,67)], na.rm = TRUE) + 0.5*pop[77]  # 26,36,46,56,66,76
    A7 <- 0.5*pop[28] + sum(pop[c(38,48,58,68)], na.rm = TRUE) + 0.5*pop[78]  # 27,37,47,57,67,77
    A8 <- sum(pop[c(29,39,49,59,69)], na.rm = TRUE)  # Âges 28,38,48,58,68
    A9 <- sum(pop[c(30,40,50,60,70)], na.rm = TRUE)  # Âges 29,39,49,59,69
    
    A <- c(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)
    
    # Calcul des Du (dénominateurs)
    B23_72 <- sum(pop[24:73], na.rm = TRUE)  # Âges 23-72
    B24_73 <- sum(pop[25:74], na.rm = TRUE)  # Âges 24-73
    B25_74 <- sum(pop[26:75], na.rm = TRUE)  # Âges 25-74
    B26_75 <- sum(pop[27:76], na.rm = TRUE)  # Âges 26-75
    B27_76 <- sum(pop[28:77], na.rm = TRUE)  # Âges 27-76
    B28_77 <- sum(pop[29:78], na.rm = TRUE)  # Âges 28-77
    
    D0 <- 0.5 * (B25_74 + B26_75)
    D1 <- 0.5 * (B26_75 + B27_76)
    D2 <- 0.5 * (B27_76 + B28_77)
    D3 <- 0.5 * (B23_72 + B24_73)
    D4 <- 0.5 * (B24_73 + B25_74)
    D5 <- 0.5 * (B25_74 + B26_75)
    D6 <- 0.5 * (B26_75 + B27_76)
    D7 <- 0.5 * (B27_76 + B28_77)
    D8 <- 0.5 * (B23_72 + B24_73)
    D9 <- 0.5 * (B24_73 + B25_74)
    
    D <- c(D0, D1, D2, D3, D4, D5, D6, D7, D8, D9)
    
    # Éviter la division par zéro
    D <- ifelse(D == 0, 1, D)
    
    ru <- 100 * A / D
    ecarts_positifs <- pmax(ru - 10, 0)
    indice <- sum(ecarts_positifs, na.rm = TRUE)
    
    return(list(indice = indice, ru = ru, chiffres = 0:9))
  }
  
  return(list(
    masculin = calc_bachi(pop_m),
    feminin = calc_bachi(pop_f)
  ))
}

#####################Indice combiné Nations Unies complet####################
indice_combine_nu <- function(pop_m_quinquenal, pop_f_quinquenal, taille_population = NULL) {
  n <- length(pop_m_quinquenal)
  
  if (n < 3) {
    return(list(
      I_brut = NA, I_net = NA, J_m = NA, J_f = NA, K = NA, S_correction = NA
    ))
  }
  
  # Calcul des rapports de groupes d'âge
  calc_rapports <- function(pop) {
    rapports <- numeric(n - 2)
    for (i in 2:(n - 1)) {
      denom <- pop[i-1] + pop[i+1]
      if (denom == 0) {
        rapports[i-1] <- 100
      } else {
        rapports[i-1] <- 100 * (2 * pop[i]) / denom
      }
    }
    return(rapports)
  }
  
  m_rapports <- calc_rapports(pop_m_quinquenal)
  f_rapports <- calc_rapports(pop_f_quinquenal)
  
  # Indices d'irrégularité
  J_m <- mean(abs(m_rapports - 100), na.rm = TRUE)
  J_f <- mean(abs(f_rapports - 100), na.rm = TRUE)
  
  # Rapport de masculinité
  rapports_masc <- 100 * pop_m_quinquenal / pop_f_quinquenal
  rapports_masc <- ifelse(is.infinite(rapports_masc) | is.nan(rapports_masc), 100, rapports_masc)
  
  # Variation du rapport de masculinité
  differences <- abs(diff(rapports_masc))
  K <- mean(differences, na.rm = TRUE)
  
  # Indice combiné brut
  I_brut <- 3 * K + J_m + J_f
  
  # Correction pour taille de population
  if (!is.null(taille_population) && taille_population > 0) {
    S_correction <- (3500 / sqrt(taille_population)) - 3
    I_net <- max(I_brut - S_correction, 0)
  } else {
    S_correction <- NA
    I_net <- I_brut
  }
  
  return(list(
    I_brut = I_brut,
    I_net = I_net,
    J_m = J_m,
    J_f = J_f,
    K = K,
    S_correction = S_correction
  ))
}