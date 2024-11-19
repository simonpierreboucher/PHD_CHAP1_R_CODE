setwd("/Volumes/ssd/PHD/CH1")
load("CL_PC_DB.Rdata")
X=CL_PC_DB
# Identification des colonnes à filtrer
ss_columns <- grep("^SS", names(X), value = TRUE)

# Filtrer les valeurs inférieures à -3 et supérieures à 3 pour toutes les colonnes SS et créer un nouveau dataframe filtré
X_filtered <- X
for (col in ss_columns) {
  X_filtered <- X_filtered[X_filtered[[col]] >= -3 & X_filtered[[col]] <= 3, ]
}

# Régression initiale sans valeurs extrêmes
regress <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + X_filtered$SSCBCC + 
                        X_filtered$SSARS + X_filtered$SSBP + X_filtered$SSConstS + 
                        X_filtered$SSConsC + X_filtered$SSCPI + X_filtered$SSDGO + 
                        X_filtered$SSEHS + X_filtered$SSFO + X_filtered$SSGDP + 
                        X_filtered$SSHS + X_filtered$SSIP + X_filtered$SSMCS + 
                        X_filtered$SSNHS + X_filtered$SSNFPR + X_filtered$SSPHS + 
                        X_filtered$SSPersoS + X_filtered$SSPersoI + X_filtered$SSPPI + 
                        X_filtered$SSTB + X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered)

# Calcul des poids pour la régression pondérée
w <- numeric(length = length(regress$residuals))
w[1] <- abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1 * abs(regress$residuals[i+1]) + 0.9 * w[i]
}
w <- w[1:i]
regress_weights <- 1 / (w^2)



# Régression pondérée avec les variables sélectionnées
regress_CL_NLS_KUROV <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + 
                                     X_filtered$SSCBCC + X_filtered$SSARS + 
                                     X_filtered$SSBP + X_filtered$SSConstS + 
                                     X_filtered$SSConsC + X_filtered$SSCPI + 
                                     X_filtered$SSDGO + X_filtered$SSEHS + 
                                     X_filtered$SSFO + X_filtered$SSGDP + 
                                     X_filtered$SSHS + X_filtered$SSIP + 
                                     X_filtered$SSMCS + X_filtered$SSNHS + 
                                     X_filtered$SSNFPR + X_filtered$SSPHS + 
                                     X_filtered$SSPersoS + X_filtered$SSPersoI + 
                                     X_filtered$SSPPI + X_filtered$SSTB + 
                                     X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered, weights = regress_weights)
 setwd("/Volumes/ssd/PHD/CH1/FULL_NO_OUTLIERS")
saveRDS(regress_CL_NLS_KUROV, "regress_CL_NLS_KUROV.rds")



setwd("/Volumes/ssd/PHD/CH1")
load("GC_PC_DB.Rdata")
X=GC_PC_DB
# Identification des colonnes à filtrer
ss_columns <- grep("^SS", names(X), value = TRUE)

# Filtrer les valeurs inférieures à -3 et supérieures à 3 pour toutes les colonnes SS et créer un nouveau dataframe filtré
X_filtered <- X
for (col in ss_columns) {
  X_filtered <- X_filtered[X_filtered[[col]] >= -3 & X_filtered[[col]] <= 3, ]
}

# Régression initiale sans valeurs extrêmes
regress <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + X_filtered$SSCBCC + 
                        X_filtered$SSARS + X_filtered$SSBP + X_filtered$SSConstS + 
                        X_filtered$SSConsC + X_filtered$SSCPI + X_filtered$SSDGO + 
                        X_filtered$SSEHS + X_filtered$SSFO + X_filtered$SSGDP + 
                        X_filtered$SSHS + X_filtered$SSIP + X_filtered$SSMCS + 
                        X_filtered$SSNHS + X_filtered$SSNFPR + X_filtered$SSPHS + 
                        X_filtered$SSPersoS + X_filtered$SSPersoI + X_filtered$SSPPI + 
                        X_filtered$SSTB + X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered)

# Calcul des poids pour la régression pondérée
w <- numeric(length = length(regress$residuals))
w[1] <- abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1 * abs(regress$residuals[i+1]) + 0.9 * w[i]
}
w <- w[1:i]
regress_weights <- 1 / (w^2)



# Régression pondérée avec les variables sélectionnées
regress_GC_NLS_KUROV <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + 
                                     X_filtered$SSCBCC + X_filtered$SSARS + 
                                     X_filtered$SSBP + X_filtered$SSConstS + 
                                     X_filtered$SSConsC + X_filtered$SSCPI + 
                                     X_filtered$SSDGO + X_filtered$SSEHS + 
                                     X_filtered$SSFO + X_filtered$SSGDP + 
                                     X_filtered$SSHS + X_filtered$SSIP + 
                                     X_filtered$SSMCS + X_filtered$SSNHS + 
                                     X_filtered$SSNFPR + X_filtered$SSPHS + 
                                     X_filtered$SSPersoS + X_filtered$SSPersoI + 
                                     X_filtered$SSPPI + X_filtered$SSTB + 
                                     X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered, weights = regress_weights)
setwd("/Volumes/ssd/PHD/CH1/FULL_NO_OUTLIERS")
saveRDS(regress_GC_NLS_KUROV, "regress_GC_NLS_KUROV.rds")


setwd("/Volumes/ssd/PHD/CH1")
load("SI_PC_DB.Rdata")
X=SI_PC_DB
# Identification des colonnes à filtrer
ss_columns <- grep("^SS", names(X), value = TRUE)

# Filtrer les valeurs inférieures à -3 et supérieures à 3 pour toutes les colonnes SS et créer un nouveau dataframe filtré
X_filtered <- X
for (col in ss_columns) {
  X_filtered <- X_filtered[X_filtered[[col]] >= -3 & X_filtered[[col]] <= 3, ]
}

# Régression initiale sans valeurs extrêmes
regress <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + X_filtered$SSCBCC + 
                        X_filtered$SSARS + X_filtered$SSBP + X_filtered$SSConstS + 
                        X_filtered$SSConsC + X_filtered$SSCPI + X_filtered$SSDGO + 
                        X_filtered$SSEHS + X_filtered$SSFO + X_filtered$SSGDP + 
                        X_filtered$SSHS + X_filtered$SSIP + X_filtered$SSMCS + 
                        X_filtered$SSNHS + X_filtered$SSNFPR + X_filtered$SSPHS + 
                        X_filtered$SSPersoS + X_filtered$SSPersoI + X_filtered$SSPPI + 
                        X_filtered$SSTB + X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered)

# Calcul des poids pour la régression pondérée
w <- numeric(length = length(regress$residuals))
w[1] <- abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1 * abs(regress$residuals[i+1]) + 0.9 * w[i]
}
w <- w[1:i]
regress_weights <- 1 / (w^2)



# Régression pondérée avec les variables sélectionnées
regress_SI_NLS_KUROV <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + 
                                     X_filtered$SSCBCC + X_filtered$SSARS + 
                                     X_filtered$SSBP + X_filtered$SSConstS + 
                                     X_filtered$SSConsC + X_filtered$SSCPI + 
                                     X_filtered$SSDGO + X_filtered$SSEHS + 
                                     X_filtered$SSFO + X_filtered$SSGDP + 
                                     X_filtered$SSHS + X_filtered$SSIP + 
                                     X_filtered$SSMCS + X_filtered$SSNHS + 
                                     X_filtered$SSNFPR + X_filtered$SSPHS + 
                                     X_filtered$SSPersoS + X_filtered$SSPersoI + 
                                     X_filtered$SSPPI + X_filtered$SSTB + 
                                     X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered, weights = regress_weights)
setwd("/Volumes/ssd/PHD/CH1/FULL_NO_OUTLIERS")
saveRDS(regress_SI_NLS_KUROV, "regress_SI_NLS_KUROV.rds")

setwd("/Volumes/ssd/PHD/CH1")
load("HG_PC_DB.Rdata")
X=HG_PC_DB
# Identification des colonnes à filtrer
ss_columns <- grep("^SS", names(X), value = TRUE)

# Filtrer les valeurs inférieures à -3 et supérieures à 3 pour toutes les colonnes SS et créer un nouveau dataframe filtré
X_filtered <- X
for (col in ss_columns) {
  X_filtered <- X_filtered[X_filtered[[col]] >= -3 & X_filtered[[col]] <= 3, ]
}

# Régression initiale sans valeurs extrêmes
regress <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + X_filtered$SSCBCC + 
                        X_filtered$SSARS + X_filtered$SSBP + X_filtered$SSConstS + 
                        X_filtered$SSConsC + X_filtered$SSCPI + X_filtered$SSDGO + 
                        X_filtered$SSEHS + X_filtered$SSFO + X_filtered$SSGDP + 
                        X_filtered$SSHS + X_filtered$SSIP + X_filtered$SSMCS + 
                        X_filtered$SSNHS + X_filtered$SSNFPR + X_filtered$SSPHS + 
                        X_filtered$SSPersoS + X_filtered$SSPersoI + X_filtered$SSPPI + 
                        X_filtered$SSTB + X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered)

# Calcul des poids pour la régression pondérée
w <- numeric(length = length(regress$residuals))
w[1] <- abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1 * abs(regress$residuals[i+1]) + 0.9 * w[i]
}
w <- w[1:i]
regress_weights <- 1 / (w^2)



# Régression pondérée avec les variables sélectionnées
regress_HG_NLS_KUROV <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + 
                                     X_filtered$SSCBCC + X_filtered$SSARS + 
                                     X_filtered$SSBP + X_filtered$SSConstS + 
                                     X_filtered$SSConsC + X_filtered$SSCPI + 
                                     X_filtered$SSDGO + X_filtered$SSEHS + 
                                     X_filtered$SSFO + X_filtered$SSGDP + 
                                     X_filtered$SSHS + X_filtered$SSIP + 
                                     X_filtered$SSMCS + X_filtered$SSNHS + 
                                     X_filtered$SSNFPR + X_filtered$SSPHS + 
                                     X_filtered$SSPersoS + X_filtered$SSPersoI + 
                                     X_filtered$SSPPI + X_filtered$SSTB + 
                                     X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered, weights = regress_weights)
setwd("/Volumes/ssd/PHD/CH1/FULL_NO_OUTLIERS")
saveRDS(regress_HG_NLS_KUROV, "regress_HG_NLS_KUROV.rds")


setwd("/Volumes/ssd/PHD/CH1")
load("PA_PC_DB.Rdata")
X=PA_PC_DB
# Identification des colonnes à filtrer
ss_columns <- grep("^SS", names(X), value = TRUE)

# Filtrer les valeurs inférieures à -3 et supérieures à 3 pour toutes les colonnes SS et créer un nouveau dataframe filtré
X_filtered <- X
for (col in ss_columns) {
  X_filtered <- X_filtered[X_filtered[[col]] >= -3 & X_filtered[[col]] <= 3, ]
}

# Régression initiale sans valeurs extrêmes
regress <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + X_filtered$SSCBCC + 
                        X_filtered$SSARS + X_filtered$SSBP + X_filtered$SSConstS + 
                        X_filtered$SSConsC + X_filtered$SSCPI + X_filtered$SSDGO + 
                        X_filtered$SSEHS + X_filtered$SSFO + X_filtered$SSGDP + 
                        X_filtered$SSHS + X_filtered$SSIP + X_filtered$SSMCS + 
                        X_filtered$SSNHS + X_filtered$SSNFPR + X_filtered$SSPHS + 
                        X_filtered$SSPersoS + X_filtered$SSPersoI + X_filtered$SSPPI + 
                        X_filtered$SSTB + X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered)

# Calcul des poids pour la régression pondérée
w <- numeric(length = length(regress$residuals))
w[1] <- abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1 * abs(regress$residuals[i+1]) + 0.9 * w[i]
}
w <- w[1:i]
regress_weights <- 1 / (w^2)



# Régression pondérée avec les variables sélectionnées
regress_PA_NLS_KUROV <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + 
                                     X_filtered$SSCBCC + X_filtered$SSARS + 
                                     X_filtered$SSBP + X_filtered$SSConstS + 
                                     X_filtered$SSConsC + X_filtered$SSCPI + 
                                     X_filtered$SSDGO + X_filtered$SSEHS + 
                                     X_filtered$SSFO + X_filtered$SSGDP + 
                                     X_filtered$SSHS + X_filtered$SSIP + 
                                     X_filtered$SSMCS + X_filtered$SSNHS + 
                                     X_filtered$SSNFPR + X_filtered$SSPHS + 
                                     X_filtered$SSPersoS + X_filtered$SSPersoI + 
                                     X_filtered$SSPPI + X_filtered$SSTB + 
                                     X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered, weights = regress_weights)
setwd("/Volumes/ssd/PHD/CH1/FULL_NO_OUTLIERS")
saveRDS(regress_PA_NLS_KUROV, "regress_PA_NLS_KUROV.rds")

setwd("/Volumes/ssd/PHD/CH1")
load("NG_PC_DB.Rdata")
X=NG_PC_DB
# Identification des colonnes à filtrer
ss_columns <- grep("^SS", names(X), value = TRUE)

# Filtrer les valeurs inférieures à -3 et supérieures à 3 pour toutes les colonnes SS et créer un nouveau dataframe filtré
X_filtered <- X
for (col in ss_columns) {
  X_filtered <- X_filtered[X_filtered[[col]] >= -3 & X_filtered[[col]] <= 3, ]
}

# Régression initiale sans valeurs extrêmes
regress <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + X_filtered$SSCBCC + 
                        X_filtered$SSARS + X_filtered$SSBP + X_filtered$SSConstS + 
                        X_filtered$SSConsC + X_filtered$SSCPI + X_filtered$SSDGO + 
                        X_filtered$SSEHS + X_filtered$SSFO + X_filtered$SSGDP + 
                        X_filtered$SSHS + X_filtered$SSIP + X_filtered$SSMCS + 
                        X_filtered$SSNHS + X_filtered$SSNFPR + X_filtered$SSPHS + 
                        X_filtered$SSPersoS + X_filtered$SSPersoI + X_filtered$SSPPI + 
                        X_filtered$SSTB + X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered)

# Calcul des poids pour la régression pondérée
w <- numeric(length = length(regress$residuals))
w[1] <- abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1 * abs(regress$residuals[i+1]) + 0.9 * w[i]
}
w <- w[1:i]
regress_weights <- 1 / (w^2)



# Régression pondérée avec les variables sélectionnées
regress_NG_NLS_KUROV <- lm(r_t1 ~ (X_filtered$SSIJC + X_filtered$SSADP + 
                                     X_filtered$SSCBCC + X_filtered$SSARS + 
                                     X_filtered$SSBP + X_filtered$SSConstS + 
                                     X_filtered$SSConsC + X_filtered$SSCPI + 
                                     X_filtered$SSDGO + X_filtered$SSEHS + 
                                     X_filtered$SSFO + X_filtered$SSGDP + 
                                     X_filtered$SSHS + X_filtered$SSIP + 
                                     X_filtered$SSMCS + X_filtered$SSNHS + 
                                     X_filtered$SSNFPR + X_filtered$SSPHS + 
                                     X_filtered$SSPersoS + X_filtered$SSPersoI + 
                                     X_filtered$SSPPI + X_filtered$SSTB + 
                                     X_filtered$SSAWCOS) * NLS + r_m1, data = X_filtered, weights = regress_weights)
setwd("/Volumes/ssd/PHD/CH1/FULL_NO_OUTLIERS")
saveRDS(regress_NG_NLS_KUROV, "regress_NG_NLS_KUROV.rds")
