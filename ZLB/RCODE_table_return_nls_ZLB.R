setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
noms_modeles <- c("regress_CL_NLS_KUROV", "regress_GC_NLS_KUROV", "regress_SI_NLS_KUROV", 
                  "regress_PA_NLS_KUROV", "regress_HG_NLS_KUROV", "regress_NG_NLS_KUROV")

# Initialiser une liste pour stocker les modèles importés
modeles_importes <- list()

# Boucle pour charger chaque modèle
for(nom in noms_modeles) {
  chemin_fichier <- paste0(nom, ".rds") # Modifiez le chemin selon votre structure de dossiers
  modele <- readRDS(chemin_fichier)
  modeles_importes[[nom]] <- modele
}


add_significance_markers <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Fonction pour ajuster les coefficients et ajouter des astérisques
adjust_coefs_and_add_markers <- function(reg) {
  coef_df <- as.data.frame(summary(reg)$coefficients)
  coef_df$Estimate <- round(coef_df$Estimate * 100, 5) # Arrondir après multiplication
  # Ajouter des astérisques basés sur la p-value
  coef_df$Significance <- sapply(coef_df$`Pr(>|t|)`, add_significance_markers)
  # Sélectionner et renommer les colonnes pour inclure les astérisques avec les estimations
  coef_df <- coef_df[c("Estimate", "Significance")]
  names(coef_df)[1] <- "Coefficient (x100)"
  coef_df$Coefficient <- paste0(coef_df$`Coefficient (x100)`, coef_df$Significance)
  return(coef_df$Coefficient)
}
n1=nobs(modeles_importes$regress_CL_NLS_KUROV)
n2=nobs(modeles_importes$regress_GC_NLS_KUROV)
n3=nobs(modeles_importes$regress_SI_NLS_KUROV)
n4=nobs(modeles_importes$regress_HG_NLS_KUROV)
n5=nobs(modeles_importes$regress_PA_NLS_KUROV)
n6=nobs(modeles_importes$regress_NG_NLS_KUROV)
r1 <- round(summary(modeles_importes$regress_CL_NLS_KUROV)$r.squared, 6)
r2 <- round(summary(modeles_importes$regress_GC_NLS_KUROV)$r.squared, 6)
r3 <- round(summary(modeles_importes$regress_SI_NLS_KUROV)$r.squared, 6)
r4 <- round(summary(modeles_importes$regress_HG_NLS_KUROV)$r.squared, 6)
r5 <- round(summary(modeles_importes$regress_PA_NLS_KUROV)$r.squared, 6)
r6 <- round(summary(modeles_importes$regress_NG_NLS_KUROV)$r.squared, 6)


reg1=adjust_coefs_and_add_markers(modeles_importes$regress_CL_NLS_KUROV)
reg2=adjust_coefs_and_add_markers(modeles_importes$regress_GC_NLS_KUROV)
reg3=adjust_coefs_and_add_markers(modeles_importes$regress_SI_NLS_KUROV)
reg4=adjust_coefs_and_add_markers(modeles_importes$regress_HG_NLS_KUROV)
reg5=adjust_coefs_and_add_markers(modeles_importes$regress_PA_NLS_KUROV)
reg6=adjust_coefs_and_add_markers(modeles_importes$regress_NG_NLS_KUROV)
l1=c(reg1[2],reg1[27],reg2[2],reg2[26],reg3[2],reg3[26],reg4[2],reg4[26],reg5[2],reg5[26],reg6[2],reg6[27])
l2 <- c(reg1[3], reg1[28], reg2[3], reg2[27], reg3[3], reg3[27], reg4[3], reg4[27], reg5[3], reg5[27], reg6[3], reg6[28])
l3 <- c(reg1[4], reg1[29], reg2[4], reg2[28], reg3[4], reg3[28], reg4[4], reg4[28], reg5[4], reg5[28], reg6[4], reg6[29])
l4 <- c(reg1[5], reg1[30], reg2[5], reg2[29], reg3[5], reg3[29], reg4[5], reg4[29], reg5[5], reg5[29], reg6[5], reg6[30])
l5 <- c(reg1[6], reg1[31], reg2[6], reg2[30], reg3[6], reg3[30], reg4[6], reg4[30], reg5[6], reg5[30], reg6[6], reg6[31])
l6 <- c(reg1[7], reg1[32], reg2[7], reg2[31], reg3[7], reg3[31], reg4[7], reg4[31], reg5[7], reg5[31], reg6[7], reg6[32])
l7 <- c(reg1[8], reg1[33], reg2[8], reg2[32], reg3[8], reg3[32], reg4[8], reg4[32], reg5[8], reg5[32], reg6[8], reg6[33])
l8 <- c(reg1[9], reg1[34], reg2[9], reg2[33], reg3[9], reg3[33], reg4[9], reg4[33], reg5[9], reg5[33], reg6[9], reg6[34])
l9 <- c(reg1[10], reg1[35], reg2[10], reg2[34], reg3[10], reg3[34], reg4[10], reg4[34], reg5[10], reg5[34], reg6[10], reg6[35])
l10 <- c(reg1[11], reg1[36], reg2[11], reg2[35], reg3[11], reg3[35], reg4[11], reg4[35], reg5[11], reg5[35], reg6[11], reg6[36])
l11 <- c(reg1[12], reg1[37], reg2[12], reg2[36], reg3[12], reg3[36], reg4[12], reg4[36], reg5[12], reg5[36], reg6[12], reg6[37])
l12 <- c(reg1[13], reg1[38], reg2[13], reg2[37], reg3[13], reg3[37], reg4[13], reg4[37], reg5[13], reg5[37], reg6[13], reg6[38])
l13 <- c(reg1[14], reg1[39], reg2[14], reg2[38], reg3[14], reg3[38], reg4[14], reg4[38], reg5[14], reg5[38], reg6[14], reg6[39])
l14 <- c(reg1[15], reg1[40], reg2[15], reg2[39], reg3[15], reg3[39], reg4[15], reg4[39], reg5[15], reg5[39], reg6[15], reg6[40])
l15 <- c(reg1[16], reg1[41], reg2[16], reg2[40], reg3[16], reg3[40], reg4[16], reg4[40], reg5[16], reg5[40], reg6[16], reg6[41])
l16 <- c(reg1[17], reg1[42], reg2[17], reg2[41], reg3[17], reg3[41], reg4[17], reg4[41], reg5[17], reg5[41], reg6[17], reg6[42])
l17 <- c(reg1[18], reg1[43], reg2[18], reg2[42], reg3[18], reg3[42], reg4[18], reg4[42], reg5[18], reg5[42], reg6[18], reg6[43])
l18 <- c(reg1[19], reg1[44], reg2[19], reg2[43], reg3[19], reg3[43], reg4[19], reg4[43], reg5[19], reg5[43], reg6[19], reg6[44])
l19 <- c(reg1[20], reg1[45], reg2[20], reg2[44], reg3[20], reg3[44], reg4[20], reg4[44], reg5[20], reg5[44], reg6[20], reg6[45])
l20 <- c(reg1[21], reg1[46], reg2[21], reg2[45], reg3[21], reg3[45], reg4[21], reg4[45], reg5[21], reg5[45], reg6[21], reg6[46])
l21 <- c(reg1[22], reg1[47], reg2[22], reg2[46], reg3[22], reg3[46], reg4[22], reg4[46], reg5[22], reg5[46], reg6[22], reg6[47])
l22 <- c(reg1[23], reg1[48], reg2[23], reg2[47], reg3[23], reg3[47], reg4[23], reg4[47], reg5[23], reg5[47], reg6[23], reg6[48])
l23 <- c(reg1[24], reg1[49],"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"","")
l24 <- c("", "","" ,"" ,"" ,"" ,"" ,"" ,"" ,"" , reg6[24], reg6[49])


frame0<-paste("
\\documentclass{article}

% Packages nécessaires
\\usepackage{pdflscape}
\\usepackage{booktabs}
\\usepackage{graphicx} 
\\usepackage{threeparttable} 
\\usepackage{caption} 
\\usepackage{amsmath}
\\usepackage{natbib}
\\usepackage{longtable} 
\\usepackage{rotating} 

\\begin{document}
")
frame1<-paste("\\begin{sidewaystable}
\\caption{Announcement and Financialization Effects on Futures Returns  (full sample (2007-2023))}
\\label{tab:return-fin-full}
\\centering
\\resizebox{\\linewidth}{!}{%
\\begin{tabular}{@{}lllllllllllll@{}}
\\toprule
\\textbf{Commodities}         & \\multicolumn{2}{c}{\\textbf{Crude Oil}}    & \\multicolumn{2}{c}{\\textbf{Gold}}          & \\multicolumn{2}{c}{\\textbf{Copper}}       & \\multicolumn{2}{c}{\\textbf{Silver}}  & \\multicolumn{2}{c}{\\textbf{Palladium}}    & \\multicolumn{2}{c}{\\textbf{Natural Gas}}       \\\\ \\midrule
\\textbf{Announcements}       & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} \\\\ \\midrule")
frame <-paste0("\\multicolumn{13}{c}{\\textbf{Macroeconomic News Announcements}} \\\\ \\midrule")
tex1 <- paste("\\textbf{Initial jobless claims}&", paste(l1, collapse = " & "), "\\\\")
tex2 <- paste("\\textbf{ADP Employment}&", paste(l2, collapse = " & "), "\\\\")
tex3 <- paste("\\textbf{CB Consumer}&", paste(l3, collapse = " & "), "\\\\")
tex4 <- paste("\\textbf{Advance retail sales}&", paste(l4, collapse = " & "), "\\\\")
tex5 <- paste("\\textbf{Building permit}&", paste(l5, collapse = " & "), "\\\\")
tex6 <- paste("\\textbf{Construction spending}&", paste(l6, collapse = " & "), "\\\\")
tex7 <- paste("\\textbf{Consumer credit}&", paste(l7, collapse = " & "), "\\\\")
tex8 <- paste("\\textbf{Consumer price index}&", paste(l8, collapse = " & "), "\\\\")
tex9 <- paste("\\textbf{Durable goods orders}&", paste(l9, collapse = " & "), "\\\\")
tex10 <- paste("\\textbf{Existing home sales}&", paste(l10, collapse = " & "), "\\\\")
tex11 <- paste("\\textbf{Factory orders}&", paste(l11, collapse = " & "), "\\\\")
tex12 <- paste("\\textbf{GDP}&", paste(l12, collapse = " & "), "\\\\")
tex13 <- paste("\\textbf{Housing starts}&", paste(l13, collapse = " & "), "\\\\")
tex14 <- paste("\\textbf{Industrial production}&", paste(l14, collapse = " & "), "\\\\")
tex15 <- paste("\\textbf{Michigan Sentiment Index}&", paste(l15, collapse = " & "), "\\\\")
tex16 <- paste("\\textbf{New home sales}&", paste(l16, collapse = " & "), "\\\\")
tex18 <- paste("\\textbf{Pending home sales}&", paste(l18, collapse = " & "), "\\\\")
tex17 <- paste("\\textbf{Non-farm employment}&", paste(l17, collapse = " & "), "\\\\")
tex19 <- paste("\\textbf{Personal consumption}&", paste(l19, collapse = " & "), "\\\\")
tex20 <- paste("\\textbf{Personal income}&", paste(l20, collapse = " & "), "\\\\")
tex21 <- paste("\\textbf{Producer price index}&", paste(l21, collapse = " & "), "\\\\")
tex22 <- paste("\\textbf{Trade balance}&", paste(l22, collapse = " & "), "\\\\")
frame2 <-paste0(" \\midrule \\multicolumn{13}{c}{\\textbf{Announcements specific to commodity markets}} \\\\ \\midrule")
tex23 <- paste("\\textbf{Crude Oil Weekly inventory}&", paste(l23, collapse = " & "), "\\\\")
tex24 <- paste("\\textbf{Natural Gas Weekly inventory}&", paste(l24, collapse = " & "), "\\\\", " \\midrule")
frame3 <- paste("\\textbf{Observations}             &\\multicolumn{2}{c}{", n1, "}                                                 & \\multicolumn{2}{c}{", n2, "}                                                 & \\multicolumn{2}{c}{", n3, "}                                                 & \\multicolumn{2}{c}{",n4,"}                                                 & \\multicolumn{2}{c}{",n5,"}                                                   & \\multicolumn{2}{c}{" ,n6, "}                                                 \\\\")
frame4 <- paste("\\textbf{$R^2$}             &\\multicolumn{2}{c}{", r1, "}                                                 & \\multicolumn{2}{c}{", r2, "}                                                 & \\multicolumn{2}{c}{", r3, "}                                                 & \\multicolumn{2}{c}{",r4,"}                                                 & \\multicolumn{2}{c}{",r5,"}                                                   & \\multicolumn{2}{c}{" ,r6, "}                                                 \\\\","\\bottomrule")
frame5 <- paste("
\\end{tabular}
}
\\begin{tablenotes}
    \\singlespacing
    \\footnotesize
    This table presents estimates of eq. \\ref{eq:Model 1}, $R_{t}^{t+\\tau}=\\alpha+\\sum_{m=1}^{22} \\gamma_m S_{m,t}+ \\delta X_{t,i} + \\sum_{m=1}^{22} \\theta_m (S_{m,t} \\cdot X_t)+\\beta R_{t-\\tau}^{t}+\\epsilon_{t}$ using the method proposed by \\citet{kurov2019price} and financialization variable $X_{t,2}=NLS_t$. The $\\gamma_m$ coefficients capture the instantaneous change in return when an announcement has just occurred and especially if that announcement was unanticipated. The coefficients $\\theta_m$ capture the instantaneous change in return when an announcement has just occurred in conjunction with the level of financialization.
\\end{tablenotes}
\\end{sidewaystable}
\\end{document}")
sink("Table_return_nls_kurov_ZLB.tex")
cat(frame0,frame1,frame,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12,tex13,tex14,tex16,tex17,tex18,tex19,tex20,tex21,tex22,frame2,tex23,tex24,frame3,frame4,frame5)
sink()









setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
noms_modeles <- c("regress_CL_NLS_ANDERSON", "regress_GC_NLS_ANDERSON", "regress_SI_NLS_ANDERSON", 
                  "regress_PA_NLS_ANDERSON", "regress_HG_NLS_ANDERSON", "regress_NG_NLS_ANDERSON")

# Initialiser une liste pour stocker les modèles importés
modeles_importes <- list()

# Boucle pour charger chaque modèle
for(nom in noms_modeles) {
  chemin_fichier <- paste0(nom, ".rds") # Modifiez le chemin selon votre structure de dossiers
  modele <- readRDS(chemin_fichier)
  modeles_importes[[nom]] <- modele
}


add_significance_markers <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Fonction pour ajuster les coefficients et ajouter des astérisques
adjust_coefs_and_add_markers <- function(reg) {
  coef_df <- as.data.frame(summary(reg)$coefficients)
  coef_df$Estimate <- round(coef_df$Estimate * 100, 5) # Arrondir après multiplication
  # Ajouter des astérisques basés sur la p-value
  coef_df$Significance <- sapply(coef_df$`Pr(>|t|)`, add_significance_markers)
  # Sélectionner et renommer les colonnes pour inclure les astérisques avec les estimations
  coef_df <- coef_df[c("Estimate", "Significance")]
  names(coef_df)[1] <- "Coefficient (x100)"
  coef_df$Coefficient <- paste0(coef_df$`Coefficient (x100)`, coef_df$Significance)
  return(coef_df$Coefficient)
}
n1=nobs(modeles_importes$regress_CL_NLS_ANDERSON)
n2=nobs(modeles_importes$regress_GC_NLS_ANDERSON)
n3=nobs(modeles_importes$regress_SI_NLS_ANDERSON)
n4=nobs(modeles_importes$regress_HG_NLS_ANDERSON)
n5=nobs(modeles_importes$regress_PA_NLS_ANDERSON)
n6=nobs(modeles_importes$regress_NG_NLS_ANDERSON)
r1 <- round(summary(modeles_importes$regress_CL_NLS_ANDERSON)$r.squared, 6)
r2 <- round(summary(modeles_importes$regress_GC_NLS_ANDERSON)$r.squared, 6)
r3 <- round(summary(modeles_importes$regress_SI_NLS_ANDERSON)$r.squared, 6)
r4 <- round(summary(modeles_importes$regress_HG_NLS_ANDERSON)$r.squared, 6)
r5 <- round(summary(modeles_importes$regress_PA_NLS_ANDERSON)$r.squared, 6)
r6 <- round(summary(modeles_importes$regress_NG_NLS_ANDERSON)$r.squared, 6)


reg1=adjust_coefs_and_add_markers(modeles_importes$regress_CL_NLS_ANDERSON)
reg2=adjust_coefs_and_add_markers(modeles_importes$regress_GC_NLS_ANDERSON)
reg3=adjust_coefs_and_add_markers(modeles_importes$regress_SI_NLS_ANDERSON)
reg4=adjust_coefs_and_add_markers(modeles_importes$regress_HG_NLS_ANDERSON)
reg5=adjust_coefs_and_add_markers(modeles_importes$regress_PA_NLS_ANDERSON)
reg6=adjust_coefs_and_add_markers(modeles_importes$regress_NG_NLS_ANDERSON)
l1=c(reg1[2],reg1[27],reg2[2],reg2[26],reg3[2],reg3[26],reg4[2],reg4[26],reg5[2],reg5[26],reg6[2],reg6[27])
l2 <- c(reg1[3], reg1[28], reg2[3], reg2[27], reg3[3], reg3[27], reg4[3], reg4[27], reg5[3], reg5[27], reg6[3], reg6[28])
l3 <- c(reg1[4], reg1[29], reg2[4], reg2[28], reg3[4], reg3[28], reg4[4], reg4[28], reg5[4], reg5[28], reg6[4], reg6[29])
l4 <- c(reg1[5], reg1[30], reg2[5], reg2[29], reg3[5], reg3[29], reg4[5], reg4[29], reg5[5], reg5[29], reg6[5], reg6[30])
l5 <- c(reg1[6], reg1[31], reg2[6], reg2[30], reg3[6], reg3[30], reg4[6], reg4[30], reg5[6], reg5[30], reg6[6], reg6[31])
l6 <- c(reg1[7], reg1[32], reg2[7], reg2[31], reg3[7], reg3[31], reg4[7], reg4[31], reg5[7], reg5[31], reg6[7], reg6[32])
l7 <- c(reg1[8], reg1[33], reg2[8], reg2[32], reg3[8], reg3[32], reg4[8], reg4[32], reg5[8], reg5[32], reg6[8], reg6[33])
l8 <- c(reg1[9], reg1[34], reg2[9], reg2[33], reg3[9], reg3[33], reg4[9], reg4[33], reg5[9], reg5[33], reg6[9], reg6[34])
l9 <- c(reg1[10], reg1[35], reg2[10], reg2[34], reg3[10], reg3[34], reg4[10], reg4[34], reg5[10], reg5[34], reg6[10], reg6[35])
l10 <- c(reg1[11], reg1[36], reg2[11], reg2[35], reg3[11], reg3[35], reg4[11], reg4[35], reg5[11], reg5[35], reg6[11], reg6[36])
l11 <- c(reg1[12], reg1[37], reg2[12], reg2[36], reg3[12], reg3[36], reg4[12], reg4[36], reg5[12], reg5[36], reg6[12], reg6[37])
l12 <- c(reg1[13], reg1[38], reg2[13], reg2[37], reg3[13], reg3[37], reg4[13], reg4[37], reg5[13], reg5[37], reg6[13], reg6[38])
l13 <- c(reg1[14], reg1[39], reg2[14], reg2[38], reg3[14], reg3[38], reg4[14], reg4[38], reg5[14], reg5[38], reg6[14], reg6[39])
l14 <- c(reg1[15], reg1[40], reg2[15], reg2[39], reg3[15], reg3[39], reg4[15], reg4[39], reg5[15], reg5[39], reg6[15], reg6[40])
l15 <- c(reg1[16], reg1[41], reg2[16], reg2[40], reg3[16], reg3[40], reg4[16], reg4[40], reg5[16], reg5[40], reg6[16], reg6[41])
l16 <- c(reg1[17], reg1[42], reg2[17], reg2[41], reg3[17], reg3[41], reg4[17], reg4[41], reg5[17], reg5[41], reg6[17], reg6[42])
l17 <- c(reg1[18], reg1[43], reg2[18], reg2[42], reg3[18], reg3[42], reg4[18], reg4[42], reg5[18], reg5[42], reg6[18], reg6[43])
l18 <- c(reg1[19], reg1[44], reg2[19], reg2[43], reg3[19], reg3[43], reg4[19], reg4[43], reg5[19], reg5[43], reg6[19], reg6[44])
l19 <- c(reg1[20], reg1[45], reg2[20], reg2[44], reg3[20], reg3[44], reg4[20], reg4[44], reg5[20], reg5[44], reg6[20], reg6[45])
l20 <- c(reg1[21], reg1[46], reg2[21], reg2[45], reg3[21], reg3[45], reg4[21], reg4[45], reg5[21], reg5[45], reg6[21], reg6[46])
l21 <- c(reg1[22], reg1[47], reg2[22], reg2[46], reg3[22], reg3[46], reg4[22], reg4[46], reg5[22], reg5[46], reg6[22], reg6[47])
l22 <- c(reg1[23], reg1[48], reg2[23], reg2[47], reg3[23], reg3[47], reg4[23], reg4[47], reg5[23], reg5[47], reg6[23], reg6[48])
l23 <- c(reg1[24], reg1[49],"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"","")
l24 <- c("", "","" ,"" ,"" ,"" ,"" ,"" ,"" ,"" , reg6[24], reg6[49])


frame0<-paste("
\\documentclass{article}

% Packages nécessaires
\\usepackage{pdflscape}
\\usepackage{booktabs}
\\usepackage{graphicx} 
\\usepackage{threeparttable} 
\\usepackage{caption} 
\\usepackage{amsmath}
\\usepackage{natbib}
\\usepackage{longtable} 
\\usepackage{rotating} 

\\begin{document}
")
frame1<-paste("\\begin{sidewaystable}
\\caption{Announcement and Financialization Effects on Futures Returns  (full sample (2007-2023))}
\\label{tab:return-fin-full}
\\centering
\\resizebox{\\linewidth}{!}{%
\\begin{tabular}{@{}lllllllllllll@{}}
\\toprule
\\textbf{Commodities}         & \\multicolumn{2}{c}{\\textbf{Crude Oil}}    & \\multicolumn{2}{c}{\\textbf{Gold}}          & \\multicolumn{2}{c}{\\textbf{Copper}}       & \\multicolumn{2}{c}{\\textbf{Silver}}  & \\multicolumn{2}{c}{\\textbf{Palladium}}    & \\multicolumn{2}{c}{\\textbf{Natural Gas}}       \\\\ \\midrule
\\textbf{Announcements}       & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} \\\\ \\midrule")
frame <-paste0("\\multicolumn{13}{c}{\\textbf{Macroeconomic News Announcements}} \\\\ \\midrule")
tex1 <- paste("\\textbf{Initial jobless claims}&", paste(l1, collapse = " & "), "\\\\")
tex2 <- paste("\\textbf{ADP Employment}&", paste(l2, collapse = " & "), "\\\\")
tex3 <- paste("\\textbf{CB Consumer}&", paste(l3, collapse = " & "), "\\\\")
tex4 <- paste("\\textbf{Advance retail sales}&", paste(l4, collapse = " & "), "\\\\")
tex5 <- paste("\\textbf{Building permit}&", paste(l5, collapse = " & "), "\\\\")
tex6 <- paste("\\textbf{Construction spending}&", paste(l6, collapse = " & "), "\\\\")
tex7 <- paste("\\textbf{Consumer credit}&", paste(l7, collapse = " & "), "\\\\")
tex8 <- paste("\\textbf{Consumer price index}&", paste(l8, collapse = " & "), "\\\\")
tex9 <- paste("\\textbf{Durable goods orders}&", paste(l9, collapse = " & "), "\\\\")
tex10 <- paste("\\textbf{Existing home sales}&", paste(l10, collapse = " & "), "\\\\")
tex11 <- paste("\\textbf{Factory orders}&", paste(l11, collapse = " & "), "\\\\")
tex12 <- paste("\\textbf{GDP}&", paste(l12, collapse = " & "), "\\\\")
tex13 <- paste("\\textbf{Housing starts}&", paste(l13, collapse = " & "), "\\\\")
tex14 <- paste("\\textbf{Industrial production}&", paste(l14, collapse = " & "), "\\\\")
tex15 <- paste("\\textbf{Michigan Sentiment Index}&", paste(l15, collapse = " & "), "\\\\")
tex16 <- paste("\\textbf{New home sales}&", paste(l16, collapse = " & "), "\\\\")
tex18 <- paste("\\textbf{Pending home sales}&", paste(l18, collapse = " & "), "\\\\")
tex17 <- paste("\\textbf{Non-farm employment}&", paste(l17, collapse = " & "), "\\\\")
tex19 <- paste("\\textbf{Personal consumption}&", paste(l19, collapse = " & "), "\\\\")
tex20 <- paste("\\textbf{Personal income}&", paste(l20, collapse = " & "), "\\\\")
tex21 <- paste("\\textbf{Producer price index}&", paste(l21, collapse = " & "), "\\\\")
tex22 <- paste("\\textbf{Trade balance}&", paste(l22, collapse = " & "), "\\\\")
frame2 <-paste0(" \\midrule \\multicolumn{13}{c}{\\textbf{Announcements specific to commodity markets}} \\\\ \\midrule")
tex23 <- paste("\\textbf{Crude Oil Weekly inventory}&", paste(l23, collapse = " & "), "\\\\")
tex24 <- paste("\\textbf{Natural Gas Weekly inventory}&", paste(l24, collapse = " & "), "\\\\", " \\midrule")
frame3 <- paste("\\textbf{Observations}             &\\multicolumn{2}{c}{", n1, "}                                                 & \\multicolumn{2}{c}{", n2, "}                                                 & \\multicolumn{2}{c}{", n3, "}                                                 & \\multicolumn{2}{c}{",n4,"}                                                 & \\multicolumn{2}{c}{",n5,"}                                                   & \\multicolumn{2}{c}{" ,n6, "}                                                 \\\\")
frame4 <- paste("\\textbf{$R^2$}             &\\multicolumn{2}{c}{", r1, "}                                                 & \\multicolumn{2}{c}{", r2, "}                                                 & \\multicolumn{2}{c}{", r3, "}                                                 & \\multicolumn{2}{c}{",r4,"}                                                 & \\multicolumn{2}{c}{",r5,"}                                                   & \\multicolumn{2}{c}{" ,r6, "}                                                 \\\\","\\bottomrule")
frame5 <- paste("
\\end{tabular}
}
\\begin{tablenotes}
    \\singlespacing
    \\footnotesize
    This table presents estimates of eq. \\ref{eq:Model 1}, $R_{t}^{t+\\tau}=\\alpha+\\sum_{m=1}^{22} \\gamma_m S_{m,t}+ \\delta X_{t,i} + \\sum_{m=1}^{22} \\theta_m (S_{m,t} \\cdot X_t)+\\beta R_{t-\\tau}^{t}+\\epsilon_{t}$ using the method proposed by \\citet{ANDERSON2019price} and financialization variable $X_{t,2}=NLS_t$. The $\\gamma_m$ coefficients capture the instantaneous change in return when an announcement has just occurred and especially if that announcement was unanticipated. The coefficients $\\theta_m$ capture the instantaneous change in return when an announcement has just occurred in conjunction with the level of financialization.
\\end{tablenotes}
\\end{sidewaystable}
\\end{document}")
sink("Table_return_nls_anderson_ZLB.tex")
cat(frame0,frame1,frame,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12,tex13,tex14,tex16,tex17,tex18,tex19,tex20,tex21,tex22,frame2,tex23,tex24,frame3,frame4,frame5)
sink()


setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
noms_modeles <- c("regress_CL_MM_NLS_KUROV", "regress_GC_MM_NLS_KUROV", "regress_SI_MM_NLS_KUROV", 
                  "regress_PA_MM_NLS_KUROV", "regress_HG_MM_NLS_KUROV", "regress_NG_MM_NLS_KUROV")

# Initialiser une liste pour stocker les modèles importés
modeles_importes <- list()

# Boucle pour charger chaque modèle
for(nom in noms_modeles) {
  chemin_fichier <- paste0(nom, ".rds") # Modifiez le chemin selon votre structure de dossiers
  modele <- readRDS(chemin_fichier)
  modeles_importes[[nom]] <- modele
}


add_significance_markers <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Fonction pour ajuster les coefficients et ajouter des astérisques
adjust_coefs_and_add_markers <- function(reg) {
  coef_df <- as.data.frame(summary(reg)$coefficients)
  coef_df$Estimate <- round(coef_df$Estimate * 100, 5) # Arrondir après multiplication
  # Ajouter des astérisques basés sur la p-value
  coef_df$Significance <- sapply(coef_df$`Pr(>|t|)`, add_significance_markers)
  # Sélectionner et renommer les colonnes pour inclure les astérisques avec les estimations
  coef_df <- coef_df[c("Estimate", "Significance")]
  names(coef_df)[1] <- "Coefficient (x100)"
  coef_df$Coefficient <- paste0(coef_df$`Coefficient (x100)`, coef_df$Significance)
  return(coef_df$Coefficient)
}
n1=nobs(modeles_importes$regress_CL_MM_NLS_KUROV)
n2=nobs(modeles_importes$regress_GC_MM_NLS_KUROV)
n3=nobs(modeles_importes$regress_SI_MM_NLS_KUROV)
n4=nobs(modeles_importes$regress_HG_MM_NLS_KUROV)
n5=nobs(modeles_importes$regress_PA_MM_NLS_KUROV)
n6=nobs(modeles_importes$regress_NG_MM_NLS_KUROV)
r1 <- round(summary(modeles_importes$regress_CL_MM_NLS_KUROV)$r.squared, 6)
r2 <- round(summary(modeles_importes$regress_GC_MM_NLS_KUROV)$r.squared, 6)
r3 <- round(summary(modeles_importes$regress_SI_MM_NLS_KUROV)$r.squared, 6)
r4 <- round(summary(modeles_importes$regress_HG_MM_NLS_KUROV)$r.squared, 6)
r5 <- round(summary(modeles_importes$regress_PA_MM_NLS_KUROV)$r.squared, 6)
r6 <- round(summary(modeles_importes$regress_NG_MM_NLS_KUROV)$r.squared, 6)


reg1=adjust_coefs_and_add_markers(modeles_importes$regress_CL_MM_NLS_KUROV)
reg2=adjust_coefs_and_add_markers(modeles_importes$regress_GC_MM_NLS_KUROV)
reg3=adjust_coefs_and_add_markers(modeles_importes$regress_SI_MM_NLS_KUROV)
reg4=adjust_coefs_and_add_markers(modeles_importes$regress_HG_MM_NLS_KUROV)
reg5=adjust_coefs_and_add_markers(modeles_importes$regress_PA_MM_NLS_KUROV)
reg6=adjust_coefs_and_add_markers(modeles_importes$regress_NG_MM_NLS_KUROV)
l1=c(reg1[2],reg1[27],reg2[2],reg2[26],reg3[2],reg3[26],reg4[2],reg4[26],reg5[2],reg5[26],reg6[2],reg6[27])
l2 <- c(reg1[3], reg1[28], reg2[3], reg2[27], reg3[3], reg3[27], reg4[3], reg4[27], reg5[3], reg5[27], reg6[3], reg6[28])
l3 <- c(reg1[4], reg1[29], reg2[4], reg2[28], reg3[4], reg3[28], reg4[4], reg4[28], reg5[4], reg5[28], reg6[4], reg6[29])
l4 <- c(reg1[5], reg1[30], reg2[5], reg2[29], reg3[5], reg3[29], reg4[5], reg4[29], reg5[5], reg5[29], reg6[5], reg6[30])
l5 <- c(reg1[6], reg1[31], reg2[6], reg2[30], reg3[6], reg3[30], reg4[6], reg4[30], reg5[6], reg5[30], reg6[6], reg6[31])
l6 <- c(reg1[7], reg1[32], reg2[7], reg2[31], reg3[7], reg3[31], reg4[7], reg4[31], reg5[7], reg5[31], reg6[7], reg6[32])
l7 <- c(reg1[8], reg1[33], reg2[8], reg2[32], reg3[8], reg3[32], reg4[8], reg4[32], reg5[8], reg5[32], reg6[8], reg6[33])
l8 <- c(reg1[9], reg1[34], reg2[9], reg2[33], reg3[9], reg3[33], reg4[9], reg4[33], reg5[9], reg5[33], reg6[9], reg6[34])
l9 <- c(reg1[10], reg1[35], reg2[10], reg2[34], reg3[10], reg3[34], reg4[10], reg4[34], reg5[10], reg5[34], reg6[10], reg6[35])
l10 <- c(reg1[11], reg1[36], reg2[11], reg2[35], reg3[11], reg3[35], reg4[11], reg4[35], reg5[11], reg5[35], reg6[11], reg6[36])
l11 <- c(reg1[12], reg1[37], reg2[12], reg2[36], reg3[12], reg3[36], reg4[12], reg4[36], reg5[12], reg5[36], reg6[12], reg6[37])
l12 <- c(reg1[13], reg1[38], reg2[13], reg2[37], reg3[13], reg3[37], reg4[13], reg4[37], reg5[13], reg5[37], reg6[13], reg6[38])
l13 <- c(reg1[14], reg1[39], reg2[14], reg2[38], reg3[14], reg3[38], reg4[14], reg4[38], reg5[14], reg5[38], reg6[14], reg6[39])
l14 <- c(reg1[15], reg1[40], reg2[15], reg2[39], reg3[15], reg3[39], reg4[15], reg4[39], reg5[15], reg5[39], reg6[15], reg6[40])
l15 <- c(reg1[16], reg1[41], reg2[16], reg2[40], reg3[16], reg3[40], reg4[16], reg4[40], reg5[16], reg5[40], reg6[16], reg6[41])
l16 <- c(reg1[17], reg1[42], reg2[17], reg2[41], reg3[17], reg3[41], reg4[17], reg4[41], reg5[17], reg5[41], reg6[17], reg6[42])
l17 <- c(reg1[18], reg1[43], reg2[18], reg2[42], reg3[18], reg3[42], reg4[18], reg4[42], reg5[18], reg5[42], reg6[18], reg6[43])
l18 <- c(reg1[19], reg1[44], reg2[19], reg2[43], reg3[19], reg3[43], reg4[19], reg4[43], reg5[19], reg5[43], reg6[19], reg6[44])
l19 <- c(reg1[20], reg1[45], reg2[20], reg2[44], reg3[20], reg3[44], reg4[20], reg4[44], reg5[20], reg5[44], reg6[20], reg6[45])
l20 <- c(reg1[21], reg1[46], reg2[21], reg2[45], reg3[21], reg3[45], reg4[21], reg4[45], reg5[21], reg5[45], reg6[21], reg6[46])
l21 <- c(reg1[22], reg1[47], reg2[22], reg2[46], reg3[22], reg3[46], reg4[22], reg4[46], reg5[22], reg5[46], reg6[22], reg6[47])
l22 <- c(reg1[23], reg1[48], reg2[23], reg2[47], reg3[23], reg3[47], reg4[23], reg4[47], reg5[23], reg5[47], reg6[23], reg6[48])
l23 <- c(reg1[24], reg1[49],"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"","")
l24 <- c("", "","" ,"" ,"" ,"" ,"" ,"" ,"" ,"" , reg6[24], reg6[49])


frame0<-paste("
\\documentclass{article}

% Packages nécessaires
\\usepackage{pdflscape}
\\usepackage{booktabs}
\\usepackage{graphicx} 
\\usepackage{threeparttable} 
\\usepackage{caption} 
\\usepackage{amsmath}
\\usepackage{natbib}
\\usepackage{longtable} 
\\usepackage{rotating} 

\\begin{document}
")
frame1<-paste("\\begin{sidewaystable}
\\caption{Announcement and Financialization Effects on Futures Returns  (full sample (2007-2023))}
\\label{tab:return-fin-full}
\\centering
\\resizebox{\\linewidth}{!}{%
\\begin{tabular}{@{}lllllllllllll@{}}
\\toprule
\\textbf{Commodities}         & \\multicolumn{2}{c}{\\textbf{Crude Oil}}    & \\multicolumn{2}{c}{\\textbf{Gold}}          & \\multicolumn{2}{c}{\\textbf{Copper}}       & \\multicolumn{2}{c}{\\textbf{Silver}}  & \\multicolumn{2}{c}{\\textbf{Palladium}}    & \\multicolumn{2}{c}{\\textbf{Natural Gas}}       \\\\ \\midrule
\\textbf{Announcements}       & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} \\\\ \\midrule")
frame <-paste0("\\multicolumn{13}{c}{\\textbf{Macroeconomic News Announcements}} \\\\ \\midrule")
tex1 <- paste("\\textbf{Initial jobless claims}&", paste(l1, collapse = " & "), "\\\\")
tex2 <- paste("\\textbf{ADP Employment}&", paste(l2, collapse = " & "), "\\\\")
tex3 <- paste("\\textbf{CB Consumer}&", paste(l3, collapse = " & "), "\\\\")
tex4 <- paste("\\textbf{Advance retail sales}&", paste(l4, collapse = " & "), "\\\\")
tex5 <- paste("\\textbf{Building permit}&", paste(l5, collapse = " & "), "\\\\")
tex6 <- paste("\\textbf{Construction spending}&", paste(l6, collapse = " & "), "\\\\")
tex7 <- paste("\\textbf{Consumer credit}&", paste(l7, collapse = " & "), "\\\\")
tex8 <- paste("\\textbf{Consumer price index}&", paste(l8, collapse = " & "), "\\\\")
tex9 <- paste("\\textbf{Durable goods orders}&", paste(l9, collapse = " & "), "\\\\")
tex10 <- paste("\\textbf{Existing home sales}&", paste(l10, collapse = " & "), "\\\\")
tex11 <- paste("\\textbf{Factory orders}&", paste(l11, collapse = " & "), "\\\\")
tex12 <- paste("\\textbf{GDP}&", paste(l12, collapse = " & "), "\\\\")
tex13 <- paste("\\textbf{Housing starts}&", paste(l13, collapse = " & "), "\\\\")
tex14 <- paste("\\textbf{Industrial production}&", paste(l14, collapse = " & "), "\\\\")
tex15 <- paste("\\textbf{Michigan Sentiment Index}&", paste(l15, collapse = " & "), "\\\\")
tex16 <- paste("\\textbf{New home sales}&", paste(l16, collapse = " & "), "\\\\")
tex18 <- paste("\\textbf{Pending home sales}&", paste(l18, collapse = " & "), "\\\\")
tex17 <- paste("\\textbf{Non-farm employment}&", paste(l17, collapse = " & "), "\\\\")
tex19 <- paste("\\textbf{Personal consumption}&", paste(l19, collapse = " & "), "\\\\")
tex20 <- paste("\\textbf{Personal income}&", paste(l20, collapse = " & "), "\\\\")
tex21 <- paste("\\textbf{Producer price index}&", paste(l21, collapse = " & "), "\\\\")
tex22 <- paste("\\textbf{Trade balance}&", paste(l22, collapse = " & "), "\\\\")
frame2 <-paste0(" \\midrule \\multicolumn{13}{c}{\\textbf{Announcements specific to commodity markets}} \\\\ \\midrule")
tex23 <- paste("\\textbf{Crude Oil Weekly inventory}&", paste(l23, collapse = " & "), "\\\\")
tex24 <- paste("\\textbf{Natural Gas Weekly inventory}&", paste(l24, collapse = " & "), "\\\\", " \\midrule")
frame3 <- paste("\\textbf{Observations}             &\\multicolumn{2}{c}{", n1, "}                                                 & \\multicolumn{2}{c}{", n2, "}                                                 & \\multicolumn{2}{c}{", n3, "}                                                 & \\multicolumn{2}{c}{",n4,"}                                                 & \\multicolumn{2}{c}{",n5,"}                                                   & \\multicolumn{2}{c}{" ,n6, "}                                                 \\\\")
frame4 <- paste("\\textbf{$R^2$}             &\\multicolumn{2}{c}{", r1, "}                                                 & \\multicolumn{2}{c}{", r2, "}                                                 & \\multicolumn{2}{c}{", r3, "}                                                 & \\multicolumn{2}{c}{",r4,"}                                                 & \\multicolumn{2}{c}{",r5,"}                                                   & \\multicolumn{2}{c}{" ,r6, "}                                                 \\\\","\\bottomrule")
frame5 <- paste("
\\end{tabular}
}
\\begin{tablenotes}
    \\singlespacing
    \\footnotesize
    This table presents estimates of eq. \\ref{eq:Model 1}, $R_{t}^{t+\\tau}=\\alpha+\\sum_{m=1}^{22} \\gamma_m S_{m,t}+ \\delta X_{t,i} + \\sum_{m=1}^{22} \\theta_m (S_{m,t} \\cdot X_t)+\\beta R_{t-\\tau}^{t}+\\epsilon_{t}$ using the method proposed by \\citet{kurov2019price} and financialization variable $X_{t,2}=MM_NLS_t$. The $\\gamma_m$ coefficients capture the instantaneous change in return when an announcement has just occurred and especially if that announcement was unanticipated. The coefficients $\\theta_m$ capture the instantaneous change in return when an announcement has just occurred in conjunction with the level of financialization.
\\end{tablenotes}
\\end{sidewaystable}
\\end{document}")
sink("Table_return_MM_NLS_kurov_ZLB.tex")
cat(frame0,frame1,frame,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12,tex13,tex14,tex16,tex17,tex18,tex19,tex20,tex21,tex22,frame2,tex23,tex24,frame3,frame4,frame5)
sink()






setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
noms_modeles <- c("regress_CL_SWAP_NLS_KUROV", "regress_GC_SWAP_NLS_KUROV", "regress_SI_SWAP_NLS_KUROV", 
                  "regress_PA_SWAP_NLS_KUROV", "regress_HG_SWAP_NLS_KUROV", "regress_NG_SWAP_NLS_KUROV")

# Initialiser une liste pour stocker les modèles importés
modeles_importes <- list()

# Boucle pour charger chaque modèle
for(nom in noms_modeles) {
  chemin_fichier <- paste0(nom, ".rds") # Modifiez le chemin selon votre structure de dossiers
  modele <- readRDS(chemin_fichier)
  modeles_importes[[nom]] <- modele
}


add_significance_markers <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Fonction pour ajuster les coefficients et ajouter des astérisques
adjust_coefs_and_add_markers <- function(reg) {
  coef_df <- as.data.frame(summary(reg)$coefficients)
  coef_df$Estimate <- round(coef_df$Estimate * 100, 5) # Arrondir après multiplication
  # Ajouter des astérisques basés sur la p-value
  coef_df$Significance <- sapply(coef_df$`Pr(>|t|)`, add_significance_markers)
  # Sélectionner et renommer les colonnes pour inclure les astérisques avec les estimations
  coef_df <- coef_df[c("Estimate", "Significance")]
  names(coef_df)[1] <- "Coefficient (x100)"
  coef_df$Coefficient <- paste0(coef_df$`Coefficient (x100)`, coef_df$Significance)
  return(coef_df$Coefficient)
}
n1=nobs(modeles_importes$regress_CL_SWAP_NLS_KUROV)
n2=nobs(modeles_importes$regress_GC_SWAP_NLS_KUROV)
n3=nobs(modeles_importes$regress_SI_SWAP_NLS_KUROV)
n4=nobs(modeles_importes$regress_HG_SWAP_NLS_KUROV)
n5=nobs(modeles_importes$regress_PA_SWAP_NLS_KUROV)
n6=nobs(modeles_importes$regress_NG_SWAP_NLS_KUROV)
r1 <- round(summary(modeles_importes$regress_CL_SWAP_NLS_KUROV)$r.squared, 6)
r2 <- round(summary(modeles_importes$regress_GC_SWAP_NLS_KUROV)$r.squared, 6)
r3 <- round(summary(modeles_importes$regress_SI_SWAP_NLS_KUROV)$r.squared, 6)
r4 <- round(summary(modeles_importes$regress_HG_SWAP_NLS_KUROV)$r.squared, 6)
r5 <- round(summary(modeles_importes$regress_PA_SWAP_NLS_KUROV)$r.squared, 6)
r6 <- round(summary(modeles_importes$regress_NG_SWAP_NLS_KUROV)$r.squared, 6)


reg1=adjust_coefs_and_add_markers(modeles_importes$regress_CL_SWAP_NLS_KUROV)
reg2=adjust_coefs_and_add_markers(modeles_importes$regress_GC_SWAP_NLS_KUROV)
reg3=adjust_coefs_and_add_markers(modeles_importes$regress_SI_SWAP_NLS_KUROV)
reg4=adjust_coefs_and_add_markers(modeles_importes$regress_HG_SWAP_NLS_KUROV)
reg5=adjust_coefs_and_add_markers(modeles_importes$regress_PA_SWAP_NLS_KUROV)
reg6=adjust_coefs_and_add_markers(modeles_importes$regress_NG_SWAP_NLS_KUROV)
l1=c(reg1[2],reg1[27],reg2[2],reg2[26],reg3[2],reg3[26],reg4[2],reg4[26],reg5[2],reg5[26],reg6[2],reg6[27])
l2 <- c(reg1[3], reg1[28], reg2[3], reg2[27], reg3[3], reg3[27], reg4[3], reg4[27], reg5[3], reg5[27], reg6[3], reg6[28])
l3 <- c(reg1[4], reg1[29], reg2[4], reg2[28], reg3[4], reg3[28], reg4[4], reg4[28], reg5[4], reg5[28], reg6[4], reg6[29])
l4 <- c(reg1[5], reg1[30], reg2[5], reg2[29], reg3[5], reg3[29], reg4[5], reg4[29], reg5[5], reg5[29], reg6[5], reg6[30])
l5 <- c(reg1[6], reg1[31], reg2[6], reg2[30], reg3[6], reg3[30], reg4[6], reg4[30], reg5[6], reg5[30], reg6[6], reg6[31])
l6 <- c(reg1[7], reg1[32], reg2[7], reg2[31], reg3[7], reg3[31], reg4[7], reg4[31], reg5[7], reg5[31], reg6[7], reg6[32])
l7 <- c(reg1[8], reg1[33], reg2[8], reg2[32], reg3[8], reg3[32], reg4[8], reg4[32], reg5[8], reg5[32], reg6[8], reg6[33])
l8 <- c(reg1[9], reg1[34], reg2[9], reg2[33], reg3[9], reg3[33], reg4[9], reg4[33], reg5[9], reg5[33], reg6[9], reg6[34])
l9 <- c(reg1[10], reg1[35], reg2[10], reg2[34], reg3[10], reg3[34], reg4[10], reg4[34], reg5[10], reg5[34], reg6[10], reg6[35])
l10 <- c(reg1[11], reg1[36], reg2[11], reg2[35], reg3[11], reg3[35], reg4[11], reg4[35], reg5[11], reg5[35], reg6[11], reg6[36])
l11 <- c(reg1[12], reg1[37], reg2[12], reg2[36], reg3[12], reg3[36], reg4[12], reg4[36], reg5[12], reg5[36], reg6[12], reg6[37])
l12 <- c(reg1[13], reg1[38], reg2[13], reg2[37], reg3[13], reg3[37], reg4[13], reg4[37], reg5[13], reg5[37], reg6[13], reg6[38])
l13 <- c(reg1[14], reg1[39], reg2[14], reg2[38], reg3[14], reg3[38], reg4[14], reg4[38], reg5[14], reg5[38], reg6[14], reg6[39])
l14 <- c(reg1[15], reg1[40], reg2[15], reg2[39], reg3[15], reg3[39], reg4[15], reg4[39], reg5[15], reg5[39], reg6[15], reg6[40])
l15 <- c(reg1[16], reg1[41], reg2[16], reg2[40], reg3[16], reg3[40], reg4[16], reg4[40], reg5[16], reg5[40], reg6[16], reg6[41])
l16 <- c(reg1[17], reg1[42], reg2[17], reg2[41], reg3[17], reg3[41], reg4[17], reg4[41], reg5[17], reg5[41], reg6[17], reg6[42])
l17 <- c(reg1[18], reg1[43], reg2[18], reg2[42], reg3[18], reg3[42], reg4[18], reg4[42], reg5[18], reg5[42], reg6[18], reg6[43])
l18 <- c(reg1[19], reg1[44], reg2[19], reg2[43], reg3[19], reg3[43], reg4[19], reg4[43], reg5[19], reg5[43], reg6[19], reg6[44])
l19 <- c(reg1[20], reg1[45], reg2[20], reg2[44], reg3[20], reg3[44], reg4[20], reg4[44], reg5[20], reg5[44], reg6[20], reg6[45])
l20 <- c(reg1[21], reg1[46], reg2[21], reg2[45], reg3[21], reg3[45], reg4[21], reg4[45], reg5[21], reg5[45], reg6[21], reg6[46])
l21 <- c(reg1[22], reg1[47], reg2[22], reg2[46], reg3[22], reg3[46], reg4[22], reg4[46], reg5[22], reg5[46], reg6[22], reg6[47])
l22 <- c(reg1[23], reg1[48], reg2[23], reg2[47], reg3[23], reg3[47], reg4[23], reg4[47], reg5[23], reg5[47], reg6[23], reg6[48])
l23 <- c(reg1[24], reg1[49],"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"","")
l24 <- c("", "","" ,"" ,"" ,"" ,"" ,"" ,"" ,"" , reg6[24], reg6[49])


frame0<-paste("
\\documentclass{article}

% Packages nécessaires
\\usepackage{pdflscape}
\\usepackage{booktabs}
\\usepackage{graphicx} 
\\usepackage{threeparttable} 
\\usepackage{caption} 
\\usepackage{amsmath}
\\usepackage{natbib}
\\usepackage{longtable} 
\\usepackage{rotating} 

\\begin{document}
")
frame1<-paste("\\begin{sidewaystable}
\\caption{Announcement and Financialization Effects on Futures Returns  (full sample (2007-2023))}
\\label{tab:return-fin-full}
\\centering
\\resizebox{\\linewidth}{!}{%
\\begin{tabular}{@{}lllllllllllll@{}}
\\toprule
\\textbf{Commodities}         & \\multicolumn{2}{c}{\\textbf{Crude Oil}}    & \\multicolumn{2}{c}{\\textbf{Gold}}          & \\multicolumn{2}{c}{\\textbf{Copper}}       & \\multicolumn{2}{c}{\\textbf{Silver}}  & \\multicolumn{2}{c}{\\textbf{Palladium}}    & \\multicolumn{2}{c}{\\textbf{Natural Gas}}       \\\\ \\midrule
\\textbf{Announcements}       & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} \\\\ \\midrule")
frame <-paste0("\\multicolumn{13}{c}{\\textbf{Macroeconomic News Announcements}} \\\\ \\midrule")
tex1 <- paste("\\textbf{Initial jobless claims}&", paste(l1, collapse = " & "), "\\\\")
tex2 <- paste("\\textbf{ADP Employment}&", paste(l2, collapse = " & "), "\\\\")
tex3 <- paste("\\textbf{CB Consumer}&", paste(l3, collapse = " & "), "\\\\")
tex4 <- paste("\\textbf{Advance retail sales}&", paste(l4, collapse = " & "), "\\\\")
tex5 <- paste("\\textbf{Building permit}&", paste(l5, collapse = " & "), "\\\\")
tex6 <- paste("\\textbf{Construction spending}&", paste(l6, collapse = " & "), "\\\\")
tex7 <- paste("\\textbf{Consumer credit}&", paste(l7, collapse = " & "), "\\\\")
tex8 <- paste("\\textbf{Consumer price index}&", paste(l8, collapse = " & "), "\\\\")
tex9 <- paste("\\textbf{Durable goods orders}&", paste(l9, collapse = " & "), "\\\\")
tex10 <- paste("\\textbf{Existing home sales}&", paste(l10, collapse = " & "), "\\\\")
tex11 <- paste("\\textbf{Factory orders}&", paste(l11, collapse = " & "), "\\\\")
tex12 <- paste("\\textbf{GDP}&", paste(l12, collapse = " & "), "\\\\")
tex13 <- paste("\\textbf{Housing starts}&", paste(l13, collapse = " & "), "\\\\")
tex14 <- paste("\\textbf{Industrial production}&", paste(l14, collapse = " & "), "\\\\")
tex15 <- paste("\\textbf{Michigan Sentiment Index}&", paste(l15, collapse = " & "), "\\\\")
tex16 <- paste("\\textbf{New home sales}&", paste(l16, collapse = " & "), "\\\\")
tex18 <- paste("\\textbf{Pending home sales}&", paste(l18, collapse = " & "), "\\\\")
tex17 <- paste("\\textbf{Non-farm employment}&", paste(l17, collapse = " & "), "\\\\")
tex19 <- paste("\\textbf{Personal consumption}&", paste(l19, collapse = " & "), "\\\\")
tex20 <- paste("\\textbf{Personal income}&", paste(l20, collapse = " & "), "\\\\")
tex21 <- paste("\\textbf{Producer price index}&", paste(l21, collapse = " & "), "\\\\")
tex22 <- paste("\\textbf{Trade balance}&", paste(l22, collapse = " & "), "\\\\")
frame2 <-paste0(" \\midrule \\multicolumn{13}{c}{\\textbf{Announcements specific to commodity markets}} \\\\ \\midrule")
tex23 <- paste("\\textbf{Crude Oil Weekly inventory}&", paste(l23, collapse = " & "), "\\\\")
tex24 <- paste("\\textbf{Natural Gas Weekly inventory}&", paste(l24, collapse = " & "), "\\\\", " \\midrule")
frame3 <- paste("\\textbf{Observations}             &\\multicolumn{2}{c}{", n1, "}                                                 & \\multicolumn{2}{c}{", n2, "}                                                 & \\multicolumn{2}{c}{", n3, "}                                                 & \\multicolumn{2}{c}{",n4,"}                                                 & \\multicolumn{2}{c}{",n5,"}                                                   & \\multicolumn{2}{c}{" ,n6, "}                                                 \\\\")
frame4 <- paste("\\textbf{$R^2$}             &\\multicolumn{2}{c}{", r1, "}                                                 & \\multicolumn{2}{c}{", r2, "}                                                 & \\multicolumn{2}{c}{", r3, "}                                                 & \\multicolumn{2}{c}{",r4,"}                                                 & \\multicolumn{2}{c}{",r5,"}                                                   & \\multicolumn{2}{c}{" ,r6, "}                                                 \\\\","\\bottomrule")
frame5 <- paste("
\\end{tabular}
}
\\begin{tablenotes}
    \\singlespacing
    \\footnotesize
    This table presents estimates of eq. \\ref{eq:Model 1}, $R_{t}^{t+\\tau}=\\alpha+\\sum_{m=1}^{22} \\gamma_m S_{m,t}+ \\delta X_{t,i} + \\sum_{m=1}^{22} \\theta_m (S_{m,t} \\cdot X_t)+\\beta R_{t-\\tau}^{t}+\\epsilon_{t}$ using the method proposed by \\citet{kurov2019price} and financialization variable $X_{t,2}=SWAP_NLS_t$. The $\\gamma_m$ coefficients capture the instantaneous change in return when an announcement has just occurred and especially if that announcement was unanticipated. The coefficients $\\theta_m$ capture the instantaneous change in return when an announcement has just occurred in conjunction with the level of financialization.
\\end{tablenotes}
\\end{sidewaystable}
\\end{document}")
sink("Table_return_SWAP_NLS_kurov_ZLB.tex")
cat(frame0,frame1,frame,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12,tex13,tex14,tex16,tex17,tex18,tex19,tex20,tex21,tex22,frame2,tex23,tex24,frame3,frame4,frame5)
sink()





setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
noms_modeles <- c("regress_CL_MM_NLS_ANDERSON", "regress_GC_MM_NLS_ANDERSON", "regress_SI_MM_NLS_ANDERSON", 
                  "regress_PA_MM_NLS_ANDERSON", "regress_HG_MM_NLS_ANDERSON", "regress_NG_MM_NLS_ANDERSON")

# Initialiser une liste pour stocker les modèles importés
modeles_importes <- list()

# Boucle pour charger chaque modèle
for(nom in noms_modeles) {
  chemin_fichier <- paste0(nom, ".rds") # Modifiez le chemin selon votre structure de dossiers
  modele <- readRDS(chemin_fichier)
  modeles_importes[[nom]] <- modele
}


add_significance_markers <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Fonction pour ajuster les coefficients et ajouter des astérisques
adjust_coefs_and_add_markers <- function(reg) {
  coef_df <- as.data.frame(summary(reg)$coefficients)
  coef_df$Estimate <- round(coef_df$Estimate * 100, 5) # Arrondir après multiplication
  # Ajouter des astérisques basés sur la p-value
  coef_df$Significance <- sapply(coef_df$`Pr(>|t|)`, add_significance_markers)
  # Sélectionner et renommer les colonnes pour inclure les astérisques avec les estimations
  coef_df <- coef_df[c("Estimate", "Significance")]
  names(coef_df)[1] <- "Coefficient (x100)"
  coef_df$Coefficient <- paste0(coef_df$`Coefficient (x100)`, coef_df$Significance)
  return(coef_df$Coefficient)
}
n1=nobs(modeles_importes$regress_CL_MM_NLS_ANDERSON)
n2=nobs(modeles_importes$regress_GC_MM_NLS_ANDERSON)
n3=nobs(modeles_importes$regress_SI_MM_NLS_ANDERSON)
n4=nobs(modeles_importes$regress_HG_MM_NLS_ANDERSON)
n5=nobs(modeles_importes$regress_PA_MM_NLS_ANDERSON)
n6=nobs(modeles_importes$regress_NG_MM_NLS_ANDERSON)
r1 <- round(summary(modeles_importes$regress_CL_MM_NLS_ANDERSON)$r.squared, 6)
r2 <- round(summary(modeles_importes$regress_GC_MM_NLS_ANDERSON)$r.squared, 6)
r3 <- round(summary(modeles_importes$regress_SI_MM_NLS_ANDERSON)$r.squared, 6)
r4 <- round(summary(modeles_importes$regress_HG_MM_NLS_ANDERSON)$r.squared, 6)
r5 <- round(summary(modeles_importes$regress_PA_MM_NLS_ANDERSON)$r.squared, 6)
r6 <- round(summary(modeles_importes$regress_NG_MM_NLS_ANDERSON)$r.squared, 6)


reg1=adjust_coefs_and_add_markers(modeles_importes$regress_CL_MM_NLS_ANDERSON)
reg2=adjust_coefs_and_add_markers(modeles_importes$regress_GC_MM_NLS_ANDERSON)
reg3=adjust_coefs_and_add_markers(modeles_importes$regress_SI_MM_NLS_ANDERSON)
reg4=adjust_coefs_and_add_markers(modeles_importes$regress_HG_MM_NLS_ANDERSON)
reg5=adjust_coefs_and_add_markers(modeles_importes$regress_PA_MM_NLS_ANDERSON)
reg6=adjust_coefs_and_add_markers(modeles_importes$regress_NG_MM_NLS_ANDERSON)
l1=c(reg1[2],reg1[27],reg2[2],reg2[26],reg3[2],reg3[26],reg4[2],reg4[26],reg5[2],reg5[26],reg6[2],reg6[27])
l2 <- c(reg1[3], reg1[28], reg2[3], reg2[27], reg3[3], reg3[27], reg4[3], reg4[27], reg5[3], reg5[27], reg6[3], reg6[28])
l3 <- c(reg1[4], reg1[29], reg2[4], reg2[28], reg3[4], reg3[28], reg4[4], reg4[28], reg5[4], reg5[28], reg6[4], reg6[29])
l4 <- c(reg1[5], reg1[30], reg2[5], reg2[29], reg3[5], reg3[29], reg4[5], reg4[29], reg5[5], reg5[29], reg6[5], reg6[30])
l5 <- c(reg1[6], reg1[31], reg2[6], reg2[30], reg3[6], reg3[30], reg4[6], reg4[30], reg5[6], reg5[30], reg6[6], reg6[31])
l6 <- c(reg1[7], reg1[32], reg2[7], reg2[31], reg3[7], reg3[31], reg4[7], reg4[31], reg5[7], reg5[31], reg6[7], reg6[32])
l7 <- c(reg1[8], reg1[33], reg2[8], reg2[32], reg3[8], reg3[32], reg4[8], reg4[32], reg5[8], reg5[32], reg6[8], reg6[33])
l8 <- c(reg1[9], reg1[34], reg2[9], reg2[33], reg3[9], reg3[33], reg4[9], reg4[33], reg5[9], reg5[33], reg6[9], reg6[34])
l9 <- c(reg1[10], reg1[35], reg2[10], reg2[34], reg3[10], reg3[34], reg4[10], reg4[34], reg5[10], reg5[34], reg6[10], reg6[35])
l10 <- c(reg1[11], reg1[36], reg2[11], reg2[35], reg3[11], reg3[35], reg4[11], reg4[35], reg5[11], reg5[35], reg6[11], reg6[36])
l11 <- c(reg1[12], reg1[37], reg2[12], reg2[36], reg3[12], reg3[36], reg4[12], reg4[36], reg5[12], reg5[36], reg6[12], reg6[37])
l12 <- c(reg1[13], reg1[38], reg2[13], reg2[37], reg3[13], reg3[37], reg4[13], reg4[37], reg5[13], reg5[37], reg6[13], reg6[38])
l13 <- c(reg1[14], reg1[39], reg2[14], reg2[38], reg3[14], reg3[38], reg4[14], reg4[38], reg5[14], reg5[38], reg6[14], reg6[39])
l14 <- c(reg1[15], reg1[40], reg2[15], reg2[39], reg3[15], reg3[39], reg4[15], reg4[39], reg5[15], reg5[39], reg6[15], reg6[40])
l15 <- c(reg1[16], reg1[41], reg2[16], reg2[40], reg3[16], reg3[40], reg4[16], reg4[40], reg5[16], reg5[40], reg6[16], reg6[41])
l16 <- c(reg1[17], reg1[42], reg2[17], reg2[41], reg3[17], reg3[41], reg4[17], reg4[41], reg5[17], reg5[41], reg6[17], reg6[42])
l17 <- c(reg1[18], reg1[43], reg2[18], reg2[42], reg3[18], reg3[42], reg4[18], reg4[42], reg5[18], reg5[42], reg6[18], reg6[43])
l18 <- c(reg1[19], reg1[44], reg2[19], reg2[43], reg3[19], reg3[43], reg4[19], reg4[43], reg5[19], reg5[43], reg6[19], reg6[44])
l19 <- c(reg1[20], reg1[45], reg2[20], reg2[44], reg3[20], reg3[44], reg4[20], reg4[44], reg5[20], reg5[44], reg6[20], reg6[45])
l20 <- c(reg1[21], reg1[46], reg2[21], reg2[45], reg3[21], reg3[45], reg4[21], reg4[45], reg5[21], reg5[45], reg6[21], reg6[46])
l21 <- c(reg1[22], reg1[47], reg2[22], reg2[46], reg3[22], reg3[46], reg4[22], reg4[46], reg5[22], reg5[46], reg6[22], reg6[47])
l22 <- c(reg1[23], reg1[48], reg2[23], reg2[47], reg3[23], reg3[47], reg4[23], reg4[47], reg5[23], reg5[47], reg6[23], reg6[48])
l23 <- c(reg1[24], reg1[49],"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"","")
l24 <- c("", "","" ,"" ,"" ,"" ,"" ,"" ,"" ,"" , reg6[24], reg6[49])


frame0<-paste("
\\documentclass{article}

% Packages nécessaires
\\usepackage{pdflscape}
\\usepackage{booktabs}
\\usepackage{graphicx} 
\\usepackage{threeparttable} 
\\usepackage{caption} 
\\usepackage{amsmath}
\\usepackage{natbib}
\\usepackage{longtable} 
\\usepackage{rotating} 

\\begin{document}
")
frame1<-paste("\\begin{sidewaystable}
\\caption{Announcement and Financialization Effects on Futures Returns  (full sample (2007-2023))}
\\label{tab:return-fin-full}
\\centering
\\resizebox{\\linewidth}{!}{%
\\begin{tabular}{@{}lllllllllllll@{}}
\\toprule
\\textbf{Commodities}         & \\multicolumn{2}{c}{\\textbf{Crude Oil}}    & \\multicolumn{2}{c}{\\textbf{Gold}}          & \\multicolumn{2}{c}{\\textbf{Copper}}       & \\multicolumn{2}{c}{\\textbf{Silver}}  & \\multicolumn{2}{c}{\\textbf{Palladium}}    & \\multicolumn{2}{c}{\\textbf{Natural Gas}}       \\\\ \\midrule
\\textbf{Announcements}       & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} \\\\ \\midrule")
frame <-paste0("\\multicolumn{13}{c}{\\textbf{Macroeconomic News Announcements}} \\\\ \\midrule")
tex1 <- paste("\\textbf{Initial jobless claims}&", paste(l1, collapse = " & "), "\\\\")
tex2 <- paste("\\textbf{ADP Employment}&", paste(l2, collapse = " & "), "\\\\")
tex3 <- paste("\\textbf{CB Consumer}&", paste(l3, collapse = " & "), "\\\\")
tex4 <- paste("\\textbf{Advance retail sales}&", paste(l4, collapse = " & "), "\\\\")
tex5 <- paste("\\textbf{Building permit}&", paste(l5, collapse = " & "), "\\\\")
tex6 <- paste("\\textbf{Construction spending}&", paste(l6, collapse = " & "), "\\\\")
tex7 <- paste("\\textbf{Consumer credit}&", paste(l7, collapse = " & "), "\\\\")
tex8 <- paste("\\textbf{Consumer price index}&", paste(l8, collapse = " & "), "\\\\")
tex9 <- paste("\\textbf{Durable goods orders}&", paste(l9, collapse = " & "), "\\\\")
tex10 <- paste("\\textbf{Existing home sales}&", paste(l10, collapse = " & "), "\\\\")
tex11 <- paste("\\textbf{Factory orders}&", paste(l11, collapse = " & "), "\\\\")
tex12 <- paste("\\textbf{GDP}&", paste(l12, collapse = " & "), "\\\\")
tex13 <- paste("\\textbf{Housing starts}&", paste(l13, collapse = " & "), "\\\\")
tex14 <- paste("\\textbf{Industrial production}&", paste(l14, collapse = " & "), "\\\\")
tex15 <- paste("\\textbf{Michigan Sentiment Index}&", paste(l15, collapse = " & "), "\\\\")
tex16 <- paste("\\textbf{New home sales}&", paste(l16, collapse = " & "), "\\\\")
tex18 <- paste("\\textbf{Pending home sales}&", paste(l18, collapse = " & "), "\\\\")
tex17 <- paste("\\textbf{Non-farm employment}&", paste(l17, collapse = " & "), "\\\\")
tex19 <- paste("\\textbf{Personal consumption}&", paste(l19, collapse = " & "), "\\\\")
tex20 <- paste("\\textbf{Personal income}&", paste(l20, collapse = " & "), "\\\\")
tex21 <- paste("\\textbf{Producer price index}&", paste(l21, collapse = " & "), "\\\\")
tex22 <- paste("\\textbf{Trade balance}&", paste(l22, collapse = " & "), "\\\\")
frame2 <-paste0(" \\midrule \\multicolumn{13}{c}{\\textbf{Announcements specific to commodity markets}} \\\\ \\midrule")
tex23 <- paste("\\textbf{Crude Oil Weekly inventory}&", paste(l23, collapse = " & "), "\\\\")
tex24 <- paste("\\textbf{Natural Gas Weekly inventory}&", paste(l24, collapse = " & "), "\\\\", " \\midrule")
frame3 <- paste("\\textbf{Observations}             &\\multicolumn{2}{c}{", n1, "}                                                 & \\multicolumn{2}{c}{", n2, "}                                                 & \\multicolumn{2}{c}{", n3, "}                                                 & \\multicolumn{2}{c}{",n4,"}                                                 & \\multicolumn{2}{c}{",n5,"}                                                   & \\multicolumn{2}{c}{" ,n6, "}                                                 \\\\")
frame4 <- paste("\\textbf{$R^2$}             &\\multicolumn{2}{c}{", r1, "}                                                 & \\multicolumn{2}{c}{", r2, "}                                                 & \\multicolumn{2}{c}{", r3, "}                                                 & \\multicolumn{2}{c}{",r4,"}                                                 & \\multicolumn{2}{c}{",r5,"}                                                   & \\multicolumn{2}{c}{" ,r6, "}                                                 \\\\","\\bottomrule")
frame5 <- paste("
\\end{tabular}
}
\\begin{tablenotes}
    \\singlespacing
    \\footnotesize
    This table presents estimates of eq. \\ref{eq:Model 1}, $R_{t}^{t+\\tau}=\\alpha+\\sum_{m=1}^{22} \\gamma_m S_{m,t}+ \\delta X_{t,i} + \\sum_{m=1}^{22} \\theta_m (S_{m,t} \\cdot X_t)+\\beta R_{t-\\tau}^{t}+\\epsilon_{t}$ using the method proposed by \\citet{ANDERSON2019price} and financialization variable $X_{t,2}=MM_NLS_t$. The $\\gamma_m$ coefficients capture the instantaneous change in return when an announcement has just occurred and especially if that announcement was unanticipated. The coefficients $\\theta_m$ capture the instantaneous change in return when an announcement has just occurred in conjunction with the level of financialization.
\\end{tablenotes}
\\end{sidewaystable}
\\end{document}")
sink("Table_return_MM_NLS_anderson_ZLB.tex")
cat(frame0,frame1,frame,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12,tex13,tex14,tex16,tex17,tex18,tex19,tex20,tex21,tex22,frame2,tex23,tex24,frame3,frame4,frame5)
sink()




setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
noms_modeles <- c("regress_CL_SWAP_NLS_ANDERSON", "regress_GC_SWAP_NLS_ANDERSON", "regress_SI_SWAP_NLS_ANDERSON", 
                  "regress_PA_SWAP_NLS_ANDERSON", "regress_HG_SWAP_NLS_ANDERSON", "regress_NG_SWAP_NLS_ANDERSON")

# Initialiser une liste pour stocker les modèles importés
modeles_importes <- list()

# Boucle pour charger chaque modèle
for(nom in noms_modeles) {
  chemin_fichier <- paste0(nom, ".rds") # Modifiez le chemin selon votre structure de dossiers
  modele <- readRDS(chemin_fichier)
  modeles_importes[[nom]] <- modele
}


add_significance_markers <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Fonction pour ajuster les coefficients et ajouter des astérisques
adjust_coefs_and_add_markers <- function(reg) {
  coef_df <- as.data.frame(summary(reg)$coefficients)
  coef_df$Estimate <- round(coef_df$Estimate * 100, 5) # Arrondir après multiplication
  # Ajouter des astérisques basés sur la p-value
  coef_df$Significance <- sapply(coef_df$`Pr(>|t|)`, add_significance_markers)
  # Sélectionner et renommer les colonnes pour inclure les astérisques avec les estimations
  coef_df <- coef_df[c("Estimate", "Significance")]
  names(coef_df)[1] <- "Coefficient (x100)"
  coef_df$Coefficient <- paste0(coef_df$`Coefficient (x100)`, coef_df$Significance)
  return(coef_df$Coefficient)
}
n1=nobs(modeles_importes$regress_CL_SWAP_NLS_ANDERSON)
n2=nobs(modeles_importes$regress_GC_SWAP_NLS_ANDERSON)
n3=nobs(modeles_importes$regress_SI_SWAP_NLS_ANDERSON)
n4=nobs(modeles_importes$regress_HG_SWAP_NLS_ANDERSON)
n5=nobs(modeles_importes$regress_PA_SWAP_NLS_ANDERSON)
n6=nobs(modeles_importes$regress_NG_SWAP_NLS_ANDERSON)
r1 <- round(summary(modeles_importes$regress_CL_SWAP_NLS_ANDERSON)$r.squared, 6)
r2 <- round(summary(modeles_importes$regress_GC_SWAP_NLS_ANDERSON)$r.squared, 6)
r3 <- round(summary(modeles_importes$regress_SI_SWAP_NLS_ANDERSON)$r.squared, 6)
r4 <- round(summary(modeles_importes$regress_HG_SWAP_NLS_ANDERSON)$r.squared, 6)
r5 <- round(summary(modeles_importes$regress_PA_SWAP_NLS_ANDERSON)$r.squared, 6)
r6 <- round(summary(modeles_importes$regress_NG_SWAP_NLS_ANDERSON)$r.squared, 6)


reg1=adjust_coefs_and_add_markers(modeles_importes$regress_CL_SWAP_NLS_ANDERSON)
reg2=adjust_coefs_and_add_markers(modeles_importes$regress_GC_SWAP_NLS_ANDERSON)
reg3=adjust_coefs_and_add_markers(modeles_importes$regress_SI_SWAP_NLS_ANDERSON)
reg4=adjust_coefs_and_add_markers(modeles_importes$regress_HG_SWAP_NLS_ANDERSON)
reg5=adjust_coefs_and_add_markers(modeles_importes$regress_PA_SWAP_NLS_ANDERSON)
reg6=adjust_coefs_and_add_markers(modeles_importes$regress_NG_SWAP_NLS_ANDERSON)
l1=c(reg1[2],reg1[27],reg2[2],reg2[26],reg3[2],reg3[26],reg4[2],reg4[26],reg5[2],reg5[26],reg6[2],reg6[27])
l2 <- c(reg1[3], reg1[28], reg2[3], reg2[27], reg3[3], reg3[27], reg4[3], reg4[27], reg5[3], reg5[27], reg6[3], reg6[28])
l3 <- c(reg1[4], reg1[29], reg2[4], reg2[28], reg3[4], reg3[28], reg4[4], reg4[28], reg5[4], reg5[28], reg6[4], reg6[29])
l4 <- c(reg1[5], reg1[30], reg2[5], reg2[29], reg3[5], reg3[29], reg4[5], reg4[29], reg5[5], reg5[29], reg6[5], reg6[30])
l5 <- c(reg1[6], reg1[31], reg2[6], reg2[30], reg3[6], reg3[30], reg4[6], reg4[30], reg5[6], reg5[30], reg6[6], reg6[31])
l6 <- c(reg1[7], reg1[32], reg2[7], reg2[31], reg3[7], reg3[31], reg4[7], reg4[31], reg5[7], reg5[31], reg6[7], reg6[32])
l7 <- c(reg1[8], reg1[33], reg2[8], reg2[32], reg3[8], reg3[32], reg4[8], reg4[32], reg5[8], reg5[32], reg6[8], reg6[33])
l8 <- c(reg1[9], reg1[34], reg2[9], reg2[33], reg3[9], reg3[33], reg4[9], reg4[33], reg5[9], reg5[33], reg6[9], reg6[34])
l9 <- c(reg1[10], reg1[35], reg2[10], reg2[34], reg3[10], reg3[34], reg4[10], reg4[34], reg5[10], reg5[34], reg6[10], reg6[35])
l10 <- c(reg1[11], reg1[36], reg2[11], reg2[35], reg3[11], reg3[35], reg4[11], reg4[35], reg5[11], reg5[35], reg6[11], reg6[36])
l11 <- c(reg1[12], reg1[37], reg2[12], reg2[36], reg3[12], reg3[36], reg4[12], reg4[36], reg5[12], reg5[36], reg6[12], reg6[37])
l12 <- c(reg1[13], reg1[38], reg2[13], reg2[37], reg3[13], reg3[37], reg4[13], reg4[37], reg5[13], reg5[37], reg6[13], reg6[38])
l13 <- c(reg1[14], reg1[39], reg2[14], reg2[38], reg3[14], reg3[38], reg4[14], reg4[38], reg5[14], reg5[38], reg6[14], reg6[39])
l14 <- c(reg1[15], reg1[40], reg2[15], reg2[39], reg3[15], reg3[39], reg4[15], reg4[39], reg5[15], reg5[39], reg6[15], reg6[40])
l15 <- c(reg1[16], reg1[41], reg2[16], reg2[40], reg3[16], reg3[40], reg4[16], reg4[40], reg5[16], reg5[40], reg6[16], reg6[41])
l16 <- c(reg1[17], reg1[42], reg2[17], reg2[41], reg3[17], reg3[41], reg4[17], reg4[41], reg5[17], reg5[41], reg6[17], reg6[42])
l17 <- c(reg1[18], reg1[43], reg2[18], reg2[42], reg3[18], reg3[42], reg4[18], reg4[42], reg5[18], reg5[42], reg6[18], reg6[43])
l18 <- c(reg1[19], reg1[44], reg2[19], reg2[43], reg3[19], reg3[43], reg4[19], reg4[43], reg5[19], reg5[43], reg6[19], reg6[44])
l19 <- c(reg1[20], reg1[45], reg2[20], reg2[44], reg3[20], reg3[44], reg4[20], reg4[44], reg5[20], reg5[44], reg6[20], reg6[45])
l20 <- c(reg1[21], reg1[46], reg2[21], reg2[45], reg3[21], reg3[45], reg4[21], reg4[45], reg5[21], reg5[45], reg6[21], reg6[46])
l21 <- c(reg1[22], reg1[47], reg2[22], reg2[46], reg3[22], reg3[46], reg4[22], reg4[46], reg5[22], reg5[46], reg6[22], reg6[47])
l22 <- c(reg1[23], reg1[48], reg2[23], reg2[47], reg3[23], reg3[47], reg4[23], reg4[47], reg5[23], reg5[47], reg6[23], reg6[48])
l23 <- c(reg1[24], reg1[49],"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"","")
l24 <- c("", "","" ,"" ,"" ,"" ,"" ,"" ,"" ,"" , reg6[24], reg6[49])


frame0<-paste("
\\documentclass{article}

% Packages nécessaires
\\usepackage{pdflscape}
\\usepackage{booktabs}
\\usepackage{graphicx} 
\\usepackage{threeparttable} 
\\usepackage{caption} 
\\usepackage{amsmath}
\\usepackage{natbib}
\\usepackage{longtable} 
\\usepackage{rotating} 

\\begin{document}
")
frame1<-paste("\\begin{sidewaystable}
\\caption{Announcement and Financialization Effects on Futures Returns  (full sample (2007-2023))}
\\label{tab:return-fin-full}
\\centering
\\resizebox{\\linewidth}{!}{%
\\begin{tabular}{@{}lllllllllllll@{}}
\\toprule
\\textbf{Commodities}         & \\multicolumn{2}{c}{\\textbf{Crude Oil}}    & \\multicolumn{2}{c}{\\textbf{Gold}}          & \\multicolumn{2}{c}{\\textbf{Copper}}       & \\multicolumn{2}{c}{\\textbf{Silver}}  & \\multicolumn{2}{c}{\\textbf{Palladium}}    & \\multicolumn{2}{c}{\\textbf{Natural Gas}}       \\\\ \\midrule
\\textbf{Announcements}       & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} & \\multicolumn{1}{c}{$\\gamma_m$} & \\multicolumn{1}{c}{$\\theta_m$} \\\\ \\midrule")
frame <-paste0("\\multicolumn{13}{c}{\\textbf{Macroeconomic News Announcements}} \\\\ \\midrule")
tex1 <- paste("\\textbf{Initial jobless claims}&", paste(l1, collapse = " & "), "\\\\")
tex2 <- paste("\\textbf{ADP Employment}&", paste(l2, collapse = " & "), "\\\\")
tex3 <- paste("\\textbf{CB Consumer}&", paste(l3, collapse = " & "), "\\\\")
tex4 <- paste("\\textbf{Advance retail sales}&", paste(l4, collapse = " & "), "\\\\")
tex5 <- paste("\\textbf{Building permit}&", paste(l5, collapse = " & "), "\\\\")
tex6 <- paste("\\textbf{Construction spending}&", paste(l6, collapse = " & "), "\\\\")
tex7 <- paste("\\textbf{Consumer credit}&", paste(l7, collapse = " & "), "\\\\")
tex8 <- paste("\\textbf{Consumer price index}&", paste(l8, collapse = " & "), "\\\\")
tex9 <- paste("\\textbf{Durable goods orders}&", paste(l9, collapse = " & "), "\\\\")
tex10 <- paste("\\textbf{Existing home sales}&", paste(l10, collapse = " & "), "\\\\")
tex11 <- paste("\\textbf{Factory orders}&", paste(l11, collapse = " & "), "\\\\")
tex12 <- paste("\\textbf{GDP}&", paste(l12, collapse = " & "), "\\\\")
tex13 <- paste("\\textbf{Housing starts}&", paste(l13, collapse = " & "), "\\\\")
tex14 <- paste("\\textbf{Industrial production}&", paste(l14, collapse = " & "), "\\\\")
tex15 <- paste("\\textbf{Michigan Sentiment Index}&", paste(l15, collapse = " & "), "\\\\")
tex16 <- paste("\\textbf{New home sales}&", paste(l16, collapse = " & "), "\\\\")
tex18 <- paste("\\textbf{Pending home sales}&", paste(l18, collapse = " & "), "\\\\")
tex17 <- paste("\\textbf{Non-farm employment}&", paste(l17, collapse = " & "), "\\\\")
tex19 <- paste("\\textbf{Personal consumption}&", paste(l19, collapse = " & "), "\\\\")
tex20 <- paste("\\textbf{Personal income}&", paste(l20, collapse = " & "), "\\\\")
tex21 <- paste("\\textbf{Producer price index}&", paste(l21, collapse = " & "), "\\\\")
tex22 <- paste("\\textbf{Trade balance}&", paste(l22, collapse = " & "), "\\\\")
frame2 <-paste0(" \\midrule \\multicolumn{13}{c}{\\textbf{Announcements specific to commodity markets}} \\\\ \\midrule")
tex23 <- paste("\\textbf{Crude Oil Weekly inventory}&", paste(l23, collapse = " & "), "\\\\")
tex24 <- paste("\\textbf{Natural Gas Weekly inventory}&", paste(l24, collapse = " & "), "\\\\", " \\midrule")
frame3 <- paste("\\textbf{Observations}             &\\multicolumn{2}{c}{", n1, "}                                                 & \\multicolumn{2}{c}{", n2, "}                                                 & \\multicolumn{2}{c}{", n3, "}                                                 & \\multicolumn{2}{c}{",n4,"}                                                 & \\multicolumn{2}{c}{",n5,"}                                                   & \\multicolumn{2}{c}{" ,n6, "}                                                 \\\\")
frame4 <- paste("\\textbf{$R^2$}             &\\multicolumn{2}{c}{", r1, "}                                                 & \\multicolumn{2}{c}{", r2, "}                                                 & \\multicolumn{2}{c}{", r3, "}                                                 & \\multicolumn{2}{c}{",r4,"}                                                 & \\multicolumn{2}{c}{",r5,"}                                                   & \\multicolumn{2}{c}{" ,r6, "}                                                 \\\\","\\bottomrule")
frame5 <- paste("
\\end{tabular}
}
\\begin{tablenotes}
    \\singlespacing
    \\footnotesize
    This table presents estimates of eq. \\ref{eq:Model 1}, $R_{t}^{t+\\tau}=\\alpha+\\sum_{m=1}^{22} \\gamma_m S_{m,t}+ \\delta X_{t,i} + \\sum_{m=1}^{22} \\theta_m (S_{m,t} \\cdot X_t)+\\beta R_{t-\\tau}^{t}+\\epsilon_{t}$ using the method proposed by \\citet{ANDERSON2019price} and financialization variable $X_{t,2}=SWAP_NLS_t$. The $\\gamma_m$ coefficients capture the instantaneous change in return when an announcement has just occurred and especially if that announcement was unanticipated. The coefficients $\\theta_m$ capture the instantaneous change in return when an announcement has just occurred in conjunction with the level of financialization.
\\end{tablenotes}
\\end{sidewaystable}
\\end{document}")
sink("Table_return_SWAP_NLS_anderson_ZLB.tex")
cat(frame0,frame1,frame,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12,tex13,tex14,tex16,tex17,tex18,tex19,tex20,tex21,tex22,frame2,tex23,tex24,frame3,frame4,frame5)
sink()
