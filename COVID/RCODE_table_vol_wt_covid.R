setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/COVID")
noms_modeles <- c("regress_CL_WT_VOL", "regress_GC_WT_VOL", "regress_SI_WT_VOL", 
                  "regress_PA_WT_VOL", "regress_HG_WT_VOL", "regress_NG_WT_VOL")

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
n1=nobs(modeles_importes$regress_CL_WT_VOL)
n2=nobs(modeles_importes$regress_GC_WT_VOL)
n3=nobs(modeles_importes$regress_SI_WT_VOL)
n4=nobs(modeles_importes$regress_HG_WT_VOL)
n5=nobs(modeles_importes$regress_PA_WT_VOL)
n6=nobs(modeles_importes$regress_NG_WT_VOL)
r1 <- round(summary(modeles_importes$regress_CL_WT_VOL)$r.squared, 6)
r2 <- round(summary(modeles_importes$regress_GC_WT_VOL)$r.squared, 6)
r3 <- round(summary(modeles_importes$regress_SI_WT_VOL)$r.squared, 6)
r4 <- round(summary(modeles_importes$regress_HG_WT_VOL)$r.squared, 6)
r5 <- round(summary(modeles_importes$regress_PA_WT_VOL)$r.squared, 6)
r6 <- round(summary(modeles_importes$regress_NG_WT_VOL)$r.squared, 6)


reg1=adjust_coefs_and_add_markers(modeles_importes$regress_CL_WT_VOL)
reg2=adjust_coefs_and_add_markers(modeles_importes$regress_GC_WT_VOL)
reg3=adjust_coefs_and_add_markers(modeles_importes$regress_SI_WT_VOL)
reg4=adjust_coefs_and_add_markers(modeles_importes$regress_HG_WT_VOL)
reg5=adjust_coefs_and_add_markers(modeles_importes$regress_PA_WT_VOL)
reg6=adjust_coefs_and_add_markers(modeles_importes$regress_NG_WT_VOL)

summary(modeles_importes$regress_CL_WT_VOL)
l1 <- c(reg1[2], reg1[49], reg2[2], reg2[48], reg3[2], reg3[48], reg4[2], reg4[48], reg5[2], reg5[48], reg6[2], reg6[49])
l2 <- c(reg1[3], reg1[50], reg2[3], reg2[49], reg3[3], reg3[49], reg4[3], reg4[49], reg5[3], reg5[49], reg6[3], reg6[50])
l3 <- c(reg1[4], reg1[51], reg2[4], reg2[50], reg3[4], reg3[50], reg4[4], reg4[50], reg5[4], reg5[50], reg6[4], reg6[51])
l4 <- c(reg1[5], reg1[52], reg2[5], reg2[51], reg3[5], reg3[51], reg4[5], reg4[51], reg5[5], reg5[51], reg6[5], reg6[52])
l5 <- c(reg1[6], reg1[53], reg2[6], reg2[52], reg3[6], reg3[52], reg4[6], reg4[52], reg5[6], reg5[52], reg6[6], reg6[53])
l6 <- c(reg1[7], reg1[54], reg2[7], reg2[53], reg3[7], reg3[53], reg4[7], reg4[53], reg5[7], reg5[53], reg6[7], reg6[54])
l7 <- c(reg1[8], reg1[55], reg2[8], reg2[54], reg3[8], reg3[54], reg4[8], reg4[54], reg5[8], reg5[54], reg6[8], reg6[55])
l8 <- c(reg1[9], reg1[56], reg2[9], reg2[55], reg3[9], reg3[55], reg4[9], reg4[55], reg5[9], reg5[55], reg6[9], reg6[56])
l9 <- c(reg1[10], reg1[57], reg2[10], reg2[56], reg3[10], reg3[56], reg4[10], reg4[56], reg5[10], reg5[56], reg6[10], reg6[57])
l10 <- c(reg1[11], reg1[58], reg2[11], reg2[57], reg3[11], reg3[57], reg4[11], reg4[57], reg5[11], reg5[57], reg6[11], reg6[58])
l11 <- c(reg1[12], reg1[59], reg2[12], reg2[58], reg3[12], reg3[58], reg4[12], reg4[58], reg5[12], reg5[58], reg6[12], reg6[59])
l12 <- c(reg1[13], reg1[60], reg2[13], reg2[59], reg3[13], reg3[59], reg4[13], reg4[59], reg5[13], reg5[59], reg6[13], reg6[60])
l13 <- c(reg1[14], reg1[61], reg2[14], reg2[60], reg3[14], reg3[60], reg4[14], reg4[60], reg5[14], reg5[60], reg6[14], reg6[61])
l14 <- c(reg1[15], reg1[62], reg2[15], reg2[61], reg3[15], reg3[61], reg4[15], reg4[61], reg5[15], reg5[61], reg6[15], reg6[62])
l15 <- c(reg1[16], reg1[63], reg2[16], reg2[62], reg3[16], reg3[62], reg4[16], reg4[62], reg5[16], reg5[62], reg6[16], reg6[63])
l16 <- c(reg1[17], reg1[64], reg2[17], reg2[63], reg3[17], reg3[63], reg4[17], reg4[63], reg5[17], reg5[63], reg6[17], reg6[64])
l17 <- c(reg1[18], reg1[65], reg2[18], reg2[64], reg3[18], reg3[64], reg4[18], reg4[64], reg5[18], reg5[64], reg6[18], reg6[65])
l18 <- c(reg1[19], reg1[66], reg2[19], reg2[65], reg3[19], reg3[65], reg4[19], reg4[65], reg5[19], reg5[65], reg6[19], reg6[66])
l19 <- c(reg1[20], reg1[67], reg2[20], reg2[66], reg3[20], reg3[66], reg4[20], reg4[66], reg5[20], reg5[66], reg6[20], reg6[67])
l20 <- c(reg1[21], reg1[68], reg2[21], reg2[67], reg3[21], reg3[67], reg4[21], reg4[67], reg5[21], reg5[67], reg6[21], reg6[68])
l21 <- c(reg1[22], reg1[69], reg2[22], reg2[68], reg3[22], reg3[68], reg4[22], reg4[68], reg5[22], reg5[68], reg6[22], reg6[69])
l22 <- c(reg1[23], reg1[70], reg2[23], reg2[69], reg3[23], reg3[69], reg4[23], reg4[69], reg5[23], reg5[69], reg6[23], reg6[70])
l23 <- c(reg1[24], reg1[71],"" ,"" ,"" ,"" ,"" ,"" ,"" ,"" ,"","")
l24 <- c("", "","" ,"" ,"" ,"" ,"" ,"" ,"" ,"" , reg6[24], reg6[71])


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
    This table presents estimates of eq. \\ref{eq:Model 1}, $R_{t}^{t+\\tau}=\\alpha+\\sum_{m=1}^{22} \\gamma_m S_{m,t}+ \\delta X_{t,i} + \\sum_{m=1}^{22} \\theta_m (S_{m,t} \\cdot X_t)+\\beta R_{t-\\tau}^{t}+\\epsilon_{t}$ using the method proposed by \\citet{ANDERSON2019price} and financialization variable $X_{t,2}=WT_t$. The $\\gamma_m$ coefficients capture the instantaneous change in return when an announcement has just occurred and especially if that announcement was unanticipated. The coefficients $\\theta_m$ capture the instantaneous change in return when an announcement has just occurred in conjunction with the level of financialization.
\\end{tablenotes}
\\end{sidewaystable}
\\end{document}")
sink("Table_vol_WT_covid.tex")
cat(frame0,frame1,frame,tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12,tex13,tex14,tex16,tex17,tex18,tex19,tex20,tex21,tex22,frame2,tex23,tex24,frame3,frame4,frame5)
sink()

