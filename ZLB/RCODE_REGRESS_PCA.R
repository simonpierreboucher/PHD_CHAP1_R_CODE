
setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1")
load("CL_PC_ZLB_DB.Rdata")
X=CL_PC_ZLB_DB
library(FactoMineR)
XPCA <- PCA(X[, c("MSCT", "NLS", "WT")], graph = FALSE)
X$PCA <- XPCA$ind$coord[,1]

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
w <- numeric(length = length(regress$residuals))
w[1]<-abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1*abs(regress$residuals[i+1]) + 0.9*w[i]
}
w<-w[1:i]
regress.weights<-1/(w^2)
regress_CL_PCA_KUROV <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                          weights = regress.weights)
X$TIMEF=factor(X$HOUR.x)

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
regress.weights <- 1 / (fitted(lm(abs(regress$residuals) ~ X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS+X$TIMEF,data = X)))^2
regress_CL_PCA_ANDERSON <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                             weights = regress.weights)

X$S1<-ifelse(X$SSIJC==0,0,1)
X$S2<-ifelse(X$SSADP==0,0,1)
X$S3<-ifelse(X$SSCBCC==0,0,1)
X$S4<-ifelse(X$SSARS==0,0,1)
X$S5<-ifelse(X$SSBP==0,0,1)
X$S6<-ifelse(X$SSConstS==0,0,1)
X$S7<-ifelse(X$SSConsC==0,0,1)
X$S8<-ifelse(X$SSCPI==0,0,1)
X$S9<-ifelse(X$SSDGO==0,0,1)
X$S10<-ifelse(X$SSEHS==0,0,1)
X$S11<-ifelse(X$SSFO==0,0,1)
X$S12<-ifelse(X$SSGDP==0,0,1)
X$S13<-ifelse(X$SSHS==0,0,1)
X$S14<-ifelse(X$SSIP==0,0,1)
X$S15<-ifelse(X$SSMCS==0,0,1)
X$S16<-ifelse(X$SSNHS==0,0,1)
X$S17<-ifelse(X$SSNFPR==0,0,1)
X$S18<-ifelse(X$SSPHS==0,0,1)
X$S19<-ifelse(X$SSPersoS==0,0,1)
X$S20<-ifelse(X$SSPersoI==0,0,1)
X$S21<-ifelse(X$SSPPI==0,0,1)
X$S22<-ifelse(X$SSTB==0,0,1)
X$S23<-ifelse(X$SSAWCOS==0,0,1)

regress<-lm(r_t1~X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS+r_m1, data= X)
regress_CL_PCA_VOL<-lm(abs(regress$residuals) ~(S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15+S16+S17+S18+S19+S20+S21+S22+S23)*PCA+X$TIMEF,data = X)

setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
saveRDS(regress_CL_PCA_KUROV, "regress_CL_PCA_KUROV.rds")
saveRDS(regress_CL_PCA_ANDERSON, "regress_CL_PCA_ANDERSON.rds")
saveRDS(regress_CL_PCA_VOL, "regress_CL_PCA_VOL.rds")



setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1")
load("GC_PC_ZLB_DB.Rdata")
X=GC_PC_ZLB_DB

XPCA <- PCA(X[, c("MSCT", "NLS", "WT")], graph = FALSE)
X$PCA <- XPCA$ind$coord[,1]

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
w <- numeric(length = length(regress$residuals))
w[1]<-abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1*abs(regress$residuals[i+1]) + 0.9*w[i]
}
w<-w[1:i]
regress.weights<-1/(w^2)
regress_GC_PCA_KUROV <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                          weights = regress.weights)
X$TIMEF=factor(X$HOUR.x)

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
regress.weights <- 1 / (fitted(lm(abs(regress$residuals) ~ X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS+X$TIMEF,data = X)))^2
regress_GC_PCA_ANDERSON <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                             weights = regress.weights)

X$S1<-ifelse(X$SSIJC==0,0,1)
X$S2<-ifelse(X$SSADP==0,0,1)
X$S3<-ifelse(X$SSCBCC==0,0,1)
X$S4<-ifelse(X$SSARS==0,0,1)
X$S5<-ifelse(X$SSBP==0,0,1)
X$S6<-ifelse(X$SSConstS==0,0,1)
X$S7<-ifelse(X$SSConsC==0,0,1)
X$S8<-ifelse(X$SSCPI==0,0,1)
X$S9<-ifelse(X$SSDGO==0,0,1)
X$S10<-ifelse(X$SSEHS==0,0,1)
X$S11<-ifelse(X$SSFO==0,0,1)
X$S12<-ifelse(X$SSGDP==0,0,1)
X$S13<-ifelse(X$SSHS==0,0,1)
X$S14<-ifelse(X$SSIP==0,0,1)
X$S15<-ifelse(X$SSMCS==0,0,1)
X$S16<-ifelse(X$SSNHS==0,0,1)
X$S17<-ifelse(X$SSNFPR==0,0,1)
X$S18<-ifelse(X$SSPHS==0,0,1)
X$S19<-ifelse(X$SSPersoS==0,0,1)
X$S20<-ifelse(X$SSPersoI==0,0,1)
X$S21<-ifelse(X$SSPPI==0,0,1)
X$S22<-ifelse(X$SSTB==0,0,1)


regress<-lm(r_t1~X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+r_m1, data= X)
regress_GC_PCA_VOL<-lm(abs(regress$residuals) ~(S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15+S16+S17+S18+S19+S20+S21+S22)*PCA+X$TIMEF,data = X)


setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
saveRDS(regress_GC_PCA_KUROV, "regress_GC_PCA_KUROV.rds")
saveRDS(regress_GC_PCA_ANDERSON, "regress_GC_PCA_ANDERSON.rds")
saveRDS(regress_GC_PCA_VOL, "regress_GC_PCA_VOL.rds")









setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1")
load("SI_PC_ZLB_DB.Rdata")
X=SI_PC_ZLB_DB

XPCA <- PCA(X[, c("MSCT", "NLS", "WT")], graph = FALSE)
X$PCA <- XPCA$ind$coord[,1]

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
w <- numeric(length = length(regress$residuals))
w[1]<-abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1*abs(regress$residuals[i+1]) + 0.9*w[i]
}
w<-w[1:i]
regress.weights<-1/(w^2)
regress_SI_PCA_KUROV <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                          weights = regress.weights)
X$TIMEF=factor(X$HOUR.x)

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
regress.weights <- 1 / (fitted(lm(abs(regress$residuals) ~ X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS+X$TIMEF,data = X)))^2
regress_SI_PCA_ANDERSON <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                             weights = regress.weights)
X$S1<-ifelse(X$SSIJC==0,0,1)
X$S2<-ifelse(X$SSADP==0,0,1)
X$S3<-ifelse(X$SSCBCC==0,0,1)
X$S4<-ifelse(X$SSARS==0,0,1)
X$S5<-ifelse(X$SSBP==0,0,1)
X$S6<-ifelse(X$SSConstS==0,0,1)
X$S7<-ifelse(X$SSConsC==0,0,1)
X$S8<-ifelse(X$SSCPI==0,0,1)
X$S9<-ifelse(X$SSDGO==0,0,1)
X$S10<-ifelse(X$SSEHS==0,0,1)
X$S11<-ifelse(X$SSFO==0,0,1)
X$S12<-ifelse(X$SSGDP==0,0,1)
X$S13<-ifelse(X$SSHS==0,0,1)
X$S14<-ifelse(X$SSIP==0,0,1)
X$S15<-ifelse(X$SSMCS==0,0,1)
X$S16<-ifelse(X$SSNHS==0,0,1)
X$S17<-ifelse(X$SSNFPR==0,0,1)
X$S18<-ifelse(X$SSPHS==0,0,1)
X$S19<-ifelse(X$SSPersoS==0,0,1)
X$S20<-ifelse(X$SSPersoI==0,0,1)
X$S21<-ifelse(X$SSPPI==0,0,1)
X$S22<-ifelse(X$SSTB==0,0,1)


regress<-lm(r_t1~X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+r_m1, data= X)
regress_SI_PCA_VOL<-lm(abs(regress$residuals) ~(S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15+S16+S17+S18+S19+S20+S21+S22)*PCA+X$TIMEF,data = X)


setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
saveRDS(regress_SI_PCA_KUROV, "regress_SI_PCA_KUROV.rds")
saveRDS(regress_SI_PCA_ANDERSON, "regress_SI_PCA_ANDERSON.rds")
saveRDS(regress_SI_PCA_VOL, "regress_SI_PCA_VOL.rds")

setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1")
load("HG_PC_ZLB_DB.Rdata")
X=HG_PC_ZLB_DB

XPCA <- PCA(X[, c("MSCT", "NLS", "WT")], graph = FALSE)
X$PCA <- XPCA$ind$coord[,1]

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
w <- numeric(length = length(regress$residuals))
w[1]<-abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1*abs(regress$residuals[i+1]) + 0.9*w[i]
}
w<-w[1:i]
regress.weights<-1/(w^2)
regress_HG_PCA_KUROV <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                          weights = regress.weights)
X$TIMEF=factor(X$HOUR.x)

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
regress.weights <- 1 / (fitted(lm(abs(regress$residuals) ~ X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS+X$TIMEF,data = X)))^2
regress_HG_PCA_ANDERSON <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                             weights = regress.weights)

X$S1<-ifelse(X$SSIJC==0,0,1)
X$S2<-ifelse(X$SSADP==0,0,1)
X$S3<-ifelse(X$SSCBCC==0,0,1)
X$S4<-ifelse(X$SSARS==0,0,1)
X$S5<-ifelse(X$SSBP==0,0,1)
X$S6<-ifelse(X$SSConstS==0,0,1)
X$S7<-ifelse(X$SSConsC==0,0,1)
X$S8<-ifelse(X$SSCPI==0,0,1)
X$S9<-ifelse(X$SSDGO==0,0,1)
X$S10<-ifelse(X$SSEHS==0,0,1)
X$S11<-ifelse(X$SSFO==0,0,1)
X$S12<-ifelse(X$SSGDP==0,0,1)
X$S13<-ifelse(X$SSHS==0,0,1)
X$S14<-ifelse(X$SSIP==0,0,1)
X$S15<-ifelse(X$SSMCS==0,0,1)
X$S16<-ifelse(X$SSNHS==0,0,1)
X$S17<-ifelse(X$SSNFPR==0,0,1)
X$S18<-ifelse(X$SSPHS==0,0,1)
X$S19<-ifelse(X$SSPersoS==0,0,1)
X$S20<-ifelse(X$SSPersoI==0,0,1)
X$S21<-ifelse(X$SSPPI==0,0,1)
X$S22<-ifelse(X$SSTB==0,0,1)


regress<-lm(r_t1~X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+r_m1, data= X)
regress_HG_PCA_VOL<-lm(abs(regress$residuals) ~(S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15+S16+S17+S18+S19+S20+S21+S22)*PCA+X$TIMEF,data = X)

setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
saveRDS(regress_HG_PCA_KUROV, "regress_HG_PCA_KUROV.rds")
saveRDS(regress_HG_PCA_ANDERSON, "regress_HG_PCA_ANDERSON.rds")
saveRDS(regress_HG_PCA_VOL, "regress_HG_PCA_VOL.rds")

setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1")
load("PA_PC_ZLB_DB.Rdata")
X=PA_PC_ZLB_DB

XPCA <- PCA(X[, c("MSCT", "NLS", "WT")], graph = FALSE)
X$PCA <- XPCA$ind$coord[,1]

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
w <- numeric(length = length(regress$residuals))
w[1]<-abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1*abs(regress$residuals[i+1]) + 0.9*w[i]
}
w<-w[1:i]
regress.weights<-1/(w^2)
regress_PA_PCA_KUROV <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                          weights = regress.weights)
X$TIMEF=factor(X$HOUR.x)

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
regress.weights <- 1 / (fitted(lm(abs(regress$residuals) ~ X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS+X$TIMEF,data = X)))^2
regress_PA_PCA_ANDERSON <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                             weights = regress.weights)

X$S1<-ifelse(X$SSIJC==0,0,1)
X$S2<-ifelse(X$SSADP==0,0,1)
X$S3<-ifelse(X$SSCBCC==0,0,1)
X$S4<-ifelse(X$SSARS==0,0,1)
X$S5<-ifelse(X$SSBP==0,0,1)
X$S6<-ifelse(X$SSConstS==0,0,1)
X$S7<-ifelse(X$SSConsC==0,0,1)
X$S8<-ifelse(X$SSCPI==0,0,1)
X$S9<-ifelse(X$SSDGO==0,0,1)
X$S10<-ifelse(X$SSEHS==0,0,1)
X$S11<-ifelse(X$SSFO==0,0,1)
X$S12<-ifelse(X$SSGDP==0,0,1)
X$S13<-ifelse(X$SSHS==0,0,1)
X$S14<-ifelse(X$SSIP==0,0,1)
X$S15<-ifelse(X$SSMCS==0,0,1)
X$S16<-ifelse(X$SSNHS==0,0,1)
X$S17<-ifelse(X$SSNFPR==0,0,1)
X$S18<-ifelse(X$SSPHS==0,0,1)
X$S19<-ifelse(X$SSPersoS==0,0,1)
X$S20<-ifelse(X$SSPersoI==0,0,1)
X$S21<-ifelse(X$SSPPI==0,0,1)
X$S22<-ifelse(X$SSTB==0,0,1)


regress<-lm(r_t1~X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+r_m1, data= X)
regress_PA_PCA_VOL<-lm(abs(regress$residuals) ~(S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15+S16+S17+S18+S19+S20+S21+S22)*PCA+X$TIMEF,data = X)

setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
saveRDS(regress_PA_PCA_KUROV, "regress_PA_PCA_KUROV.rds")
saveRDS(regress_PA_PCA_ANDERSON, "regress_PA_PCA_ANDERSON.rds")
saveRDS(regress_PA_PCA_VOL, "regress_PA_PCA_VOL.rds")


setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1")
load("NG_PC_ZLB_DB.Rdata")
X=NG_PC_ZLB_DB

XPCA <- PCA(X[, c("MSCT", "NLS", "WT")], graph = FALSE)
X$PCA <- XPCA$ind$coord[,1]

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
w <- numeric(length = length(regress$residuals))
w[1]<-abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1*abs(regress$residuals[i+1]) + 0.9*w[i]
}
w<-w[1:i]
regress.weights<-1/(w^2)
regress_NG_PCA_KUROV <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                          weights = regress.weights)
X$TIMEF=factor(X$HOUR.x)

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X)
regress.weights <- 1 / (fitted(lm(abs(regress$residuals) ~ X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS+X$TIMEF,data = X)))^2
regress_NG_PCA_ANDERSON <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSAWCOS)*PCA+r_m1, data= X, 
                             weights = regress.weights)
X$S1<-ifelse(X$SSIJC==0,0,1)
X$S2<-ifelse(X$SSADP==0,0,1)
X$S3<-ifelse(X$SSCBCC==0,0,1)
X$S4<-ifelse(X$SSARS==0,0,1)
X$S5<-ifelse(X$SSBP==0,0,1)
X$S6<-ifelse(X$SSConstS==0,0,1)
X$S7<-ifelse(X$SSConsC==0,0,1)
X$S8<-ifelse(X$SSCPI==0,0,1)
X$S9<-ifelse(X$SSDGO==0,0,1)
X$S10<-ifelse(X$SSEHS==0,0,1)
X$S11<-ifelse(X$SSFO==0,0,1)
X$S12<-ifelse(X$SSGDP==0,0,1)
X$S13<-ifelse(X$SSHS==0,0,1)
X$S14<-ifelse(X$SSIP==0,0,1)
X$S15<-ifelse(X$SSMCS==0,0,1)
X$S16<-ifelse(X$SSNHS==0,0,1)
X$S17<-ifelse(X$SSNFPR==0,0,1)
X$S18<-ifelse(X$SSPHS==0,0,1)
X$S19<-ifelse(X$SSPersoS==0,0,1)
X$S20<-ifelse(X$SSPersoI==0,0,1)
X$S21<-ifelse(X$SSPPI==0,0,1)
X$S22<-ifelse(X$SSTB==0,0,1)
X$S23<-ifelse(X$SSNGI==0,0,1)

regress<-lm(r_t1~X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$SSNGI+r_m1, data= X)
regress_NG_PCA_VOL<-lm(abs(regress$residuals) ~(S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15+S16+S17+S18+S19+S20+S21+S22+S23)*PCA+X$TIMEF,data = X)


setwd("/Volumes/G-DRIVE ArmorATD/PHD/CH1/ZLB")
saveRDS(regress_NG_PCA_KUROV, "regress_NG_PCA_KUROV.rds")
saveRDS(regress_NG_PCA_ANDERSON, "regress_NG_PCA_ANDERSON.rds")
saveRDS(regress_NG_PCA_VOL, "regress_NG_PCA_VOL.rds")