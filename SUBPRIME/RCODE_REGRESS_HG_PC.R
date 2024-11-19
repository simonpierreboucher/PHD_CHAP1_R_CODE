setwd("/Volumes/Extreme SSD/PHD/CH1")
load("HG_PC_SUBPRIME_DB.Rdata")
X=HG_PC_SUBPRIME_DB
regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*NLS+r_m1, data= X)
w <- numeric(length = length(regress$residuals))
w[1]<-abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1*abs(regress$residuals[i+1]) + 0.9*w[i]
}
w<-w[1:i]
regress.weights<-1/(w^2)
regress_HG_NLS_KUROV <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*NLS+r_m1, data= X, 
                          weights = regress.weights)


regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*MSCT+r_m1, data= X)
w <- numeric(length = length(regress$residuals))
w[1]<-abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1*abs(regress$residuals[i+1]) + 0.9*w[i]
}
w<-w[1:i]
regress.weights<-1/(w^2)
regress_HG_MSCT_KUROV <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*MSCT+r_m1, data= X, 
                           weights = regress.weights)

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*WT+r_m1, data= X)
w <- numeric(length = length(regress$residuals))
w[1]<-abs(regress$residuals[1])
for (i in seq_along(regress$residuals)) {
  w[i+1] <- 0.1*abs(regress$residuals[i+1]) + 0.9*w[i]
}
w<-w[1:i]
regress.weights<-1/(w^2)
regress_HG_WT_KUROV <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*WT+r_m1, data= X, 
                         weights = regress.weights)

X$TIMEF=factor(X$HOUR.x)

regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*MSCT+r_m1, data= X)
regress.weights <- 1 / (fitted(lm(abs(regress$residuals) ~ X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$TIMEF,data = X)))^2
regress_HG_MSCT_ANDERSON <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*MSCT+r_m1, data= X, 
                              weights = regress.weights)



regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*NLS+r_m1, data= X)
regress.weights <- 1 / (fitted(lm(abs(regress$residuals) ~ X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$TIMEF,data = X)))^2
regress_HG_NLS_ANDERSON <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*NLS+r_m1, data= X, 
                             weights = regress.weights)


regress<-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*WT+r_m1, data= X)
regress.weights <- 1 / (fitted(lm(abs(regress$residuals) ~ X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB+X$TIMEF,data = X)))^2
regress_HG_WT_ANDERSON <-lm(r_t1~(X$SSIJC+X$SSADP+X$SSCBCC+X$SSARS+X$SSBP+X$SSConstS+X$SSConsC+X$SSCPI+X$SSDGO+X$SSEHS+X$SSFO+X$SSGDP+X$SSHS+X$SSIP+X$SSMCS+X$SSNHS+X$SSNFPR+X$SSPHS+X$SSPersoS+X$SSPersoI+X$SSPPI+X$SSTB)*WT+r_m1, data= X, 
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
regress_HG_MSCT_VOL<-lm(abs(regress$residuals) ~(S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15+S16+S17+S18+S19+S20+S21+S22)*MSCT+X$TIMEF,data = X)
regress_HG_NLS_VOL<-lm(abs(regress$residuals) ~(S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15+S16+S17+S18+S19+S20+S21+S22)*NLS+X$TIMEF,data = X)
regress_HG_WT_VOL<-lm(abs(regress$residuals) ~(S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15+S16+S17+S18+S19+S20+S21+S22)*WT+X$TIMEF,data = X)

setwd("/Volumes/Extreme SSD/PHD/CH1/SUBPRIME")
saveRDS(regress_HG_NLS_ANDERSON, "regress_HG_NLS_ANDERSON.rds")
saveRDS(regress_HG_MSCT_ANDERSON, "regress_HG_MSCT_ANDERSON.rds")
saveRDS(regress_HG_WT_ANDERSON, "regress_HG_WT_ANDERSON.rds")
saveRDS(regress_HG_NLS_KUROV, "regress_HG_NLS_KUROV.rds")
saveRDS(regress_HG_MSCT_KUROV, "regress_HG_MSCT_KUROV.rds")
saveRDS(regress_HG_WT_KUROV, "regress_HG_WT_KUROV.rds")

saveRDS(regress_HG_MSCT_VOL, "regress_HG_MSCT_VOL.rds")
saveRDS(regress_HG_NLS_VOL, "regress_HG_NLS_VOL.rds")
saveRDS(regress_HG_WT_VOL, "regress_HG_WT_VOL.rds")