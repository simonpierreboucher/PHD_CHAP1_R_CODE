## Commodities, MNA, CFTC merge database processing 
### Simon-Pierre Boucher 

# Load necessary libraries for data manipulation, analysis, and visualization
library(xts)
library(tidyverse) # For data manipulation and visualization
library(stargazer) # For creating well-formatted tables
library(xts) # For time series analysis
library(lubridate) # For handling date-times
library("highfrequency") # For high-frequency data analysis
library(tseries) # For time series analysis
library(texreg) # For regression output formatting
library("nlme") # For linear and non-linear mixed effects models

# Set working directory to where the data files are located
setwd("/Volumes/LaCie/PHD/CH1")



# Read commodity data, set column names, and transform date and time information
X<-read.csv("cl-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open # Calculate returns
X$t<-X$HOUR*60+X$MINUTE # Convert time to minutes

# Prepare for merge by adjusting time for next period and selecting key columns
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'

# Merge the original data with the shifted data by date and time
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

# Load external macroeconomic and CFTC Commitments of Traders (COT) data
load("MNA_DATA_ALL.Rdata")
X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("CL_PC.Rda")

# Prepare commodity-specific data, merge with main dataset, and handle missing values
Y=CL_PC
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
CL_PC_DB=X
save(CL_PC_DB,file = "CL_PC_DB.Rdata")



# The above steps are repeated for each commodity (gc, si, hg, pa, ng) 
# with their respective files and data manipulations.


X<-read.csv("gc-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("GC_PC.Rda")

Y=GC_PC
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
GC_PC_DB=X
save(GC_PC_DB,file = "GC_PC_DB.Rdata")

X<-read.csv("si-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("SI_PC.Rda")

Y=SI_PC
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
SI_PC_DB=X
save(SI_PC_DB,file = "SI_PC_DB.Rdata")


X<-read.csv("hg-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("HG_PC.Rda")

Y=HG_PC
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
HG_PC_DB=X
save(HG_PC_DB,file = "HG_PC_DB.Rdata")

X<-read.csv("pa-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("PA_PC.Rda")

Y=PA_PC
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
PA_PC_DB=X
save(PA_PC_DB,file = "PA_PC_DB.Rdata")


X<-read.csv("ng-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("NG_PC.Rda")

Y=NG_PC
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
NG_PC_DB=X
save(NG_PC_DB,file = "NG_PC_DB.Rdata")


X<-read.csv("cl-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("CL_COT.Rda")

Y=CL_COT
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
CL_COT_DB=X
save(CL_COT_DB,file = "CL_COT_DB.Rdata")


X<-read.csv("gc-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("GC_COT.Rda")

Y=GC_COT
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
GC_COT_DB=X
save(GC_COT_DB,file = "GC_COT_DB.Rdata")

X<-read.csv("si-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("SI_COT.Rda")

Y=SI_COT
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
SI_COT_DB=X
save(SI_COT_DB,file = "SI_COT_DB.Rdata")


X<-read.csv("hg-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("HG_COT.Rda")

Y=HG_COT
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
HG_COT_DB=X
save(HG_COT_DB,file = "HG_COT_DB.Rdata")

X<-read.csv("pa-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("PA_COT.Rda")

Y=PA_COT
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
PA_COT_DB=X
save(PA_COT_DB,file = "PA_COT_DB.Rdata")


X<-read.csv("ng-5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("MNA_DATA_ALL.Rdata")


X <- merge(X, MACRO,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)
load("NG_COT.Rda")

Y=NG_COT
Y$YEAR<-as.numeric(substr(Y$Date,1,4))
Y$MONTH<-as.numeric(substr(Y$Date,6,7))
Y$DAY<-as.numeric(substr(Y$Date,9,10))
X <- merge(X, Y,by=c("DAY","MONTH","YEAR"),all.x = TRUE)
X[is.na(X)] <- 0
X <- X[order(X$YEAR, X$MONTH,X$DAY,X$t),]
NG_COT_DB=X
save(NG_COT_DB,file = "NG_COT_DB.Rdata")



# After all commodities are processed, create subsets of data based on specific date ranges
# such as the Zero Lower Bound (ZLB) period and the COVID-19 period.


# For example, for the CL commodity during the ZLB period:

df=CL_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
CL_PC_ZLB_DB=filtered_df


# Similar steps are repeated for creating COVID and ZLB datasets for all commodities
# and their corresponding COT data, and saving them for future use.

df=GC_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
GC_PC_ZLB_DB=filtered_df



df=SI_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
SI_PC_ZLB_DB=filtered_df


df=HG_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
HG_PC_ZLB_DB=filtered_df


df=PA_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
PA_PC_ZLB_DB=filtered_df



df=NG_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
NG_PC_ZLB_DB=filtered_df


df=CL_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
CL_COT_ZLB_DB=filtered_df


df=GC_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
GC_COT_ZLB_DB=filtered_df


df=SI_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
SI_COT_ZLB_DB=filtered_df



df=HG_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
HG_COT_ZLB_DB=filtered_df


df=PA_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
PA_COT_ZLB_DB=filtered_df



df=NG_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2008-12-22") & df$DATE <= as.Date("2015-12-21"), ]
NG_COT_ZLB_DB=filtered_df



df=CL_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
CL_COT_COVID_DB=filtered_df


df=CL_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
CL_PC_COVID_DB=filtered_df


df=GC_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
GC_PC_COVID_DB=filtered_df


df=SI_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
SI_PC_COVID_DB=filtered_df



df=HG_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
HG_PC_COVID_DB=filtered_df


df=PA_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
PA_PC_COVID_DB=filtered_df



df=NG_PC_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
NG_PC_COVID_DB=filtered_df




df=GC_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
GC_COT_COVID_DB=filtered_df


df=SI_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
SI_COT_COVID_DB=filtered_df



df=HG_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
HG_COT_COVID_DB=filtered_df


df=PA_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
PA_COT_COVID_DB=filtered_df



df=NG_COT_DB
df$DATE <- as.Date(paste(df$YEAR, df$MONTH, df$DAY, sep="-"), "%Y-%m-%d")
filtered_df <- df[df$DATE >= as.Date("2020-01-31") & df$DATE <= as.Date("2022-06-10"), ]
NG_COT_COVID_DB=filtered_df

# These subsets are then saved as separate Rdata files for further analysis.

save(CL_PC_ZLB_DB,file = "CL_PC_ZLB_DB.Rdata")
save(GC_PC_ZLB_DB,file = "GC_PC_ZLB_DB.Rdata")
save(SI_PC_ZLB_DB,file = "SI_PC_ZLB_DB.Rdata")
save(HG_PC_ZLB_DB,file = "HG_PC_ZLB_DB.Rdata")
save(PA_PC_ZLB_DB,file = "PA_PC_ZLB_DB.Rdata")
save(NG_PC_ZLB_DB,file = "NG_PC_ZLB_DB.Rdata")


save(CL_COT_ZLB_DB,file = "CL_COT_ZLB_DB.Rdata")
save(GC_COT_ZLB_DB,file = "GC_COT_ZLB_DB.Rdata")
save(SI_COT_ZLB_DB,file = "SI_COT_ZLB_DB.Rdata")
save(HG_COT_ZLB_DB,file = "HG_COT_ZLB_DB.Rdata")
save(PA_COT_ZLB_DB,file = "PA_COT_ZLB_DB.Rdata")
save(NG_COT_ZLB_DB,file = "NG_COT_ZLB_DB.Rdata")

save(CL_PC_COVID_DB,file = "CL_PC_COVID_DB.Rdata")
save(GC_PC_COVID_DB,file = "GC_PC_COVID_DB.Rdata")
save(SI_PC_COVID_DB,file = "SI_PC_COVID_DB.Rdata")
save(HG_PC_COVID_DB,file = "HG_PC_COVID_DB.Rdata")
save(PA_PC_COVID_DB,file = "PA_PC_COVID_DB.Rdata")
save(NG_PC_COVID_DB,file = "NG_PC_COVID_DB.Rdata")


save(CL_COT_COVID_DB,file = "CL_COT_COVID_DB.Rdata")
save(GC_COT_COVID_DB,file = "GC_COT_COVID_DB.Rdata")
save(SI_COT_COVID_DB,file = "SI_COT_COVID_DB.Rdata")
save(HG_COT_COVID_DB,file = "HG_COT_COVID_DB.Rdata")
save(PA_COT_COVID_DB,file = "PA_COT_COVID_DB.Rdata")
save(NG_COT_COVID_DB,file = "NG_COT_COVID_DB.Rdata")


