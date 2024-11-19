# CFTC Trader positions data processing 
# Simon-Pierre Boucher 

# The provided code is a script written in R programming language. It performs several tasks related to data manipulation and analysis using the Quandl package for accessing financial market data.

# Here's a breakdown of what the code does:

## It begins by installing and loading the Quandl package.
## Sets the working directory to a specific location.
## Retrieves various datasets from the Quandl API using unique identifiers and an API key.
## Defines a function (shift_dates) to shift dates in a dataframe.
## Processes each dataset separately (e.g., CFTC_CL_PC, CFTC_GC_PC) by shifting dates and calculating additional metrics like MSCT, NLS, and WT.
## Saves the processed datasets as Rda files (R data files) for future use.
## Overall, the code appears to be performing data preprocessing and feature engineering tasks on financial market data obtained from Quandl.

# Install and load Quandl package

install.packages("Quandl")
library(Quandl)

# Set working directory

setwd("/Volumes/LaCie/PHD/CH1")


# Retrieve data from Quandl API

CFTC_GC_COT<-Quandl("CFTC/088691_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_GC_PC<-Quandl("CFTC/088691_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_CL_COT<-Quandl("CFTC/067651_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_CL_PC<-Quandl("CFTC/067651_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_HG_COT<-Quandl("CFTC/085692_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_HG_PC<-Quandl("CFTC/085692_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_SI_COT<-Quandl("CFTC/084691_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_SI_PC<-Quandl("CFTC/084691_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_NG_COT<-Quandl("CFTC/023651_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_NG_PC<-Quandl("CFTC/023651_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_PA_COT<-Quandl("CFTC/075651_F_ALL", api_key="e9aZeuvn-X6isXXx9TCk")
CFTC_PA_PC<-Quandl("CFTC/075651_F_L_ALL", api_key="e9aZeuvn-X6isXXx9TCk")

# Define shift values

shift_values <- c(-3, -2, -1, 1, 2, 3)

# Function to shift dates

shift_dates <- function(data, column_name, shift) {
  shifted_data <- data
  shifted_data$Date <- shifted_data$Date + shift
  return(shifted_data)
}

# Process CFTC_CL_PC data  
tt=CFTC_CL_PC
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MSCT<-(tt$`Noncommercial Long`+tt$`Noncommercial Short`)/(2*tt$`Open Interest`)
tt$NLS<-(tt$`Noncommercial Long`- tt$`Noncommercial Short`)/ tt$`Open Interest`

tt$WT1<-1+(tt$`Noncommercial Short`/(tt$`Commercial Long`+tt$`Commercial Short`))
tt$WT2<-1+(tt$`Noncommercial Long`/(tt$`Commercial Long`+tt$`Commercial Short`))

tt$WT=ifelse(tt$`Commercial Short` > tt$`Commercial Long`,tt$WT1,tt$WT2)
CL_PC=tt


# Process CFTC_GC_PC data

tt=CFTC_GC_PC
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MSCT<-(tt$`Noncommercial Long`+tt$`Noncommercial Short`)/(2*tt$`Open Interest`)
tt$NLS<-(tt$`Noncommercial Long`- tt$`Noncommercial Short`)/ tt$`Open Interest`

tt$WT1<-1+(tt$`Noncommercial Short`/(tt$`Commercial Long`+tt$`Commercial Short`))
tt$WT2<-1+(tt$`Noncommercial Long`/(tt$`Commercial Long`+tt$`Commercial Short`))

tt$WT=ifelse(tt$`Commercial Short` > tt$`Commercial Long`,tt$WT1,tt$WT2)
GC_PC=tt


# Process CFTC_SI_PC data

tt=CFTC_SI_PC
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MSCT<-(tt$`Noncommercial Long`+tt$`Noncommercial Short`)/(2*tt$`Open Interest`)
tt$NLS<-(tt$`Noncommercial Long`- tt$`Noncommercial Short`)/ tt$`Open Interest`

tt$WT1<-1+(tt$`Noncommercial Short`/(tt$`Commercial Long`+tt$`Commercial Short`))
tt$WT2<-1+(tt$`Noncommercial Long`/(tt$`Commercial Long`+tt$`Commercial Short`))

tt$WT=ifelse(tt$`Commercial Short` > tt$`Commercial Long`,tt$WT1,tt$WT2)
SI_PC=tt


# Process CFTC_HG_PC data

tt=CFTC_HG_PC
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MSCT<-(tt$`Noncommercial Long`+tt$`Noncommercial Short`)/(2*tt$`Open Interest`)
tt$NLS<-(tt$`Noncommercial Long`- tt$`Noncommercial Short`)/ tt$`Open Interest`

tt$WT1<-1+(tt$`Noncommercial Short`/(tt$`Commercial Long`+tt$`Commercial Short`))
tt$WT2<-1+(tt$`Noncommercial Long`/(tt$`Commercial Long`+tt$`Commercial Short`))

tt$WT=ifelse(tt$`Commercial Short` > tt$`Commercial Long`,tt$WT1,tt$WT2)
HG_PC=tt

# Process CFTC_PA_PC data

tt=CFTC_PA_PC
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MSCT<-(tt$`Noncommercial Long`+tt$`Noncommercial Short`)/(2*tt$`Open Interest`)
tt$NLS<-(tt$`Noncommercial Long`- tt$`Noncommercial Short`)/ tt$`Open Interest`

tt$WT1<-1+(tt$`Noncommercial Short`/(tt$`Commercial Long`+tt$`Commercial Short`))
tt$WT2<-1+(tt$`Noncommercial Long`/(tt$`Commercial Long`+tt$`Commercial Short`))

tt$WT=ifelse(tt$`Commercial Short` > tt$`Commercial Long`,tt$WT1,tt$WT2)
PA_PC=tt

# Process CFTC_NG_PC data

tt=CFTC_NG_PC
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MSCT<-(tt$`Noncommercial Long`+tt$`Noncommercial Short`)/(2*tt$`Open Interest`)
tt$NLS<-(tt$`Noncommercial Long`- tt$`Noncommercial Short`)/ tt$`Open Interest`

tt$WT1<-1+(tt$`Noncommercial Short`/(tt$`Commercial Long`+tt$`Commercial Short`))
tt$WT2<-1+(tt$`Noncommercial Long`/(tt$`Commercial Long`+tt$`Commercial Short`))

tt$WT=ifelse(tt$`Commercial Short` > tt$`Commercial Long`,tt$WT1,tt$WT2)
NG_PC=tt

# Process CFTC_CL_COT data

tt=CFTC_CL_COT
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MM_NLS<-(tt$`Money Manager Longs` - tt$`Money Manager Shorts`)/tt$`Open Interest`
tt$SWAP_NLS<-(tt$`Swap Dealer Longs` - tt$`Swap Dealer Shorts`)/tt$`Open Interest`
CL_COT=tt

# Process CFTC_GC_COT data

tt=CFTC_GC_COT
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MM_NLS<-(tt$`Money Manager Longs` - tt$`Money Manager Shorts`)/tt$`Open Interest`
tt$SWAP_NLS<-(tt$`Swap Dealer Longs` - tt$`Swap Dealer Shorts`)/tt$`Open Interest`
GC_COT=tt

# Process CFTC_SI_COT data

tt=CFTC_SI_COT
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MM_NLS<-(tt$`Money Manager Longs` - tt$`Money Manager Shorts`)/tt$`Open Interest`
tt$SWAP_NLS<-(tt$`Swap Dealer Longs` - tt$`Swap Dealer Shorts`)/tt$`Open Interest`
SI_COT=tt

# Process CFTC_NG_COT data

tt=CFTC_NG_COT
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MM_NLS<-(tt$`Money Manager Longs` - tt$`Money Manager Shorts`)/tt$`Open Interest`
tt$SWAP_NLS<-(tt$`Swap Dealer Longs` - tt$`Swap Dealer Shorts`)/tt$`Open Interest`
NG_COT=tt

# Process CFTC_PA_COT data

tt=CFTC_PA_COT
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MM_NLS<-(tt$`Money Manager Longs` - tt$`Money Manager Shorts`)/tt$`Open Interest`
tt$SWAP_NLS<-(tt$`Swap Dealer Longs` - tt$`Swap Dealer Shorts`)/tt$`Open Interest`
PA_COT=tt

# Process CFTC_HG_COT data

tt=CFTC_HG_COT
t=tt
for (shift in shift_values) {
  xx=shift_dates(t,Date,shift)
  tt=rbind(tt,xx)
}
tt$MM_NLS<-(tt$`Money Manager Longs` - tt$`Money Manager Shorts`)/tt$`Open Interest`
tt$SWAP_NLS<-(tt$`Swap Dealer Longs` - tt$`Swap Dealer Shorts`)/tt$`Open Interest`
HG_COT=tt

# Save processed datasets

save(CL_COT,file="CL_COT.Rda")
save(CL_PC,file="CL_PC.Rda")
save(GC_COT,file="GC_COT.Rda")
save(GC_PC,file="GC_PC.Rda")
save(HG_COT,file="HG_COT.Rda")
save(HG_PC,file="HG_PC.Rda")
save(NG_COT,file="NG_COT.Rda")
save(NG_PC,file="NG_PC.Rda")
save(PA_COT,file="PA_COT.Rda")
save(PA_PC,file="PA_PC.Rda")
save(SI_COT,file="SI_COT.Rda")
save(SI_PC,file="SI_PC.Rda")

