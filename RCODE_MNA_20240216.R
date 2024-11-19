# Macroeconomics Announcements data processing
# Simon-Pierre Boucher
# Processing macroeconomic data from multiple CSV files spanning different years. 
# It reads in each CSV file, filters specific events, calculates surprise values, aggregates data, and saves the final dataset as an RData file. 
# Additionally, it standardizes the surprise values and creates new columns for each event category. 
# Overall, it's a comprehensive data processing pipeline for macroeconomic analysis.

# Load necessary libraries

library(tidyverse)

# Set working directory

setwd("/Volumes/LaCie/PHD/CH1")

# Read data for each year

data_2023 <- read_csv("data_2023.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))

data_2022 <- read_csv("data_2022.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))

data_2021 <- read_csv("data_2021.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))
data_2020 <- read_csv("data_2020.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))

data_2019 <- read_csv("data_2019.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))
data_2018 <- read_csv("data_2018.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))
data_2017 <- read_csv("data_2017.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))
data_2016 <- read_csv("data_2016.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))

data_2015 <- read_csv("data_2015.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))
data_2014 <- read_csv("data_2014.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))
data_2013 <- read_csv("data_2013.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))

data_2012 <- read_csv("data_2012.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))
data_2011 <- read_csv("data_2011.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))


data_2010 <- read_csv("data_2010.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))

data_2009 <- read_csv("data_2009.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))

data_2008 <- read_csv("data_2008.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))

data_2007 <- read_csv("data_2007.csv", 
                      col_types = cols(date = col_date(format = "%d/%m/%Y"), 
                                       time = col_time(format = "%H:%M"), 
                                       actual = col_number(), forecast = col_number(), 
                                       previous = col_number()))

# Combine data for all years

macro=rbind(data_2008,data_2009,data_2010,data_2011,data_2012,data_2013,data_2014,data_2015,data_2016,data_2017,data_2018,data_2019,data_2020,data_2021,data_2022,data_2023)

# Filter data for specific events and add a new column indicating the event

IJC=macro %>% filter(
  event %in% c("Initial Jobless Claims", "Initial Jobless Claims  (Dec)","Initial Jobless Claims  (Aug)","Initial Jobless Claims  (Apr)","Initial Jobless Claims  (Jul)","Initial Jobless Claims  (Jan)","Initial Jobless Claims  (Feb)","Initial Jobless Claims  (May)","Initial Jobless Claims  (Mar)","Initial Jobless Claims  (Jun)","Initial Jobless Claims  (Sep)","Initial Jobless Claims  (Oct)","Initial Jobless Claims  (Nov)")
)
IJC$MNA= "IJC"

ADP=macro %>% filter(
  event %in% c("ADP Nonfarm Employment Change", "ADP Nonfarm Employment Change  (Dec)","ADP Nonfarm Employment Change  (Aug)","ADP Nonfarm Employment Change  (Apr)","ADP Nonfarm Employment Change  (Jul)","ADP Nonfarm Employment Change  (Jan)","ADP Nonfarm Employment Change  (Feb)","ADP Nonfarm Employment Change  (May)","ADP Nonfarm Employment Change  (Mar)","ADP Nonfarm Employment Change  (Jun)","ADP Nonfarm Employment Change  (Sep)","ADP Nonfarm Employment Change  (Oct)","ADP Nonfarm Employment Change  (Nov)")
)
ADP$MNA= "ADP"

CBCC=macro %>% filter(
  event %in% c("CB Consumer Confidence", "CB Consumer Confidence  (Dec)","CB Consumer Confidence  (Aug)","CB Consumer Confidence  (Apr)","CB Consumer Confidence  (Jul)","CB Consumer Confidence  (Jan)","CB Consumer Confidence  (Feb)","CB Consumer Confidence  (May)","CB Consumer Confidence  (Mar)","CB Consumer Confidence  (Jun)","CB Consumer Confidence  (Sep)","CB Consumer Confidence  (Oct)","CB Consumer Confidence  (Nov)")
)
CBCC$MNA= "CBCC"

ARS=macro %>% filter(
  event %in% c("Retail Sales (MoM)", "Retail Sales (MoM)  (Dec)","Retail Sales (MoM)  (Aug)","Retail Sales (MoM)  (Apr)","Retail Sales (MoM)  (Jul)","Retail Sales (MoM)  (Jan)","Retail Sales (MoM)  (Feb)","Retail Sales (MoM)  (May)","Retail Sales (MoM)  (Mar)","Retail Sales (MoM)  (Jun)","Retail Sales (MoM)  (Sep)","Retail Sales (MoM)  (Oct)","Retail Sales (MoM)  (Nov)")
)
ARS$MNA= "ARS"
BP=macro %>% filter(
  event %in% c("Building Permits", "Building Permits  (Dec)","Building Permits  (Aug)","Building Permits  (Apr)","Building Permits  (Jul)","Building Permits  (Jan)","Building Permits  (Feb)","Building Permits  (May)","Building Permits  (Mar)","Building Permits  (Jun)","Building Permits  (Sep)","Building Permits  (Oct)","Building Permits  (Nov)")
)
BP$MNA= "BP"
ConstS=macro %>% filter(
  event %in% c("Construction Spending (MoM)", "Construction Spending (MoM)  (Dec)","Construction Spending (MoM)  (Aug)","Construction Spending (MoM)  (Apr)","Construction Spending (MoM)  (Jul)","Construction Spending (MoM)  (Jan)","Construction Spending (MoM)  (Feb)","Construction Spending (MoM)  (May)","Construction Spending (MoM)  (Mar)","Construction Spending (MoM)  (Jun)","Construction Spending (MoM)  (Sep)","Construction Spending (MoM)  (Oct)","Construction Spending (MoM)  (Nov)")
)
ConstS$MNA= "ConstS"
ConsC=macro %>% filter(
  event %in% c("Consumer Credit", "Consumer Credit  (Dec)","Consumer Credit  (Aug)","Consumer Credit  (Apr)","Consumer Credit  (Jul)","Consumer Credit  (Jan)","Consumer Credit  (Feb)","Consumer Credit  (May)","Consumer Credit  (Mar)","Consumer Credit  (Jun)","Consumer Credit  (Sep)","Consumer Credit  (Oct)","Consumer Credit  (Nov)")
)
ConsC$MNA= "ConsC"
CPI=macro %>% filter(
  event %in% c("CPI (MoM)", "CPI (MoM)  (Dec)","CPI (MoM)  (Aug)","CPI (MoM)  (Apr)","CPI (MoM)  (Jul)","CPI (MoM)  (Jan)","CPI (MoM)  (Feb)","CPI (MoM)  (May)","CPI (MoM)  (Mar)","CPI (MoM)  (Jun)","CPI (MoM)  (Sep)","CPI (MoM)  (Oct)","CPI (MoM)  (Nov)")
)
CPI$MNA= "CPI"
DGO=macro %>% filter(
  event %in% c("Durable Goods Orders (MoM)", "Durable Goods Orders (MoM)  (Dec)","Durable Goods Orders (MoM)  (Aug)","Durable Goods Orders (MoM)  (Apr)","Durable Goods Orders (MoM)  (Jul)","Durable Goods Orders (MoM)  (Jan)","Durable Goods Orders (MoM)  (Feb)","Durable Goods Orders (MoM)  (May)","Durable Goods Orders (MoM)  (Mar)","Durable Goods Orders (MoM)  (Jun)","Durable Goods Orders (MoM)  (Sep)","Durable Goods Orders (MoM)  (Oct)","Durable Goods Orders (MoM)  (Nov)")
)
DGO$MNA= "DGO"
EHS=macro %>% filter(
  event %in% c("Existing Home Sales", "Existing Home Sales  (Dec)","Existing Home Sales  (Aug)","Existing Home Sales  (Apr)","Existing Home Sales  (Jul)","Existing Home Sales  (Jan)","Existing Home Sales  (Feb)","Existing Home Sales  (May)","Existing Home Sales  (Mar)","Existing Home Sales  (Jun)","Existing Home Sales  (Sep)","Existing Home Sales  (Oct)","Existing Home Sales  (Nov)")
)
EHS$MNA= "EHS"
FO=macro %>% filter(
  event %in% c("Factory Orders (MoM)", "Factory Orders (MoM)  (Dec)","Factory Orders (MoM)  (Aug)","Factory Orders (MoM)  (Apr)","Factory Orders (MoM)  (Jul)","Factory Orders (MoM)  (Jan)","Factory Orders (MoM)  (Feb)","Factory Orders (MoM)  (May)","Factory Orders (MoM)  (Mar)","Factory Orders (MoM)  (Jun)","Factory Orders (MoM)  (Sep)","Factory Orders (MoM)  (Oct)","Factory Orders (MoM)  (Nov)")
)
FO$MNA= "FO"
GDP=macro %>% filter(
  event %in% c("GDP (QoQ)", "GDP (QoQ)  (Q1)", "GDP (QoQ)  (Q2)", "GDP (QoQ)  (Q3)", "GDP (QoQ)  (Q4)")
)
GDP$MNA= "GDP"
HS=macro %>% filter(
  event %in% c("Housing Starts", "Housing Starts  (Dec)","Housing Starts  (Aug)","Housing Starts  (Apr)","Housing Starts  (Jul)","Housing Starts  (Jan)","Housing Starts  (Feb)","Housing Starts  (May)","Housing Starts  (Mar)","Housing Starts  (Jun)","Housing Starts  (Sep)","Housing Starts  (Oct)","Housing Starts  (Nov)")
)
HS$MNA= "HS"
IP=macro %>% filter(
  event %in% c("Industrial Production (MoM)", "Industrial Production (MoM)  (Dec)","Industrial Production (MoM)  (Aug)","Industrial Production (MoM)  (Apr)","Industrial Production (MoM)  (Jul)","Industrial Production (MoM)  (Jan)","Industrial Production (MoM)  (Feb)","Industrial Production (MoM)  (May)","Industrial Production (MoM)  (Mar)","Industrial Production (MoM)  (Jun)","Industrial Production (MoM)  (Sep)","Industrial Production (MoM)  (Oct)","Industrial Production (MoM)  (Nov)")
)
IP$MNA= "IP"
MCS=macro %>% filter(
  event %in% c("Michigan Consumer Sentiment", "Michigan Consumer Sentiment  (Dec)","Michigan Consumer Sentiment  (Aug)","Michigan Consumer Sentiment  (Apr)","Michigan Consumer Sentiment  (Jul)","Michigan Consumer Sentiment  (Jan)","Michigan Consumer Sentiment  (Feb)","Michigan Consumer Sentiment  (May)","Michigan Consumer Sentiment  (Mar)","Michigan Consumer Sentiment  (Jun)","Michigan Consumer Sentiment  (Sep)","Michigan Consumer Sentiment  (Oct)","Michigan Consumer Sentiment  (Nov)")
)
MCS$MNA= "MCS"
NHS=macro %>% filter(
  event %in% c("New Home Sales", "New Home Sales  (Dec)","New Home Sales  (Aug)","New Home Sales  (Apr)","New Home Sales  (Jul)","New Home Sales  (Jan)","New Home Sales  (Feb)","New Home Sales  (May)","New Home Sales  (Mar)","New Home Sales  (Jun)","New Home Sales  (Sep)","New Home Sales  (Oct)","New Home Sales  (Nov)")
)
NHS$MNA= "NHS"
NFPR=macro %>% filter(
  event %in% c("Nonfarm Payrolls", "Nonfarm Payrolls  (Dec)","Nonfarm Payrolls  (Aug)","Nonfarm Payrolls  (Apr)","Nonfarm Payrolls  (Jul)","Nonfarm Payrolls  (Jan)","Nonfarm Payrolls  (Feb)","Nonfarm Payrolls  (May)","Nonfarm Payrolls  (Mar)","Nonfarm Payrolls  (Jun)","Nonfarm Payrolls  (Sep)","Nonfarm Payrolls  (Oct)","Nonfarm Payrolls  (Nov)")
)
NFPR$MNA= "NFPR"
PHS=macro %>% filter(
  event %in% c("Pending Home Sales (MoM)", "Pending Home Sales (MoM)  (Dec)","Pending Home Sales (MoM)  (Aug)","Pending Home Sales (MoM)  (Apr)","Pending Home Sales (MoM)  (Jul)","Pending Home Sales (MoM)  (Jan)","Pending Home Sales (MoM)  (Feb)","Pending Home Sales (MoM)  (May)","Pending Home Sales (MoM)  (Mar)","Pending Home Sales (MoM)  (Jun)","Pending Home Sales (MoM)  (Sep)","Pending Home Sales (MoM)  (Oct)","Pending Home Sales (MoM)  (Nov)")
)
PHS$MNA= "PHS"

PersoS=macro %>% filter(
  event %in% c("Personal Spending (MoM)", "Personal Spending (MoM)  (Dec)","Personal Spending (MoM)  (Aug)","Personal Spending (MoM)  (Apr)","Personal Spending (MoM)  (Jul)","Personal Spending (MoM)  (Jan)","Personal Spending (MoM)  (Feb)","Personal Spending (MoM)  (May)","Personal Spending (MoM)  (Mar)","Personal Spending (MoM)  (Jun)","Personal Spending (MoM)  (Sep)","Personal Spending (MoM)  (Oct)","Personal Spending (MoM)  (Nov)")
)
PersoS$MNA= "PersoS"
PersoI=macro %>% filter(
  event %in% c("Personal Income (MoM)", "Personal Income (MoM)  (Dec)","Personal Income (MoM)  (Aug)","Personal Income (MoM)  (Apr)","Personal Income (MoM)  (Jul)","Personal Income (MoM)  (Jan)","Personal Income (MoM)  (Feb)","Personal Income (MoM)  (May)","Personal Income (MoM)  (Mar)","Personal Income (MoM)  (Jun)","Personal Income (MoM)  (Sep)","Personal Income (MoM)  (Oct)","Personal Income (MoM)  (Nov)")
)
PersoI$MNA= "PersoI"
PPI=macro %>% filter(
  event %in% c("PPI (MoM)", "PPI (MoM)  (Dec)","PPI (MoM)  (Aug)","PPI (MoM)  (Apr)","PPI (MoM)  (Jul)","PPI (MoM)  (Jan)","PPI (MoM)  (Feb)","PPI (MoM)  (May)","PPI (MoM)  (Mar)","PPI (MoM)  (Jun)","PPI (MoM)  (Sep)","PPI (MoM)  (Oct)","PPI (MoM)  (Nov)")
)
PPI$MNA= "PPI"
TB=macro %>% filter(
  event %in% c("Trade Balance", "Trade Balance  (Dec)","Trade Balance  (Aug)","Trade Balance  (Apr)","Trade Balance  (Jul)","Trade Balance  (Jan)","Trade Balance  (Feb)","Trade Balance  (May)","Trade Balance  (Mar)","Trade Balance  (Jun)","Trade Balance  (Sep)","Trade Balance  (Oct)","Trade Balance  (Nov)")
)
TB$MNA= "TB"

NGI=macro %>% filter(
  event %in% c("Natural Gas Storage", "Natural Gas Storage  (Dec)","Natural Gas Storage  (Aug)","Natural Gas Storage  (Apr)","Natural Gas Storage  (Jul)","Natural Gas Storage  (Jan)","Natural Gas Storage  (Feb)","Natural Gas Storage  (May)","Natural Gas Storage  (Mar)","Natural Gas Storage  (Jun)","Natural Gas Storage  (Sep)","Natural Gas Storage  (Oct)","Natural Gas Storage  (Nov)")
)
NGI$MNA= "NGI"
AWCOS=macro %>% filter(
  event %in% c("API Weekly Crude Oil Stock", "API Weekly Crude Oil Stock  (Dec)","API Weekly Crude Oil Stock  (Aug)","API Weekly Crude Oil Stock  (Apr)","API Weekly Crude Oil Stock  (Jul)","API Weekly Crude Oil Stock  (Jan)","API Weekly Crude Oil Stock  (Feb)","API Weekly Crude Oil Stock  (May)","API Weekly Crude Oil Stock  (Mar)","API Weekly Crude Oil Stock  (Jun)","API Weekly Crude Oil Stock  (Sep)","API Weekly Crude Oil Stock  (Oct)","API Weekly Crude Oil Stock  (Nov)")
)
AWCOS$MNA= "AWCOS"

# Combine all filtered data

MACRO_DATA=rbind(ADP,ARS,BP,CBCC,ConstS,ConsC,CPI,DGO,EHS,FO,GDP,HS,IJC,IP,NFPR,NHS,PersoI,PersoS,PHS,PPI,TB,MCS,NGI,AWCOS)

#### Convert certain columns to numeric

MACRO_DATA$actual= as.numeric(MACRO_DATA$actual)
MACRO_DATA$forecast= as.numeric(MACRO_DATA$forecast)

# Calculate surprise

MACRO_DATA$SURPRISE=MACRO_DATA$actual- MACRO_DATA$forecast

# Calculate standard deviation by category

ecart_type_par_categorie <- aggregate(SURPRISE ~ MNA, data = MACRO_DATA, FUN = sd)
names(ecart_type_par_categorie)[2] <- "ecart_type"

# Merge standard deviation data with main dataset

MACRO <- merge(MACRO_DATA, ecart_type_par_categorie, by = "MNA", all.x = TRUE)

# Calculate standardized surprise

MACRO$SS=MACRO$SURPRISE/MACRO$ecart_type

# Loop over unique categories to create new columns for each category

for (cat in unique(MACRO$MNA)) {
  nom_variable <- paste0("SS", cat)
  MACRO[[nom_variable]] <- ifelse(MACRO$MNA == cat, MACRO$SS, 0)
  MACRO[[nom_variable]] <- as.numeric(MACRO[[nom_variable]])
}

# Remove rows with missing values

MACRO=na.omit(MACRO)

# Use the Date and Time to created YEAR, MONTH, DAY, HOUR, MINUTE variables 
MACRO$YEAR<-as.numeric(substr(MACRO$date,1,4))
MACRO$MONTH<-as.numeric(substr(MACRO$date,6,7))
MACRO$DAY<-as.numeric(substr(MACRO$date,9,10))
MACRO$HOUR<-as.numeric(substr(MACRO$time,1,2))
MACRO$MINUTE<-as.numeric(substr(MACRO$time,4,5))
MACRO$t<-MACRO$HOUR*60+MACRO$MINUTE

# Save final dataset

save(MACRO,file = "MNA_DATA_ALL.Rdata")
