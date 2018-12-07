# CFSRv2 Phoenix data
# Format data after extracting with python 'extract_1hr.py'

# Settings ----------------------------------------------------------------
rm(list=ls())
setwd('~/Documents/CFSR_Phoenix')
library(feather)
library(chron)
library(lubridate)
library(dplyr)

# Functions ---------------------------------------------------------------
chron_time <- function(d_time){ #returns chron structure of date-time
  d_time <- as.character(d_time)
  d_time <- t(as.data.frame(strsplit(d_time,' ')))
  row.names(d_time) = NULL
  c_time <- chron(dates=d_time[,1], format='y/m/d')
  # time = fractional day from Jan 1, 1970, use as.numeric
  return(c_time)
}

time_of_day <- function(d_time){ # returns tod from chron structure as hour decimal
  d_time <- as.character(d_time)
  d_time <- t(as.data.frame(strsplit(d_time,' ')))
  row.names(d_time) = NULL
  tod <- d_time[,2]
  return(tod)
}


# precipitation and wind speeds -------------------------------------------

df1 <- read_feather('/Users/sarahtannenbaum/Documents/CFSR_Phoenix/hourly_precip_Uxyz.feather')
precip <- unlist(df1[,"precip_kg_m-2"], use.names = FALSE)
Ux <- unlist(df1[,"Ux_m_s-1"], use.names = FALSE)
Uy <- unlist(df1[,"Uv_m_s-1"], use.names = FALSE)
Uz <- unlist(df1[,"Uz_Pa_s-1"], use.names = FALSE)
date <- unlist(df1[,"date"], use.names = FALSE)

# convert date-time to useful variables
chron_t <- chron_time(date) # chron format structure
tod <- time_of_day(date) # as hour UTC
tod <- as.numeric(tod)
moy <- month(chron_t) # as month 1-12
num_day <- floor(as.numeric(chron_t)) # day number since 01/01/1970

# create data frame of morning averages from 7-11 am (14-18 UTC)
df_am1 <- data.frame(num_day,tod,moy,Ux,Uy,Uz)
df_am1 <- subset(df_am1, tod>=14)
df_am1 <- subset(df_am1, tod<=18)
df_am1 <- aggregate(.~num_day, df_am1, mean) 
df_am1$tod <- NULL

# surface heat fluxes and humidity ----------------------------------------

df2 <- read_feather('/Users/sarahtannenbaum/Documents/CFSR_Phoenix/hourly_LE_SH_q.feather')
date2 <- unlist(df2[,"date"], use.names = FALSE)
LE <- unlist(df2[,"LE_W_m-2"], use.names = FALSE)
SH <- unlist(df2[,"SH_W_m-2"], use.names = FALSE)
q <- unlist(df2[,"q_kg_kg-1"], use.names = FALSE)
Temp <- unlist(df2[,"temp"], use.names = FALSE)

# convert date-time to useful variables
chron_t2 <- chron_time(date2) # chron format structure
tod2 <- time_of_day(date2) # as hour UTC
tod2 <- as.numeric(tod2)
num_day2 <- floor(as.numeric(chron_t2)) # day number since 01/01/1970

# create data frame of morning averages from 7-11 am (14-18 UTC)
df_am2 <- data.frame(num_day2,tod2,LE,SH,q,Temp)
df_am2 <- subset(df_am2, tod2>=14)
df_am2 <- subset(df_am2, tod2<=18)
df_am2 <- aggregate(.~num_day2, df_am2, mean) 
df_am2$tod2 <- NULL

# merge data frames
names(df_am2)[names(df_am2)=="num_day2"] <- "num_day"
df_am <- merge(df_am1, df_am2, all=FALSE)


# Dewpoint, CAPE, CIN  -------------------------------------------------------------

df4 <- read_feather('/Users/sarahtannenbaum/Documents/CFSR_Phoenix/6hr_surf_Dew_CAPE_CIN.feather')
date4 <- unlist(df4[,"date"], use.names = FALSE)
DPT <- unlist(df4[,"DPT_K"], use.names = FALSE)
CAPE <- unlist(df4[,"CAPE_J_kg-1"], use.names = FALSE)
CIN <- unlist(df4[,"CIN_J_kg-1"], use.names = FALSE)

# convert date-time to useful variables
chron_t4 <- chron_time(date4) # chron format structure
tod4 <- time_of_day(date4) # as hour UTC
tod4 <- as.numeric(tod4)
num_day4 <- floor(as.numeric(chron_t4)) # day number since 01/01/1970

# create data frame of morning values from 5-11 am (12 UTC)
df_am4 <- data.frame(num_day4,tod4,DPT,CAPE,CIN)
df_am4 <- subset(df_am4, tod4==12)
df_am4 <- aggregate(.~num_day4, df_am4, mean) 
df_am4$tod4 <- NULL

# merge data frames
names(df_am4)[names(df_am4)=="num_day4"] <- "num_day"
df_am <- merge(df_am, df_am4, all=FALSE)


# Rain data ---------------------------------------------------------------
rain <- ceiling(precip/60)
df_rain<- data.frame(num_day, tod, rain)
# identify days with morning rain 7-11am (14-18 UTC)
am_rain <- subset(df_rain, tod>=14)
am_rain <- subset(df_rain, tod<=18)
am_rain <- aggregate(.~num_day, am_rain, max)
am_rain$tod <- NULL
names(am_rain)[names(am_rain)=="rain"] <- "morning"
# Identify days with afternoon rain 11am-12am (18-7 UTC)
pm_rain <- subset(df_rain, (tod>=18||tod<=7))
pm_rain <- aggregate(.~num_day, pm_rain, max)
pm_rain$tod <- NULL
names(pm_rain)[names(pm_rain)=="rain"] <- "afternoon"
# combine rain-am & rain_pm and categorize as pm_rain (w/o morning rain) "rainy"
rain_df <- merge(am_rain, pm_rain, all=FALSE)
rain_df <- mutate(rain_df, rainy = (morning == 0 & afternoon == 1))

# merge data frames
new_data <- merge(df_am, rain_df, all=FALSE)
write.csv(new_data, file = "new_data.csv")
save(new_data, file = "new_data.RData")
