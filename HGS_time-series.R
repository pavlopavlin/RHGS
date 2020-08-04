library(tidyverse)
library(HOAL)
library(dygraphs)
library(xts)
source("D:/PhD/HOAL/modelling/HGS_model/Scripts/RHGS/HGSfun.R", echo = F)
#==============================================================================#
#### 1. Historic Data ####

## PET and Prec
df.raw <- readxl::read_excel("D:/PhD/HOAL/processed_data/weather_station/IKT/Wetter_IKT_1945-2018.xlsx",
                             range = "A1:T26725", 
                             col_types = c("guess", rep("numeric",19)))

# Create monthly means or sums for weather data 1950-2018
df <- df.raw %>% 
  mutate(year = lubridate::year(Datum), month = lubridate::month(Datum)) %>%
  group_by(year, month) %>%
  summarise_all(., .funs = c(mean), na.rm = T)

# Mean of each month in a year
df.mean <- df %>% 
  group_by(month) %>%
  #mutate(year = NULL) %>%
  summarise_all(.funs = mean, na.rm=T)

# Fills missing values with the mean value for that month
df.fill <- as.data.frame(df)
for(ii in 1:nrow(df.fill)){
  if(any(is.na(df.fill[ii,])))
    df.fill[ii,is.na(df.fill[ii,])] <- 
      as.numeric(df.mean %>% slice(df.fill[ii,"month"]))[is.na(df.fill[ii,])]
}
df.fill <- df.fill %>% mutate(Date = lubridate::make_date(year,month, day = 1))

# Format for HGS
df.sim <- df.fill %>% 
  filter(Date >= "1950-01-01") %>%
  mutate(SimDay = round(as.double(Date - as.Date("1950-01-01"), unit = "days"), 2),
         SimDay_midMonth = round(as.double(Datum - as.POSIXct("1950-01-01"), unit = "days"), 2),
         PET.m.d = signif(ETo_Blaney_mm.d / 1000, 4),
         Prec.m.d = signif(`NS mm` / 1000,4)) %>%
  dplyr::select(Date, SimDay, Datum, SimDay_midMonth, PET.m.d, Prec.m.d)

write.csv(df.sim, "D:/PhD/HOAL/modelling/HGS_model/data/time-series/HOAL_PET_Prec_1950-2018.csv", row.names = F)
# Files with simulation dates at the begining of the month
write.table(df.sim %>% select(SimDay, PET.m.d), sep = "\t",row.names = F, col.names = F,
            file = "D:/PhD/HOAL/modelling/HGS_model/data/time-series/HOAL_PET_1950-2018_hgs.txt")
write.table(df.sim %>% select(SimDay, Prec.m.d), sep = "\t",row.names = F, col.names = F,
            file = "D:/PhD/HOAL/modelling/HGS_model/data/time-series/HOAL_Prec_1950-2018_hgs.txt")
# Files with simulation dates in the middle of the month
write.table(df.sim %>% select(SimDay_midMonth, PET.m.d), sep = "\t",row.names = F, col.names = F,
            file = "D:/PhD/HOAL/modelling/HGS_model/data/time-series/HOAL_PET_1950-2018_hgs_midMonth.txt")
write.table(df.sim %>% select(SimDay_midMonth, Prec.m.d), sep = "\t",row.names = F, col.names = F,
            file = "D:/PhD/HOAL/modelling/HGS_model/data/time-series/HOAL_Prec_1950-2018_hgs_midMonth.txt")

plot(df.sim$SimDay, df.sim$PET.m.d, type = "l")
lines(df.sim$SimDay, df.sim$Prec.m.d, col = "red", type = "l")

df.xts <- xts(df.fill[,c("ETo_Blaney_mm.d","NS mm")], order.by = df.fill$Date)
dygraph(df.xts)


#------------------------------------------------------------------------------#
## Runoff 1990-2017
Q.raw <- readxl::read_excel("D:/PhD/HOAL/data_lovrenc/stream/Runoff_Lovrenc_1990_2017.xlsx",
                            range = "A1:E10228", na = "NA") %>%
  mutate(Q.m3.s = `Observed Streamflow (m3/s)`, `Observed Streamflow (m3/s)` =NULL)

Q.day <- Q.raw %>%
  filter(is.na(Q.m3.s)) %>%
  mutate(Q.m3.d = Q.m3.s * 86400,
         Q.m3.s = NULL)

Q.fill <- HOAL::to_timestep(HOAL.data::HOAL.Discharge$MW["2018/"], "days") %>%
  fortify.zoo() %>%
  mutate(MW = MW * 86400 / 1000) %>% # covert from l/s to m3/day
  rename(Date = Index, Q.m3.d = MW) %>%
  mutate(Day = day(Date),
         Month = month(Date),
         Year = year(Date),
         Date = as_datetime(as.character(Date), tz = "UTC")) %>%
  bind_rows(Q.day,.)

Q.mon <- Q.fill %>%
  group_by(Year, Month) %>%
  summarise(Q.m3.d = mean(Q.m3.d, na.rm = T))

# Switch to simulation time (Days since 1950-01-01)
Q.sim <- Q.fill %>%
  mutate(SimDay = as.double(Date - as.POSIXct("1950-01-01", tz = "UTC"), units = "days")) %>%
  select(SimDay, Q.m3.d)

# Exporting to files

write.table(Q.sim, sep = "\t",row.names = F, col.names = F,
            file = "D:/PhD/HOAL/modelling/HGS_model/data/time-series/MW_1990-2017_StartDate_1950.txt")

#==============================================================================#

#### 2. Long term averages for PEST ####
StGW <- c("BP01", "H01", "H02", "H04", "H05", "H08", "H06", "H09", "H10", "H11", 
          "H12", "G2", "G3", "G4", "G6", "G8", "G5")
StQ <- c("MW")

GWmean <- colMeans(HOAL.data::GWL[ ,StGW], na.rm = T)
Qmean <- colMeans(HOAL.data::HOAL.Discharge[,StQ], na.rm = T) / 1000 * 86400
names(Qmean) <- replace(names(Qmean), names(Qmean) == "MW", "outflow")
df.GWmean <- data.frame(Station = names(GWmean),
                     Date = "30/12/2018",
                     Time = "00:00:00",
                     Mean = round(GWmean, digits = 3))

df.Qmean <- data.frame(Station = names(Qmean),
                        Date = "30/12/2018",
                        Time = "00:00:00",
                        Mean = round(Qmean, digits = 3))


write.table(df.GWmean, sep = "\t", row.names = F, col.names = F,quote = F,
            file = "D:/PhD/HOAL/modelling/HGS_model/HOAL_SteadyState/head_obs.smp")

write.table(df.Qmean, sep = "\t", row.names = F, col.names = F,quote = F,
            file = "D:/PhD/HOAL/modelling/HGS_model/HOAL_SteadyState/Q_obs.smp")
