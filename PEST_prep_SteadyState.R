#==============================================================================#
#
#     Convert HGS output to Bore sample file
#
#==============================================================================#

#### 0. Libraries ####
# scan for additional arguments
args = commandArgs(trailingOnly = T)
args
source("D:/PhD/HOAL/modelling/HGS_model/Scripts/RHGS/HGSfun.R", 
       echo = F, verbose = F, print.eval = F, prompt.echo = F)

# test if there are all required arguments
if(length(args) == 0) {
  stop("no arguments provided", call. = F)
} else if (length(args) < 4){
  stop("invalid number of arguments provided", call. = F)
} else{
  # this is what should be supplied through the batch script
  # %StGW% %StQ% %date_start% %interpolate% %file_head% %file_Q% %file_head_obs% %file_Q_obs%
  StGW <- unlist(str_split(args[1], "\\,\\s*"))
  StQ <-  unlist(str_split(args[2], "\\,\\s*"))
  file_head_out <- args[3]
  file_Q_out <- args[4]
  if(length(args) == 6){
    file_head_obs_out <- args[5]
    file_Q_obs_out <- args[6]
  }
}

#==============================================================================#
#### 1. Preparation ####
dir <- getwd()


#==============================================================================#
#### 2. GW heads ####

## 2.1 GW simulated heads ####

# Find head output data files
head_files <- list.files(path = dir, pattern = "(observation_well_flow)(.+)(\\.dat)$",full.names = T)
head_files
# keep only files of selected stations
head_files <- head_files[str_extract(head_files,pattern = "(?<=well_flow.)([:alnum:]+)") %in% StGW]
if(length(head_files) < 1) stop("No modeled heads found!")

# Convert head data to Bore sample file
l <- list()
for(ii in 1:length(head_files)){
  dat <- read.tecplot(head_files[ii])
  Station = substr(attr(dat, "Station"), 1,10)
  dat <- dat[nrow(dat),c("Time", "H")]
  
  l[[ii]] <- data.frame(Station = Station, 
                        Date = "30/12/2018",
                        Time = "00:00:00",
                        value = round(dat[1,2],3),
                        stringsAsFactors = F)
  
}
write.table(do.call(rbind, l), file = file_head_out, quote = F, sep = "\t",
            row.names = F, col.names = F)

paste0("Simulated heads written to: ", dir, "/",file_head_out)

## 2.2 GW observed heads ####
if(exists("file_head_obs_out")){
  GWmean <- colMeans(HOAL.data::GWL[ ,StGW], na.rm = T)
  df.GWmean <- data.frame(Station = names(GWmean),
                          Date = "30/12/2018",
                          Time = "00:00:00",
                          Mean = round(GWmean, digits = 3))
  
  write.table(df.GWmean, sep = "\t", row.names = F, col.names = F,quote = F,
              file = file_head_obs_out)
  
}

#==============================================================================#
#### 3. Discharge ####

## 3.1 Simulated Discharge ####
Q_files <- list.files(path = dir, pattern = "(hydrograph)(.+)(\\.dat)$", full.names = T)
# keep only files of selected stations
Q_files <- Q_files[str_extract(Q_files,pattern = "(?<=hydrograph\\.)(.+)(?=\\.dat)") %in% StQ]
if(length(Q_files) < 1) stop("No modeled discharges found!")

# Convert head data to Bore sample file
l <- list()
for(ii in 1:length(Q_files)){
  dat <- read.tecplot(Q_files[ii])
  Station = substr(attr(dat, "Station"), 1,10)
  dat <- dat[nrow(dat),c("Time", "Surface")]
  
  l[[ii]] <- data.frame(Station = Station, 
                        Date = "30/12/2018",
                        Time = "00:00:00",
                        value = round(dat[1,2],3),
                        stringsAsFactors = F)
  
}
write.table(do.call(rbind, l), file = file_Q_out, quote = F, sep = "\t",
            row.names = F, col.names = F)

paste0("Simulated flows written to: ", dir, "/",file_Q_out)

## 2.2 GW observed heads ####
if(exists("file_Q_obs_out")){
  # Change the name outflow with MW which is found in HOAL.Discharge
  StQ <- ifelse("outflow" %in% StQ, replace(StQ, StQ == "outflow", "MW"), StQ)
  
  Qmean <- colMeans(HOAL.data::HOAL.Discharge[,StQ], na.rm = T) / 1000 * 86400
  names(Qmean) <- replace(names(Qmean), names(Qmean) == "MW", "outflow")
  
  df.Qmean <- data.frame(Station = names(Qmean),
                         Date = "30/12/2018",
                         Time = "00:00:00",
                         Mean = round(Qmean, digits = 3))
  
  write.table(df.Qmean, sep = "\t", row.names = F, col.names = F,quote = F,
              file = file_Q_obs_out)
  
  paste0("Observed flows written to: ",dir, "/",file_Q_obs_out)
}
