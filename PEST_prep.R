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
} else if (length(args) < 6){
  stop("invalid number of arguments provided", call. = F)
} else{
  # this is what should be supplied through the batch script
  # %StGW% %StQ% %date_start% %interpolate% %file_head% %file_Q% %file_head_obs% %file_Q_obs%
  StGW <- unlist(str_split(args[1], "\\,\\s*"))
  StQ <-  unlist(str_split(args[2], "\\,\\s*"))
  date_start <- as.Date(args[3])
  interpolate <- args[4]
  file_head_out <- args[5]
  file_Q_out <- args[6]
  subset_date <- args[7]
  if(length(args) == 9){
    file_head_obs_out <- args[8]
    file_Q_obs_out <- args[9]
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

# keep only files of selected stations
head_files <- head_files[str_extract(head_files,pattern = "(?<=well_flow.)([:alnum:]+)") %in% StGW]
if(length(head_files) < 1) stop("No modeled heads found!")

# Convert head data to Bore sample file
hgs2smp(file_in = head_files, 
        file_out = paste0(dir,"/",file_head_out),
        var_name = "H",
        start_time = date_start,
        interpolate = NULL, #interpolate,
        subset = subset_date)
paste("Simulated heads written to",file_head_out)

## 2.2 GW observed heads ####
# Write observed data if output filename was supplied
if(exists("file_head_obs_out")){
  head_obs <- HOAL.data::GWL[subset_date, StGW]
  head_obs <- head_obs[startpoints(head_obs, on = interpolate),]
  xts2smp(head_obs, fn_out = paste0(dir,"/", file_head_obs_out))
  #paste("Observed heads written to",file_head_obs_out)
}

#==============================================================================#
#### 3. Discharge ####

## 3.1 Simulated Discharge ####
Q_files <- list.files(path = dir, pattern = "(hydrograph)(.+)(\\.dat)$", full.names = T)
# keep only files of selected stations
Q_files <- Q_files[str_extract(Q_files,pattern = "(?<=hydrograph\\.)(.+)(?=\\.dat)") %in% StQ]
if(length(Q_files) < 1) stop("No modeled discharges found!")

# Convert head data to Bore sample file
hgs2smp(file_in = Q_files, 
        file_out = paste0(dir,"/",file_Q_out),
        var_name = "Surface",
        start_time = date_start,
        interpolate = NULL, #interpolate,
        subset = subset_date)
paste("Simulated flows written to",file_Q_out)

## 2.2 Q observed discharge ####
if(exists("file_Q_obs_out")){
  # Change the name outflow with MW which is found in HOAL.Discharge
  StQ2 <- ifelse("outflow" %in% StQ, replace(StQ, StQ == "outflow", "MW"), StQ)
  Q_obs <- HOAL.data::HOAL.Discharge[subset_date, StQ2]
  # change the name back from MW to outflow
  colnames(Q_obs) <- ifelse("MW" %in% colnames(Q_obs), 
                            replace(colnames(Q_obs), colnames(Q_obs) == "MW", "outflow"),
                            colnames(Q_obs))
  # interpolate to the time step given by "interpolate" and convert from l/s to m3/day
  Q_obs <- Q_obs[startpoints(Q_obs, on = interpolate),] / 1000 * 86400
  xts2smp(Q_obs, fn_out = paste0(dir,"/",file_Q_obs_out))
  paste("Observed flows written to",file_Q_obs_out)
}
