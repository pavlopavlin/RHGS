#==============================================================================#
#
#                                 Observations for PEST
#
#==============================================================================#

#### 0. Libraries ####
# scan for additional arguments
args = commandArgs(trailingOnly = T)

# source functions for HGS
source("D:/PhD/HOAL/modelling/HGS_model/Scripts/RHGS/HGSfun.R", echo = F, verbose = F, print.eval = F, prompt.echo = F)

# test if there are all required arguments
if(length(args) == 0) {
  stop("no arguments provided", call. = F)
} else if (length(args) < 7){
  stop("invalid number of arguments provided", call. = F)
} else{
  # this is what should be supplied through the batch script
  # %StGW% %StQ% %date_start% %interpolate% %file_head_obs% %file_Q_obs% %subset_date%
  StGW <- unlist(str_split(args[1], "\\,\\s*"))
  StQ <-  unlist(str_split(args[2], "\\,\\s*"))
  date_start <- as.Date(args[3])
  interpolate <- args[4]
  file_head_obs_out <- args[5]
  file_Q_obs_out <- args[6]
  subset_date <- args[7]
}

#==============================================================================#
#### 1. Preparation ####
dir <- getwd()


