#==============================================================================#
#
#     Reading and exporting HGS outputs
#
#==============================================================================#
# scan for additional arguments

#### 0. Libraries ####
source("D:/PhD/HOAL/modelling/HGS_model/Scripts/RHGS/HGSfun.R", echo = F)

# scan for additional arguments
args = commandArgs(trailingOnly = T)

# test if there are all required arguments
if(length(args) == 0) {
  stop("no arguments provided", call. = F)
} else if (length(args) != 2){
  stop("invalid number of arguments provided", call. = F)
}else {
  date_start <- as.Date(args[1])
  overwrite <- as.logical(args[2])
}

#==============================================================================#
#### 1. Preparation ####
dir <- getwd()

# Create "runs" directory
dir.create("runs", showWarnings = FALSE)

# get the model prefix (e.g. "HOAL_bld")
prefix <- stringr::str_sub(list.files(path = dir, pattern = "(\\.grok)$"), end = -6)

## create model run export directory (e.g. "runs/HOAL_bld_run_01)

# First find existing run directories
existing <- str_subset(list.dirs(path = fs::path_wd("runs/"), full.names = F),
                       pattern = paste0("^(", prefix, ")"))
if(length(existing) == 0){
  # create first run directory if none exist
  dir.out <- paste0("runs/", prefix, "_run_01")
}else{
  # find latest run number
  runN <- which.max(as.numeric(str_sub(existing, -2)))
  dir.out <- paste0("runs/", prefix, "_run_", str_pad(ifelse(overwrite, runN, runN+1),
                                                      width = 2,pad = "0"))
}
dir.create(dir.out)

#==============================================================================#
#### 2. Copy relavant files ####

# Grok file
file.copy(from = paste0(dir,"/",prefix, ".grok"),
          to = paste0(dir.out,"/",prefix, ".grok"))

# head files
file.copy(from = paste0(dir, "/", list.files(dir, pattern = "(o.head_)",full.names = F)),
          to = paste0(dir.out, "/", list.files(dir, pattern = "(o.head_)",full.names = F)))

# lst file
file.copy(from = paste0(dir,"/",prefix, "o.lst"),
          to = paste0(dir.out,"/",prefix, "o.lst"))

# cleanup file
file.copy(from = paste0(dir,"/Cleanup.bat"),
          to = paste0(dir.out,"/Cleanup.bat"))
# Batch file
file.copy(from = paste0(dir,"/batch.pfx"),
          to = paste0(dir.out,"/batch.pfx"))

# paralelindx
file.copy(from = paste0(dir,"/parallelindx.dat"),
          to = paste0(dir.out,"/parallelindx.dat"))

#water balance
file.copy(from = paste0(dir,"/",prefix, "o.water_balance.dat"),
          to = paste0(dir.out,"/",prefix, "o.water_balance.dat"))

# copy included files
fs::dir_copy(path = paste0(dir, "/include"),
          new_path = paste0(dir.out, "/include"))

# default array
file.copy(from = paste0(dir,"/array_sizes.default"),
          to = paste0(dir.out,"/array_sizes.default"))


#==============================================================================#
#### 3. Plots ####

# Hydrograph

file.copy(from = paste0(dir, "/", prefix, "o.hydrograph.outflow.dat"),
          to = paste0(dir.out, "/", prefix, "o.hydrograph.outflow.dat"))
plot.hydrograph.tecplot(paste0(dir, "/", prefix, "o.hydrograph.outflow.dat"), save = T,date_start = date_start)
file.copy(from = paste0(dir, "/", prefix, "o.hydrograph.outflow.png"),
          to = paste0(dir.out, "/", prefix, "o.hydrograph.outflow.png"))
# Observation points
file.copy(from = paste0(dir, "/", list.files(dir, pattern = "(observation_well)(.+)(\\.dat)")),
          to = paste0(dir.out, "/", list.files(dir, pattern = "(observation_well)(.+)(\\.dat)")))
plot.sim.heads(dir,save = T)
file.copy(from = paste0(dir, "/", list.files(dir, pattern = "(observation_well)(.+)(\\.png)")),
          to = paste0(dir.out, "/", list.files(dir, pattern = "(observation_well)(.+)(\\.png)")))

# Water balance