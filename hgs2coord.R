#==============================================================================#
#
#     Convert HGS output to Bore sample file
#
#==============================================================================#

#### 0. Libraries ####
# scan for additional arguments
args = commandArgs(trailingOnly = T)

source("D:/PhD/HOAL/modelling/HGS_model/Scripts/RHGS/HGSfun.R", 
       echo = F, verbose = F, print.eval = F, prompt.echo = F)

# test if there are all required arguments
if(length(args) == 0) {
  stop("no arguments provided", call. = F)
} else if (length(args) != 1){
  stop("invalid number of arguments provided", call. = F)
} else{
  # this is what should be supplied through the batch script
  # %fn_mesh%
  fn_mesh = args[1]
  
}

#==============================================================================#
#### 1. ####
dir <- getwd()
mesh2cord(in_mesh = fn_mesh)


