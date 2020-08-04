######################
#R scripts: k2HGS
#
#Function: Read hydraulic conductivity field from fac2g and tranfer it to a readable HydroGeoSphere input
######################

args = commandArgs(trailingOnly = T)
if(length(args) == 2){
  file.in = args[1]
  file.out = args[2]
}else{
  file.in = "k_interpolated.txt"
  file.out = "k_calibrated.txt"
}

# read output from ppk2facg
x <- scan(file.in)
elements <- seq(1:length(x))
y <- cbind(elements,x,x,x)
# write new hydraulic conductivity field for HGS
write.table(y, file.out, row.names=FALSE, col.names=FALSE, quote = F, sep = "\t")
