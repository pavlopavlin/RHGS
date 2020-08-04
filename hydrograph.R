library(HOAL)
library(dygraphs)

#' Remove successive duplicated values
#'
#' @param x variable with duplicated values, coercable to numeric vector.
#' @param side which duplicated values should be retained. Default is "both" which
#'   retains first and last value of the successive duplicated values. Other options
#'   are "left" and "right" which retain the first or the last value, respectively.
#' @param col if "x" has multiple columns, based on which should the values be removed.
#' 
#' @details This function is usefull for e.g. removing time-steps with 0 precipitation
#'   for the use in HGS.
#' 
#' @return object of the same class as "x" with removed successive duplicated values.
#' @export
#'
#' @examples
rm.dupl <- function(x, side = "both", col = 1){
  if(side == "both"){
    l = 1
    r = 1
  }else if(side == "left"){
    l = 1
    r = 0
  }else if(side == "right"){
    l = 0
    r = 1
  }else{
    stop (paste0("Argument 'side' [", side, "] is not valid."))
  }
  x.rle <- rle(as.numeric(x[,col]))
  for(ii in 1:length(x.rle$lengths)){
    if(x.rle$lengths[ii] > 2){
      x[(sum(x.rle$lengths[0:(ii-1)])+1+l) : (sum(x.rle$lengths[0:ii])-r), col] <- NA
    }
  }
  return(x[!is.na(x[,col]),])
}


#' Efficiency measures
#'
#' @param sim object of simulated values coercable to numeric
#' @param obs object of observed values coercable to numeric
#' @param warmup length of the warmup period
#'
#' @details 
#'   calculates following measures:
#'   \itemize{
#'   \item RMSE
#'   \item Nash efficiency
#'   \item log Nash efficiency
#'   \item  bias
#'   \item mean absolute error
#'   \item MAlE = mean absolute log error
#'   \item VE = volume error
#'   }
#' @return matrix of
#' @export
#'
#' @examples
EMs <- function (sim, obs, warmup=0) {
  # obs = observed runoff in mm/d (class numeric)
  # sim = simulated runoff in mm/d (class numeric)
  # warmup = warm-up period in d
  
  simu <- as.numeric(sim[-c(1:warmup)])
  obse <- as.numeric(obs[-c(1:warmup)])
  
  # RMSE = root mean square error (mm/d)
  RMSE <- sqrt(mean((simu - obse)^2, na.rm=TRUE))
  
  # NE = Nash efficiency ()
  mobse <- mean(obse, na.rm=TRUE)
  NE <- 1 - sum((simu - obse)^2, na.rm=TRUE)/sum((obse - mobse)^2, na.rm=TRUE)
  
  # lNE = log Nash efficiency ()
  mlobse <- mean(log(obse), na.rm=TRUE)
  lNE <- 1 - sum((log(simu) - log(obse))^2, na.rm=TRUE)/sum((log(obse) - mlobse)^2, na.rm=TRUE)
  
  # B = bias (mm/d)
  B <- mean(simu - obse, na.rm=TRUE)
  
  # MAE = mean absolute error (mm/d)
  MAE <- mean(abs(simu - obse), na.rm=TRUE)
  
  # MAlE = mean absolute log error (mm/d)
  MAlE <- exp(mean(abs(log(simu) - log(obse)), na.rm=TRUE))
  
  # VE = volume error (%/%)
  VE <- (sum(simu[!is.na(obse)]) - sum(obse, na.rm=TRUE))/sum(obse, na.rm=TRUE)

  output <- c(RMSE, NE, lNE, B, MAE, MAlE, VE)
  names(output) <- c("RMSE (mm/d)", "Nash efficiency ()", "log Nash efficiency ()", "bias (mm/d)", 
                     "mean absolute error (mm/d)", "mean absolute log error (mm/d)", "volume error (%/%)")
  return(output)
}

dysave <- function(dygraph, path, interactive = T){
  if(interactive == T){
    htmlwidgets::saveWidget(dygraph, paste0(path,".html"),
                            selfcontained = T,
                            libdir = paste0(path, "_files"))
  }
  
  # if(interactive == T){
  #   webshot::webshot(paste0(path,".html"),
  #                    paste0(dir.export,"/Plots/EventHydrographsPNG/Hydrograph_",
  #                           format(events[ii,1],"%Y-%m-%d"),"_",
  #                           format(events[ii,2],"%Y-%m-%d"),".png"),
  #                    vwidth = 2000, vheight = 1400)
  # }    
}

read.tecplot <- function(filename){
  txt <- readLines(filename)
  
  # numerical part
  num <- txt[-3:-1]
  #remove leading space
  num <- stringr::str_replace(num, "^(\\s+)","")
  #remove trailing space
  num <- stringr::str_replace(num, "(\\s+)$","")
  #split by space
  num <- stringr::str_split(num,"\\s+",simplify = T)
  num <- as.data.frame(matrix(as.numeric(num), ncol = ncol(num)))
  
  NAMES <- stringr::str_remove_all(txt[2],'\"')
  NAMES <- stringr::str_remove(NAMES, "^.*(?:(=))")
  NAMES <- stringr::str_split(NAMES, ",", simplify = T)
  NAMES <- stringr::str_remove(NAMES, "^(\\s+)")
  NAMES <- stringr::str_remove(NAMES, "(\\s+)$")
  colnames(num) <- NAMES[NAMES != ""]
  
  return(list(header = txt[1:3], data = num))
}


# read.tecplot.block <- function(filename){
#   txt <- readLines(filename)
#   
#   vars <- grep("(#\\s\\w)", x = txt, perl=T)
#   
#   data <- list()
#   for (ii in 1:(length(vars)-1)){
#     tmp <- txt[(vars[ii]+1):(vars[ii+1]-1)]
#     tmp <- stringr::str_replace(tmp, "^(\\s+)","")
#     #remove trailing space
#     tmp <- stringr::str_replace(tmp, "(\\s+)$","")
#     #split by space
#     tmp <- stringr::str_split(tmp,"\\s+",simplify = F)
#     print(length(tmp))
#     #data[[ii]] <- as.numeric(unlist(tmp))
#   }
#   
#   tmp <- txt[(vars[length(vars)]+1):length(txt)]
#   tmp <- stringr::str_replace(tmp, "^(\\s+)","")
#   #remove trailing space
#   tmp <- stringr::str_replace(tmp, "(\\s+)$","")
#   #split by space
#   tmp <- stringr::str_split(tmp,"\\s+",simplify = F)
#   data[[length(vars)]] <- as.numeric(unlist(tmp))
#   data <- as.data.frame(data)
#   colnames(data) <- stringr::str_remove(txt[vars], "#\\s")
#   
# }

dat2csv <- function(filename, dir){
  require(stringr)
  
  if(all(missing(filename), missing(dir))){
    stop("Atleast one must be supplied: 'filename', 'dir'")
  }
  if(missing(filename)){
    filename <- list.files(path = dir, pattern = "(observation_well)(.+)(\\.dat)",full.names = T)
    filename <- append(filename, list.files(path = dir, pattern = "outflow_hydrograph.dat",full.names = T))
  }
  
  out <- list()
  
  for(ii in filename){
    temp <- read.tecplot(ii)
    
    write.table(temp$header, paste0(substring(ii, 1, nchar(ii)-4),".csv"),
                quote = F,row.names = F, col.names = F, sep = ",")
    write.table(temp$data, paste0(substring(ii, 1, nchar(ii)-4),".csv"),
                quote = F,row.names = F, append = T, sep = ",")
    
    out <- c(out, temp)
  }
  invisible(out)
}

plot.hydrograph.tecplot <- function(dat, facet = T, save = F, units =  c("kilogram", "metre", "day")){
  file <- read.tecplot(dat)
  g1 <- ggplot2::ggplot(reshape2::melt(data.frame(file$data), id.vars = 1),
                        ggplot2::aes(Time, value, color = variable))+
    ggplot2::geom_path()+
    ggplot2::geom_point() +
    ggplot2::labs(title = file$header[1], x = paste0("Time [", units[3], "]"))
  if(facet)g1 <- g1 + ggplot2::facet_wrap(~variable, ncol = 1, scales = "free_y")
  
  if(save){
    ggplot2::ggsave(plot = g1, paste0(substring(dat, 1, nchar(dat)-4),".png"))
  }
  g1
}

plot.tecplot <- function(dat, subset, save = F, units =  c("kilogram", "metre", "day")){
  file <- read.tecplot(dat)
  if(missing(subset)) subset <- colnames(file$data)
  if(is.null(subset)) subset <- colnames(file$data)
  g1 <- ggplot2::ggplot(reshape2::melt(data.frame(file$data[,subset]), id.vars = 1),
               ggplot2::aes(Time, value, color = variable))+
    ggplot2::geom_path(show.legend = F)+
    ggplot2::geom_point(show.legend = F) +
    ggplot2::facet_wrap(~variable, ncol=1, scales = "free_y") + 
    ggplot2::labs(title = "Outflow hydrograph", x = paste0("Time [", units[3], "]"),
         y = paste0("Discharge [", units[2], "^3/", units[3], "]"))  
  if(save){
    ggplot2::ggsave(plot = g1, paste0(substring(dat, 1, nchar(dat)-4),".png"))
  }
  g1
}

plot.sim.heads <- function(dir, save = F, obs = T, units =  c("kilogram", "metre", "day")){
  filename <- list.files(path = dir, pattern = "(observation_well)(.+)(\\.dat)$",full.names = T)
  if(length(filename) < 1) stop("No observations found!")
  data <- lapply(filename, read.tecplot)
  df <- as.data.frame(
    cbind(data[[1]]$data$Time,
              sapply(data, function(x) x$data$H))
  )
  colnames(df) <- c("Time", sapply(data, function(x) stringr::str_extract(x$header[1], "(?<=(well:\\s))\\w+\\d+")))
  df_mlt <- reshape2::melt(df, id.vars = 1)
  
  g1 <-
    ggplot2::ggplot(df_mlt, ggplot2::aes(Time, value, color = variable))+
    ggplot2::geom_path(show.legend = F) +
    ggplot2::facet_wrap(~variable, ncol=2, scales = "free_y",strip.position = "right") + 
    ggplot2::labs(title = "Simulated heads", x = paste0("Time [", units[3], "]"),
                  y = "Groundwater table [m a.s.l.]")
  if(obs == T){
    obs_mean <- data.frame(variable = colnames(HOAL.data::GWL),
                           value = sapply(HOAL.data::GWL, mean, na.rm=T))
    obs_mean <- obs_mean[obs_mean$variable %in% unique(df_mlt$variable),]
    g1 <- g1 +
      geom_hline(aes(yintercept = value), obs_mean)
    
    obs_max <- data.frame(variable = colnames(HOAL.data::GWL),
                           value = sapply(HOAL.data::GWL, max, na.rm=T))
    obs_max <- obs_max[obs_max$variable %in% unique(df_mlt$variable),]
    g1 <- g1 +
      geom_hline(aes(yintercept = value), obs_max, lty = "dashed")
    
    obs_min <- data.frame(variable = colnames(HOAL.data::GWL),
                           value = sapply(HOAL.data::GWL, min, na.rm=T))
    obs_min <- obs_min[obs_min$variable %in% unique(df_mlt$variable),]
    g1 <- g1 +
      geom_hline(aes(yintercept = value), obs_min, lty = "dashed")
  }
  
  if(save){
    ggplot2::ggsave(plot = g1, width = 12, height = 12, dpi = 600,
                    filename = 
                    paste0(dir, "/",
                           stringr::str_extract(list.files(path=dir, pattern = "(observation_well)(.+)(\\.dat)$")[1],
                                                "^(.+\\.observation.well_flow)"),
                           ".png"))
  }
  return(g1)
}

plot.sim.point <- function(dir, obs.point, 
                           start = as.POSIXct("2013-01-01",tz="etc/GMT-1"),
                           station = obs.point){
  
filename <- list.files(path = dir, 
                       pattern = paste0("(observation_well)(.+)(", obs.point ,".dat)$"),
                       full.names = T)
if(length(filename) != 1) stop(paste("Files found", length(filename), ", expected = 1."))

x <- read.tecplot(filename)
x.xts <- 
  to_timestep(xts(x$data[,-1], 
                  order.by = start + x$data$Time*86400),
              "days")
obs <- to_timestep(HOAL.data::GWL[paste0(as.character(start),"/"),station], "days")
sim <- x.xts$H
colnames(obs) <- paste0(obs.point, ".obs")
colnames(sim) <- paste0(obs.point, ".sim")
dygraphs::dygraph(merge(obs, sim))
}

#==============================================================================#
# Data ####

P_d <- to_timestep(HOAL.data::HOAL.Rain.Mean5, "days", sum)
P_d <- P_d["2013/2017"] /1000
P_d$Time <- 0:nrow(P_d[-1,])
P_d2 <- rm.dupl(P_d)
write.table(coredata(P_d2[,2:1]), "D:/PhD/HOAL/modelling/HGS_model/data/time-series/P_2013-2017_start=0_2.txt",
            row.names = F, sep = "  ", col.names = F)


ET_d <-HOAL.data::HOAL.ET.eddy
ET_d <- ET_d["2013/2017"] / 1000
ET_d$Time <- 0:nrow(ET_d[-1,])
ET_d2 <- rm.dupl(ET_d)
write.table(coredata(ET_d2[,2:1]), "D:/PhD/HOAL/modelling/HGS_model/data/time-series/ET_2013-2017_start=0_2.txt",
            row.names = F, sep = "  ", col.names = F)
write.table(coredata(ET_d2[,2:1]), "D:/PhD/HOAL/modelling/HGS_model/data/time-series/ETx2_2013-2017_start=0.txt",
            row.names = F, sep = "  ", col.names = F)

Q_d <- to_timestep(HOAL.data::HOAL.Discharge["2013/"], "days")*86400/1000
Q_d2 <-Q_d
Q_d2[Q_d2 == 0]<- NA

#=============================================================================#
# Simulations ####

# HOAL_bld
plot.sim.heads("D:/PhD/HOAL/modelling/HGS_model/HOAL_bld",save = T)
plot.hydrograph.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_bld/HOAL_bldo.hydrograph.outflow_hydrograph.dat", save =T)
Q.bld <-read.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_bld/HOAL_bldo.hydrograph.outflow_hydrograph.dat")
Q.bld.xts <- 
  to_timestep(xts(Q.bld$data, 
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + Q.bld$data$Time*86400),
              "days")
dybld <- dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], Q.bld.xts$Total))
dybld
dysave(dybld, "D:/PhD/HOAL/modelling/HGS_model/HOAL_bld/HOAL_bldo.hydrograph.outflow_hydrograph")

WB.bld <- read.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_bld/HOAL_bldo.water_balance.dat")
WB.bld.xts <-
  to_timestep(xts(WB.bld$data,
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + WB.bld$data$Time*86400),
              "days")
dygraph(merge(ET_d$ET..mm.day.*raster::area(HOAL.data::HOAL.boundary),
              abs(WB.bld.xts$Tot_ET)))
dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], abs(WB.bld.xts[,c("outflow-boundary","sys4-outlet")])))
dygraph(merge(Q_d["2013/2017", c("MW","Sys1","Sys2","Sys3","Sys4","Frau1", "Frau2")],
              abs(WB.bld.xts[,c(3,5,7,9,11,13,16)])))


EMs(Q.bld.xts$Total, Q_d2$MW[1:nrow(Q.bld.xts)], warmup = 100)
EMs(WB.bld.xts$`frau1-outlet`, Q_d2$Frau1[1:nrow(WB.bld.xts)], warmup = 10)[2]
EMs(WB.bld.xts$`frau2-outlet`, Q_d2$Frau2[1:nrow(WB.bld.xts)], warmup = 10)[2]
EMs(WB.bld.xts$`sys1-outlet`, Q_d2$Sys1[1:nrow(WB.bld.xts)], warmup = 10)[2]
EMs(WB.bld.xts$`sys2-outlet`, Q_d2$Sys2[1:nrow(WB.bld.xts)], warmup = 10)[2]
EMs(WB.bld.xts$`sys3-outlet`, Q_d2$Sys3[1:nrow(WB.bld.xts)], warmup = 10)[2]
EMs(WB.bld.xts$`sys4-outlet`, Q_d2$Sys4[1:nrow(WB.bld.xts)], warmup = 10)[2]

# HOAL_3layer
plot.sim.heads("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer",save = T)
plot.hydrograph.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer/HOAL_3layero.hydrograph.outflow_hydrograph.dat", save =T)
Q.3layer <-read.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer/HOAL_3layero.hydrograph.outflow_hydrograph.dat")
Q.3layer.xts <- 
  to_timestep(xts(Q.3layer$data, 
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + Q.3layer$data$Time*86400),
              "days")
dy3layer <- dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], Q.3layer.xts$Total))
dy3layer
dysave(dy3layer, "D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer/HOAL_3layero.hydrograph.outflow_hydrograph")

WB.3layer <- read.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer/HOAL_3layero.water_balance.dat")
WB.3layer.xts <-
  to_timestep(xts(WB.3layer$data,
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + WB.3layer$data$Time*86400),
              "days")
dygraph(merge(ET_d$ET..mm.day.*raster::area(HOAL.data::HOAL.boundary),
              abs(WB.3layer.xts$Tot_ET)))
dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], abs(WB.3layer.xts[,c("outflow-boundary","sys4-outlet")])))
dygraph(merge(Q_d["2013/2017", c("MW","Sys1","Sys2","Sys3","Sys4","Frau1", "Frau2")],
              abs(WB.3layer.xts[,c(3,5,7,9,11,13,16)])))
dygraph(merge(Q_d["2013/2017","MW"], abs(WB.3layer.xts[,c(2,4,6,8,10,12,14:18)])))%>%
  dySeries("rain", axis = "y2") %>%
  dySeries("ET_AET", axis="y2")

EMs(Q.3layer.xts$Total, Q_d$MW[1:nrow(Q.3layer.xts)], warmup = 100)
EMs(WB.3layer.xts$`frau1-outlet`, Q_d2$Frau1[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`frau2-outlet`, Q_d2$Frau2[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys1-outlet`, Q_d2$Sys1[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys2-outlet`, Q_d2$Sys2[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys3-outlet`, Q_d2$Sys3[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys4-outlet`, Q_d2$Sys4[1:nrow(WB.3layer.xts)], warmup = 10)[2]

# HOAL_3layer - Copy
plot.sim.heads("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer - Copy",save = T)
plot.hydrograph.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer - Copy/HOAL_3layero.hydrograph.outflow_hydrograph.dat", save =T)
Q.3layer <-read.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer - Copy/HOAL_3layero.hydrograph.outflow_hydrograph.dat")
Q.3layer.xts <- 
  to_timestep(xts(Q.3layer$data, 
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + Q.3layer$data$Time*86400),
              "days")
dy3layer <- dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], Q.3layer.xts$Total))
dy3layer
dysave(dy3layer, "D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer - Copy/HOAL_3layero.hydrograph.outflow_hydrograph")

WB.3layer <- read.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer - Copy/HOAL_3layero.water_balance.dat")
WB.3layer.xts <-
  to_timestep(xts(WB.3layer$data,
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + WB.3layer$data$Time*86400),
              "days")
dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], abs(WB.3layer.xts[,c("outflow-boundary","sys4-outlet")])))
dygraph(merge(Q_d["2013/2017", c("MW","Sys1","Sys2","Sys3","Sys4","Frau1", "Frau2")],
              abs(WB.3layer.xts[,c(3,5,7,9,11,13,16)])))


EMs(Q.3layer.xts$Total, Q_d2$MW[1:nrow(Q.3layer.xts)], warmup = 100)
EMs(WB.3layer.xts$`frau1-outlet`, Q_d2$Frau1[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`frau2-outlet`, Q_d2$Frau2[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys1-outlet`, Q_d2$Sys1[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys2-outlet`, Q_d2$Sys2[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys3-outlet`, Q_d2$Sys3[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys4-outlet`, Q_d2$Sys4[1:nrow(WB.3layer.xts)], warmup = 10)[2]

# HOAL_3layer - Copy (2)
plot.sim.heads("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer - Copy (2)",save = T)
plot.hydrograph.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer - Copy (2)/HOAL_3layero.hydrograph.outflow_hydrograph.dat", save =T)
Q.3layer <-read.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer - Copy (2)/HOAL_3layero.hydrograph.outflow_hydrograph.dat")
Q.3layer.xts <- 
  to_timestep(xts(Q.3layer$data, 
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + Q.3layer$data$Time*86400),
              "days")
dy3layer <- dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], Q.3layer.xts$Total))
dy3layer
dysave(dy3layer, "D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer - Copy (2)/HOAL_3layero.hydrograph.outflow_hydrograph")

WB.3layer <- read.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_3layer - Copy (2)/HOAL_3layero.water_balance.dat")
WB.3layer.xts <-
  to_timestep(xts(WB.3layer$data,
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + WB.3layer$data$Time*86400),
              "days")
dygraph(merge(ET_d$ET..mm.day.*raster::area(HOAL.data::HOAL.boundary), abs(WB.3layer.xts$Tot_ET)))
dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], abs(WB.3layer.xts[,c("outflow-boundary","sys4-outlet")])))
dygraph(merge(Q_d["2013/2017", c("MW","Sys1","Sys2","Sys3","Sys4","Frau1", "Frau2")],
              abs(WB.3layer.xts[,c(3,5,7,9,11,13,16)])))


EMs(Q.3layer.xts$Total, Q_d2$MW[1:nrow(Q.3layer.xts)], warmup = 100)
EMs(WB.3layer.xts$`frau1-outlet`, Q_d2$Frau1[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`frau2-outlet`, Q_d2$Frau2[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys1-outlet`, Q_d2$Sys1[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys2-outlet`, Q_d2$Sys2[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys3-outlet`, Q_d2$Sys3[1:nrow(WB.3layer.xts)], warmup = 10)[2]
EMs(WB.3layer.xts$`sys4-outlet`, Q_d2$Sys4[1:nrow(WB.3layer.xts)], warmup = 10)[2]

# HOAL_experimental
plot.sim.heads("D:/PhD/HOAL/modelling/HGS_model/HOAL_experimental",save = T)
plot.hydrograph.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_experimental/HOAL_expo.hydrograph.outflow_hydrograph.dat", save =T)

Q.exp <-read.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_experimental/HOAL_expo.hydrograph.outflow_hydrograph.dat")
Q.exp.xts <- 
  to_timestep(xts(Q.exp$data, 
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + Q.exp$data$Time*86400),
              "days")
dyexp <- dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], Q.exp.xts$Total))
dyexp
dysave(dyexp, "D:/PhD/HOAL/modelling/HGS_model/HOAL_experimental/HOAL_expo.hydrograph.outflow_hydrograph")

WB.exp <- read.tecplot("D:/PhD/HOAL/modelling/HGS_model/HOAL_experimental/HOAL_expo.water_balance.dat")
WB.exp.xts <-
  to_timestep(xts(WB.exp$data,
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + WB.exp$data$Time*86400),
              "days")
dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], abs(WB.exp.xts[,c("outflow-boundary","ET_17_AET","sys4-outlet")])))
dygraph(merge(Q_d["2013/2017", c("MW","Sys1","Sys2","Sys3","Sys4","Frau1", "Frau2")],
              abs(WB.bld.xts[,c(3,5,7,9,11,13,16)])))
EMs(Q.exp.xts$Total, Q_d2$MW[1:nrow(Q.exp.xts)], warmup = 300)
EMs(WB.exp.xts$`frau1-outlet`, Q_d2$Frau1[1:nrow(WB.exp.xts)], warmup = 10)[2]
EMs(WB.exp.xts$`frau2-outlet`, Q_d2$Frau2[1:nrow(WB.exp.xts)], warmup = 10)[2]
EMs(WB.exp.xts$`sys1-outlet`, Q_d2$Sys1[1:nrow(WB.exp.xts)], warmup = 10)[2]
EMs(WB.exp.xts$`sys2-outlet`, Q_d2$Sys2[1:nrow(WB.exp.xts)], warmup = 10)[2]
EMs(WB.exp.xts$`sys3-outlet`, Q_d2$Sys3[1:nrow(WB.exp.xts)], warmup = 10)[2]
EMs(WB.exp.xts$`sys4-outlet`, Q_d2$Sys4[1:nrow(WB.exp.xts)], warmup = 10)[2]


# HOAL_res
dir.res <- "D:/PhD/HOAL/modelling/HGS_model/HOAL_macropores_inv"
prefix.res <- "HOAL_macropores_inv"
plot.sim.heads(dir.res,save = T)
plot.hydrograph.tecplot(paste0(dir.res,"/",prefix.res, "o.hydrograph.outflow_hydrograph.dat"), save =T)
Q.res <-read.tecplot(paste0(dir.res,"/",prefix.res, "o.hydrograph.outflow_hydrograph.dat"))
Q.res.xts <- 
  to_timestep(xts(Q.res$data, 
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + Q.res$data$Time*86400),
              "days")
saveRDS(Q.res.xts, paste0(dir.res,"/",prefix.res, "o.hydrograph.outflow_hydrograph.RDS"))
dyres <- dygraph(merge(Q_d["2013/2017",c("MW","Sys4")], Q.res.xts$Total))
dyres
dysave(dyres, paste0(dir.res,"/",prefix.res, "o.hydrograph.outflow_hydrograph"))

WB.res <- read.tecplot(paste0(dir.res,"/",prefix.res,"o.water_balance.dat"))
WB.res.xts <-
  to_timestep(xts(WB.res$data,
                  order.by = as.POSIXct("2013-01-01", tz="Etc/GMT-1") + WB.res$data$Time*86400),
              "days")
saveRDS(WB.res.xts, paste0(dir.res,"/",prefix.res, "o.water_balance.RDS"))

EMs(Q.res.xts$Total, Q_d2$MW[1:nrow(Q.res.xts)], warmup = 10)
hydroGOF::KGE.zoo(sim = Q.res.xts$Total, obs = Q_d$MW[1:nrow(Q.res.xts)],na.rm = T)

# dy3layer <- dygraph(merge(Q_d["2013/2017",c("MW")], Q.3layer.xts$Total,Q.3layer_dual.xts$Total))%>%
#   dyOptions(strokeWidth = 2,colors = RColorBrewer::brewer.pal(3, "Set2"))%>%
#   dyAxis(name="y", label="Discharge [m3/day]")%>%
#   dyLegend(labelsSeparateLines=T)%>%
#   dySeries("MW", label = "MW Observed")%>%
#   dySeries("Total",label="High Conductivity (K = 200m/day)")%>%
#   dySeries("Total.1",label="Dual porosity")

dycompare <- 
  dygraph(merge(Q_d["2013/2017",c("MW","Sys1","Sys2","Sys3","Sys4","Frau1","Frau2")],
                abs(WB.res.xts[,c("outflow-boundary","sys1-outlet", "sys2-outlet",
                                 "sys3-outlet","sys4-outlet", "frau1-outlet","frau2-outlet")]))) %>%
  dyOptions(strokeWidth = 2, colors = rep(RColorBrewer::brewer.pal(7, "Set2"),2))%>%
  dyAxis(name="y", label="Discharge [m3/day]")%>%
  dySeries("outflow.boundary", label = "MW - Sim", strokePattern = "dashed") %>%
  dySeries("sys1.outlet", label = "Sys1 - Sim", strokePattern = "dashed") %>%
  dySeries("sys2.outlet", label = "Sys2 - Sim", strokePattern = "dashed") %>%
  dySeries("sys3.outlet", label = "Sys3 - Sim", strokePattern = "dashed") %>%
  dySeries("sys4.outlet", label = "Sys4 - Sim", strokePattern = "dashed") %>%
  dySeries("frau1.outlet", label = "Frau1 - Sim", strokePattern = "dashed") %>%
  dySeries("frau2.outlet", label = "Frau2 - Sim", strokePattern = "dashed")
dycompare
