##----------------------------------------------------------------------------##
##
##          Utility functions for work with HydroGeoSphere
##
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
#### 0. Libraries ####

suppressMessages(library(HOAL, quietly = T, verbose = F,warn.conflicts = F))
suppressMessages(library(tidyverse, quietly = T, verbose = F,warn.conflicts = F))
suppressMessages(library(lubridate, quietly = T, verbose = F,warn.conflicts = F))
suppressMessages(library(dygraphs, quietly = T, verbose = F,warn.conflicts = F))
suppressMessages(library(magrittr, quietly = T, verbose = F,warn.conflicts = F))


#==============================================================================#
#### Reading Tecplot data ####
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

#' Read Tecplot file
#'
#' Reads a Tecplot ASCII file (.dat) to a data frame. The observation point 
#' name is added as a "Station" attribute
#'
#' @param filename the name of the file which the data are to be read from.
#'
#' @return data frame with Station attribute 
#' @export
read.tecplot <- function(filename){
  txt <- readLines(filename)
  
  # Extract title from the file (1 or more lines)
  ln_title <- grep(pattern = "^(TITLE)", x = txt, ignore.case = T)
  title <- paste(trimws(stringr::str_extract(txt[ln_title], 
                                             pattern = '(?<=\").+(?=\")')), 
                 collapse =  "; ")
  # Extract variables (1 line)
  ln_vars <- grep("^(VARIABLES)", x = txt, ignore.case = T)
  vars <- stringr::str_extract(
    stringr::str_split(trimws(txt[ln_vars]),",")[[1]], 
    pattern = '(?<=\").+(?=\")')
  
  # zone line
  ln_zone <- grep("^(zone)", x = txt, ignore.case = T)
  
  # numerical part
  num <- read.table(filename, stringsAsFactors = F, skip = ln_zone, 
                    #col.names = vars, 
                    strip.white = T, fill = F)
  if(ncol(num) > length(vars)){
    num <- num[,sapply(colnames(num), FUN = function(x) !all(num[,x] == 0))]
  }
  
  # #remove leading space
  # num <- stringr::str_replace(num, "^(\\s+)","")
  # #remove trailing space
  # num <- stringr::str_replace(num, "(\\s+)$","")
  # #split by space
  # num <- stringr::str_split(num,"\\s+",simplify = T)
  # num <- as.data.frame(matrix(as.numeric(num), ncol = ncol(num)))
  # 
  # NAMES <- stringr::str_remove_all(txt[2],'\"')
  # NAMES <- stringr::str_remove(NAMES, "^.*(?:(=))")
  # NAMES <- stringr::str_split(NAMES, ",", simplify = T)
  # NAMES <- stringr::str_remove(NAMES, "^(\\s+)")
  # NAMES <- stringr::str_remove(NAMES, "(\\s+)$")
  colnames(num) <- vars[vars != ""]
  
  # add station name as an attribute
  #attr(num, "Station") <- str_extract(txt[1],"(?<=\\:\\s)([:alnum:]+)") # from text title
  attr(num, "Station") <- str_extract(filename,"(?<=\\.)([[:alnum:]_]+)(?=\\.dat)") # from filename
  #return(list(header = txt[1:3], data = num))
  return(num)
}

#------------------------------------------------------------------------------#

#' Reads a tecplot text file (.dat) to an xts object
#'
#' @param fn tecplot filename.
#' @param origin simulation start time as character vector or any date-time format
#' @param units simulation time unit. Defaults to "days"
#' @param tz time zone of series.
#'
#' @return xts object with same dimensions as data in "fn"
#' @export
#'
dat2xts <- function(fn, origin, units = "days", tz = "Etc/GMT-1"){
  dat <- read.tecplot(fn)
  index <- as.POSIXct(origin, tz = tz) + as.difftime(dat[,1], units = units)
  xts(dat, order.by = index, tzone = tz)
}


#------------------------------------------------------------------------------#


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
    
    write.table(attr(temp,"Station"), paste0(substring(ii, 1, nchar(ii)-4),".csv"),
                quote = F,row.names = F, col.names = F, sep = ",")
    write.table(temp, paste0(substring(ii, 1, nchar(ii)-4),".csv"),
                quote = F,row.names = F, append = T, sep = ",")
    
    out <- c(out, temp)
  }
  invisible(out)
}

#==============================================================================#

#' Tecplot to elements centers
#' 
#' @description Extracts elements from the *.dat generated by HGS and returns
#'    coordinates of their centers
#'    
#'
#' @param fn_dat character. Filename of Tecplot ASCII file (.dat)
#'
#' @return returns a data frame of x,y,z coordinates of the mesh element centroids.
#' @export
#'
mesh2centers <- function(fn_dat){
  dat <- readLines(con = fn_dat)
  
  ZONE <- grep("(ZONE)",ignore.case = F, x = dat, value = T)[1]

  # find the number of elements
  E <- as.numeric(gsub(".*\\sE=\\s*(\\d+).*","\\1", ZONE, perl = T))

  cell <- read.table(file = fn_dat, nrows = E, skip = line_cell)
  
  # adds element number and removes duplicated columns (for three-sided prisms)
  cell <- as.data.frame(unique(as.matrix(cell), MARGIN=2))
  
  # read X Y Z coordinates
  XYZ <- mesh2nodes(fn_mesh = fn_dat)
  
  # calculates the mean coordinates (centroid) of each of the elements (prisms)
  cell_coords <- data.frame(
    #Element = 1:E,
    X = apply(cell, 1, function(i) mean(XYZ[i,1])),
    Y = apply(cell, 1, function(i) mean(XYZ[i,2])),
    Z = apply(cell, 1, function(i) mean(XYZ[i,3]))
    )

  # return the data invisibely
  invisible(cell_coords)
}

#' read block Tecplot data
#' 
#' @description Extract data from HGS output files in Tecplot (block) ASCII (*.dat) format.
#'    
#'
#' @param dat character. Filename of HGS output file (*.dat)
#' @param out character. Output filename. If NULL no file wil be written.
#' @param elements If True, element data will be exported alongside node data.
#'
#' @return 
#' @export
#'
dat2R <- function(fn_dat){
  data <- readLines(con = fn_dat)
  # Extract title from the file (1 or more lines)
  title <- paste(trimws(stringr::str_extract(data[grep("^(TITLE)", x = data)], 
                                             pattern = '(?<=\").+(?=\")')), 
                 collapse =  "; ")
  # Extract variables (1 line)
  vars <- stringr::str_extract(
    stringr::str_split(trimws(data[grep("^(VARIABLES)", x = data)]),",")[[1]], 
                                             pattern = '(?<=\").+(?=\")')
  
  # extract XYZ coordinates of nodes
  xyz_node <- mesh2nodes(fn_mesh = fn_dat)
  
  # Extract XYZ centroid coordinates of mesh elements
  xyz_elem <- mesh2centers(fn_dat = fn_dat)
  
  # Each output time starts with a line starting with ZONE
  lines_ZONE <- grep("^(ZONE)", x = data)
  
  
  # Split the data to different output times and read data for each time seperately
  for (tt in 1:length(lines_ZONE)){
    #print(tt)
    df <- data[ts[tt]:ifelse(tt < length(lines_ZONE), 
                             lines_ZONE[tt + 1] - 1, 
                             length(data))]
    # find number of nodes
    N <- as.numeric(gsub(".*\\sN=\\s*(\\d+).*","\\1",df[1],perl = T))
    
    # find the number of elements
    E <- as.numeric(gsub(".*\\sE=\\s*(\\d+).*","\\1",df[1],perl = T))

    # find parameter lines starting with #
    pars_ln <- grep("^#", x = df) # line number
    pars = trimws(substr(df[pars_ln], start = 2, stop = nchar(df[pars_ln]))) # parameter names
    pars_elem <- grepl("(cell.centred)", pars) # cell centred parameters
    pars_node <- !pars_elem
    
    # output time
    TIME <- stringr::str_extract(df[1], "(?<=SOLUTIONTIME= )[[:digit:]E\\.\\+\\-]*")
    
    df_pars_node <- NULL
    df_pars_elem <- NULL
    for(pp in 1:(length(pars))){
      #print(pp)
      data_pars <- df[(pars_ln[pp] + 1) :ifelse(pars_elem[pp], 
                                                pars_ln[pp] +  ceiling(E/5),
                                                pars_ln[pp] +  ceiling(N/5))]
      # if parameter is cell centred
      if(pars_elem[pp]){
        x <- as.numeric(unlist(strsplit(trimws(data_pars),split = "\\s+")))
        df_pars_elem <- cbind(df_pars_elem, x)
    
      }else{ #if parameter is node centered
        # if you encounter 
        if(pars[pp] == "element node lists"){
          pars <- pars[-pp]
          pars_ln <- pars_ln[-pp]
          pars_node <- pars_node[-pp]
        }else{
          x <- as.numeric(unlist(strsplit(trimws(data_pars),split = "\\s+")))
          df_pars_node <- cbind(df_pars_node, x)
        }
      }
    } # for pp
    
    colnames(df_pars_elem) <- pars[pars_elem]
    colnames(df_pars_node) <- pars[pars_node]
    
    df_pars_elem <- data.frame(df_pars_elem)
    df_pars_node <- data.frame(df_pars_node)
    
    # Add x y z and node number to node data
    if(tt == 1){
      df_elem <- data.frame(Element = 1:E,
                            Time = TIME,
                            xyz_elem,
                            df_pars_elem)
      df_node <- data.frame(Node = 1:N,
                            Time = TIME,
                            df_pars_node)
    }else{
      # Adding shared variables
      temp <- df_elem
      temp[colnames(df_pars_elem)] <- df_pars_elem
      # adding to end result
      df_elem <- rbind(df_elem, temp)
      
      temp <- df_node
      temp[colnames(df_pars_node)] <- df_pars_node
      df_node <- rbind(df_node, temp)
    }
  }#for tt
  
  # return the data invisibely
  invisible(list(Nodes = df_node, Elements = df_elem))
}
  
#==============================================================================#
df2layer <- function(df, layer = 1, nodes2D = 3106){
    subset(df, Node %in% ((layer - 1) * nodes2D +1):(nodes2D * layer))
}

# Adds layer number to each node to a data frame generated by dat2R
# nodes2d number of nodes per layer
dfAddLayer <- function(df, nodes2D = 3106) {
  if(is.list(df)) try(df <- df$Nodes)
  df$Layer <- ceiling(df$Node / nodes2D)
  df
}


#==============================================================================#
#### Analyzing HGS output data ####
#==============================================================================#

#' Read model run listing file (.lst)
#'
#' @param dir directory of the model, where the .lst file is located
#'
#' @return data frame with 32 columns and so many rows as there are time steps taken by the model
#' @export
#'
read.lst <- function(dir){
  lst <- readLines(con = list.files(path = dir, pattern = "(\\.lst)$", full.names = T))
  
  ln_sol <- grep("(SOLUTION FOR TIMESTEP)", x = lst)
  ln_wb <- grep("(FLUID BALANCE, TIME:)", x = lst)
  
  df_lst <- data.frame() 
  for (ii in 1:length(ln_sol)){
    st <- ln_sol[ii] + 2 # first line of data (Global target time)
    ed <- ln_wb[ii]- 3 # last line of data (multiplier)
    
    # following line numbers could be a vector of multiple values
    gt <- st - 1 + grep("(Global target time)", x = lst[st:ed])  # global target time
    iter <- st - 1 + grep("(Summary of nonlinear iteration)", x = lst[st:ed]) # start of iteration summary
    mp <- st - 1 + grep("(Dt multiplier)", x = lst[st:ed]) # start of multiplier table
    # number of times the time step was cut
    dtcut = length(iter) - 1
    
    # Global target time
    t_target <- as.numeric(stringr::str_extract(lst[tail(gt,1)], "(?<=Global target time:).+(?=\\()"))
    # Time stepping information
    df_tstep <- 
      trimws(lst[tail(iter, 1) - 3]) %>%
      strsplit(., split =  "\\s+",fixed = F) %>%
      unlist() %>%
      .[1:4] %>%
      as.numeric() %>%
      t() %>%
      data.frame() %>%
      `colnames<-` (unlist(strsplit(trimws(lst[tail(gt, 1) + 1]), split =  "\\s+",fixed = F)))
    
    # Summary of nonlinear iteration
    df_iter <- 
      trimws(lst[tail(mp,1) - 2]) %>%
      strsplit(split = "\\s+", fixed = F) %>%
      unlist() %>%
      t() %>% 
      data.frame(stringsAsFactors = F) %>%
      `colnames<-` (unlist(strsplit(trimws(lst[tail(iter,1) + 1]), split =  "\\s+",fixed = F)))
    df_iter[,1:9] <- sapply(df_iter[, 1:9], as.numeric)
    
    # Multiplier table
    df_mp <- 
      # read table lines and split them by white spaces
      strsplit(trimws(lst[tail(mp,1):(ed-1)]), split =  "\\s{2,}",fixed = F) %>%
      unlist() %>%
      matrix(nrow = (ed - tail(mp,1)), byrow = T) %>%
      data.frame(stringsAsFactors = F) %>%
      # unlist and convert to data frame without the second line (just "======")
      `colnames<-` (c("Variable",  "Max.change", "Target.change", "Dt.multiplier", "At.node")) %>%
      .[-2:-1,] %>%
      # make a column out of every combination of Variable and other statistics
      mutate(Time = df_tstep[,2]) %>%
      pivot_longer(cols = c("Max.change", "Target.change", "Dt.multiplier", "At.node"),
                   names_to = "Stat") %>%
      pivot_wider(id_cols = Time, names_from = c(Variable, Stat), values_from = value)
    df_mp[,] <- sapply(df_mp[,], as.numeric)
    # Fluid (water) balance
    #time_wb <- trimws(stringr::str_extract(lst[ln_wb[ii]], "(?<=TIME:).+"))
    
    # filling the dataframe
    df_lst <- rbind(df_lst, data.frame(T_target = t_target, 
                                       dT_cut = dtcut,
                                       df_tstep,
                                       df_iter,
                                       df_mp, stringsAsFactors = F))
    
    
  }
}

#------------------------------------------------------------------------------#

#' Convert absoulute groundwater level to groundwater depth
#'
#' @param fn_dat filename of HGS output file with groundwater levels (.dat)
#' @param fn_mesh filename of HGS mesh file (.dat)
#' 
#' @details Depth to groundwater table is positive in downward direction and 
#'    negative above ground.
#'
#' @return returns a data frame with the same number of rows as in "fn_dat" 
#'    with Time, Depth2GWL, X,Y, Z (ground surface) columns.
#' @export
#'
GWL2depth <- function(fn_dat, fn_mesh){
  nodes <- mesh2nodes(fn_mesh = fn_mesh)
  dat <- read.tecplot(fn_dat)
  
  nodes %>% 
    dplyr::filter(X %in% unique(dat[,c("X","Y")])$X,
                  Y %in% unique(dat[,c("X","Y")])$Y) %>%
    group_by(X,Y) %>% 
    summarise(Z.ground = max(Z)) %>%
    ungroup() %>% 
    right_join(., dat, by = c("X","Y")) %>% 
    transmute(Time,Depth2GWT = Z.ground - H, X, Y, Z = Z.ground) %>%
    as.data.frame()
}
#==============================================================================#
#### Ploting HGS output data ####
#==============================================================================#

#' Plot data from tecplot text file (.dat)
#'
#' @details Simple ploting function to quickly plot data from HGS output files 
#'    (tecplot text format .dat). This works for observation points, surface 
#'    hydrographs and water balance. Time-series are ploted as facets. 
#'    Additional plot tweeking is posible by manipulating the outputed ggplot object.
#'
#' @param fn_dat filename of tecplot file
#' @param subset columns subset given as column numbers or names.
#' @param origin optional origin (start) date of simulation
#' @param tz time zone. Default is UTC+1
#' @param units simulation time units. Default is "days"
#'
#' @return ggplot object
#' @export 
#'
plot.tecplot <- function(fn_dat, subset, origin, tz = "Etc/GMT-1", units =  "days"){
  df <- read.tecplot(fn_dat)
  
  # if subset not supplied use all columns
  if(missing(subset)) subset <- colnames(df)
  #if(is.null(subset)) subset <- colnames(df)
  if(length(subset) < 2) stop("Subset should contain at least two variables!")
  
  # Simulation origin (start) time
  if(!missing(origin)){
    df[,1] <- SimT2T(df[,1], origin = origin, units = units, tz = tz)
  }
  
  df %>%
  select(subset) %>%
  pivot_longer(cols = -1, names_to = "variable", values_to = "value") %>%
  ggplot2::ggplot(.,
                  aes(Time, value, color = variable)) +
  ggplot2::geom_path(show.legend = F) +
  ggplot2::geom_point(show.legend = F) +
  ggplot2::facet_wrap(~variable, ncol=1, scales = "free_y") + 
  ggplot2::labs(x = ifelse(missing(origin),paste0("Time [", units, "]"), "Time"))  
}
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
plot.sim.heads <- function(dir, stations, obs = T, 
                           origin, tz = "Etc/GMT-1", units =  "days"){
  # find observation wells output files in the directory "dir"
  filename <- list.files(path = dir, pattern = "(observation_well)(.+)(\\.dat)$",full.names = T)
  
  if(length(filename) < 1) stop("No observations found!")
  
  # get stations names from file names
  st <- stringr::str_extract(filename, pattern = "(?<=observation_well_flow.)(.+)(?=\\.dat$)")
  if(!missing(stations)) {
    # keep only files for selected stations 
    filename <- filename[st %in% stations]
    # keep only station names in "stations"
    st <- st[st %in% stations]
  }
  
  # read hydraulic head data from files
  data <- lapply(1:length(st), 
                 function(x){
                   temp <- read.tecplot(filename[x])[,c("Time","H")]
                   colnames(temp) <- c("Time", st[x])
                   temp
                 })
  # merges and melts data 
  df <- reduce(.x = data, .f = inner_join, by = "Time") %>%
    pivot_longer(cols = -1, names_to = "variable", values_to = "value") %>%
    mutate(type = "simulated")
  
  # convert simulation time to real time
  if(!missing(origin)){
    df$Time <- SimT2T(df$Time, origin = origin, units = units, tz = tz)
  }
  
  # base plot
  g1 <-
    ggplot2::ggplot(mapping = aes(Time, value, color = variable, 
                                  linetype = type))+
    ggplot2::geom_path(data = df) +
    ggplot2::facet_wrap(~variable, ncol=1, scales = "free_y",strip.position = "right") + 
    ggplot2::labs(title = "Simulated heads", 
                  x = ifelse(missing(origin),paste0("Time [", units, "]"), "Time"),
                  y = "Groundwater table [m a.s.l.]",
                  linetype = NULL) +
    scale_linetype_manual(values = c("simulated"="solid", "observed" = "dashed")) +
    guides(color = F) +# remove color from legend
    theme_bw() +
    theme(legend.position = "top")
  
  
  # add observation data
  if(obs == T){
    # if real time is not known we can only add mean values
    if(missing(origin)){
      # get mean, min and max values for stations 
      df_obs <- 
        fortify.zoo(HOAL.data::GWL[,colnames(HOAL.data::GWL) %in% st]) %>%
        pivot_longer(cols = -1, names_to = "variable", values_to = "value") %>%
        group_by(variable) %>%
        summarise(mean = mean(value, na.rm = T),
                  min = min (value, na.rm = T),
                  max = max(value, na.rm = T),
                  type = "observed")
      
      # add a line for mean value and a color band between min and max
      g1 <- g1 +
        geom_hline(aes(yintercept = mean), data = df_obs, color = "black") +
        geom_hline(aes(yintercept = min), data = df_obs, linetype = "dashed") +
        geom_hline(aes(yintercept = max), data = df_obs, linetype = "dashed") 
    }else{
      # get observed heads for selected stations and for the period of simulation
      df_obs <-
        window(HOAL.data::GWL[,colnames(HOAL.data::GWL) %in% st],
               start = min(df$Time, na.rm = T),
               end = max(df$Time, na.rm = T)) %>%
        to_timestep(by= units) %>%
        fortify.zoo(.,
                    names = c("Time", "variable", "value")
                    ) %>%
        pivot_longer(cols = -1, names_to = "variable", values_to = "value") %>%
        mutate(type = "observed")
      
      # add observed time series
      g1 <-
        g1 +
        geom_path(data = df_obs)
    }
  }

  return(g1)
}

#------------------------------------------------------------------------------#

plot.sim.point <- function(dir, obs.point, 
                           start = as.POSIXct("2013-01-01",tz="etc/GMT-1"),
                           station = obs.point){
  
  filename <- list.files(path = dir, 
                         pattern = paste0("(observation_well)(.+)(", obs.point ,".dat)$"),
                         full.names = T)
  if(length(filename) != 1) stop(paste("Files found", length(filename), ", expected = 1."))
  
  x <- read.tecplot(filename)
  x.xts <- 
    to_timestep(xts(x[,-1], 
                    order.by = start + x$Time*86400),
                "days")
  obs <- to_timestep(HOAL.data::GWL[paste0(as.character(start),"/"),station], "days")
  sim <- x.xts$H
  colnames(obs) <- paste0(obs.point, ".obs")
  colnames(sim) <- paste0(obs.point, ".sim")
  dygraphs::dygraph(merge(obs, sim))
}

#==============================================================================#



##----------------------------------------------------------------------------##
#### PEST ####
##============================================================================##

#' Read HGS output file and convert it to PEST sample file (smp)
#'
#' @param file_in HGS output filename
#' @param file_out filename for PEST sample file (smp)
#' @param var_name variable name in HGS output file to be read.
#' @param start_time simulation start time (simulation time = 0)
#' @param units time units ("days", "seconds")
#' @param append append to an existing smp file. Default is FALSE.
#' @param by optional. Aggregation time step (e.g. "days", "5 hours" - see HOAL::to_timestep)
#' @param FUN optional. aggregation function.
#' @param subset time subset of read data (e.g. "2017-01-01/2017-06-01")
#' @param fn_mesh filename of model mesh file if depth to groundwater table should be read. If not NULL.
#'
#' @return one or more PEST sample files (smp).
#' @export
#'
hgs2smp <- function(file_in, file_out, var_name, start_time, 
                    subset = NULL, fn_mesh = NULL,
                    units = c("days", "seconds"), append = F, 
                    by = NULL,
                    FUN = c("mean", "sum", "first", "last")
                    ){
  # args cleanup
  units <- match.arg(units)
  FUN <- match.arg(FUN)
  
  
  l <- list()
  for(ii in 1:length(file_in)){
      dat <- read.tecplot(file_in[ii])
      Station = substr(attr(dat, "Station"), 1,10)
      
      # convert from hydraulic head to depth to GW table
      if(!is.null(fn_mesh)){
        nodes <- mesh2nodes(fn_mesh = fn_mesh) %>%
          mutate(X = round(X, 3),
                 Y = round(Y, 3))
        dat <- dat %>%
          mutate(X = round(X, 3),
                 Y = round(Y, 3))
        dat <-
          nodes %>% 
          dplyr::filter(X %in% unique(dat[,c("X","Y")])$X,
                        Y %in% unique(dat[,c("X","Y")])$Y) %>%
          group_by(X,Y) %>% 
          summarise(Z.ground = max(Z), .groups = "drop") %>%
          ungroup() %>%
          right_join(., dat, by = c("X","Y")) %>% 
          transmute(Time,Depth2GWT = Z.ground - H)
      # read selected variable normally
      }else{
      dat <- dat[,c("Time", var_name)]
      }
      
      # convert to xts obejct 
      x <- xts(dat[,-1], 
               order.by = SimT2T(dat$Time, origin = start_time),
               tzone = "Etc/GMT-1")
    
      
    # Optional interpolation to fixed time-steps
    if(!is.null(by)){
      x <- to_timestep(x, by = by, FUN = FUN, maxgap = Inf)
    }
    
    # Optional time subset
    if(is.character(subset)){
        try({
          x <- x[subset,]
        })
    }
    
    x <- x[complete.cases(x[,1]),1]
    
    l[[ii]] <- data.frame(Station = format(Station, width = 10),
                          Date = format(zoo::index(x), format = "%d/%m/%Y"),
                          Time = format(zoo::index(x), format = "%H:%M:%S"),
                          value = format(coredata(x[,1]), digits = 6, scientific = T),
                          stringsAsFactors = F)
    
  }
  write.table(do.call(rbind, l), file = file_out, append = append, quote = F, sep = "\t",
              row.names = F, col.names = F)
  
  invisible(do.call(rbind, l))
}

#' Mesh to node coordinates
#' 
#' @description Extracts nodes coordinates from the mesh *.dat generated by HGS 
#'    and writes it to a file
#'
#' @param fn_mesh character. Filename of HGS mesh file (*.dat)
#' @param out_nodes character. Output filename. If NULL no file wil be written.
#' @param zone If True, zone information will be included in last column.
#'
#' @return Writes a table of node number, x, y and z coordinate to file.
#' @export
#'
mesh2nodes <- function(fn_mesh, out_nodes = NULL){
  dat = readLines(fn_mesh)
  
  ZONE <- grep("(ZONE)",ignore.case = F, x = dat, value = T)[1]
  # find number of nodes
  N <- as.numeric(gsub(".*\\sN=\\s*(\\d+).*","\\1", ZONE, perl = T))
  
  # find the number of elements
  E <- as.numeric(gsub(".*\\sE=\\s*(\\d+).*","\\1", ZONE, perl = T))
  
  # line numbers
  line_x <- grep("^(#\\sx)", x = dat)[1]
  line_y <- grep("^(#\\sy)", x = dat)[1]
  line_z <- grep("^(#\\sz)", x = dat)[1]

  df <- data.frame(
    X = as.numeric(unlist(strsplit(trimws(dat[(line_x + 1):(line_x + ceiling(N/5))]),split = "\\s+"))),
    Y = as.numeric(unlist(strsplit(trimws(dat[(line_y + 1):(line_y + ceiling(N/5))]),split = "\\s+"))),
    Z = as.numeric(unlist(strsplit(trimws(dat[(line_z + 1):(line_z + ceiling(N/5))]),split = "\\s+")))
    )
  
  if(!is.null(out_nodes)){
    write.table(df, out_nodes, row.names = F, col.names = F, sep = "\t", quote = F)  
  }
  
  # return the data invisibely
  invisible(df)
}

##----------------------------------------------------------------------------##

#' Mesh to elements indices
#' 
#' @description Extracts elements from the mesh *.dat generated by HGS 
#'    and writes it to a file
#'
#' @param fn_mesh character. Filename of HGS mesh file (*.dat)
#' @param out_elements character. Output filename.If NULL, no file will be written.
#'
#' @return Writes a table of elements and node number of their vertices (6 for 
#' triangular and 8 for rectangular prisms).
#' @export
#'
mesh2elements <- function(fn_mesh = "mesh.dat", out_elements = "element_ind.txt"){
  header <- readLines(con = fn_mesh, n = 3)[3]
  # find number of nodes
  n <- as.numeric(gsub(".*N=\\s*(\\d+).*","\\1",header,perl = T))
  
  # find the number of elements
  m <- as.numeric(gsub(".*E=\\s*(\\d+).*","\\1",header,perl = T))
    
    
  dt <- read.table(file = fn_mesh, nrows = m, skip = 3 + 4+ 3*ceiling (n/5) + ceiling(m/5))
  
  # adds element number and removes duplicated columns (for three-sided prisms)
  dt2 <- data.frame(Node = 1:m, as.data.frame(unique(as.matrix(dt), MARGIN=2)))
  colnames(dt2) <- c("Element", paste0("V",1:(ncol(dt2)-1)))
  # add zone information
  zn <- as.vector(
      t(stringr::str_split(readLines(fn_mesh)[(8+3*ceiling(n/5)):(7+3*ceiling(n/5)+ceiling(m/5))],
                           pattern = "\\s+", simplify = T)
        )
      )
    
    dt2$Zone <- as.numeric(zn[zn != ""])
  
  if(!is.null(out_elements)){
    write.table(dt2, out_elements, row.names = F, col.names = F, sep = "\t")  
  }
  
  
  # return the data invisibely
  invisible(dt2)
}

##----------------------------------------------------------------------------##
#' Converts HGS coordinates to ppk2facg format
#'
#' @describe Read coordinates from HGS and can be used in ppk2facg
#' 
#' @param wd character. Working directory
#' @param fn_nodes character. xyz coordinates HGS file
#' @param fn_elements character. nodes of rectangle mesh element filename
#' @param fn_el_center character. Output filename of centroids
#' @param fn_xyzone character. Output filename of xyz coordinates.
#' @param zones Should zone information from mesh file be used? 
#'              Otherwise all elements are in zone 1.
#'
#' @return four ASCII files with names used in ppk2facg
#' @export
#'
mesh2cord <- function(in_mesh,
                      out_nodes = "nodes_cord.txt", 
                      out_elements = "element_ind.txt", 
                      out_el_center = "element_center.txt",
                      out_xyzone = "xyzone.txt",
                      zones = T){
  
  # Convert hgs mesh to elements
  elements_ind <- mesh2elements(fn_mesh = in_mesh, out_elements = out_elements)
  
  # Convert hgs mesh to node x y z
  nodes_cor <- mesh2nodes(fn_mesh = in_mesh, out_nodes = out_nodes)
  
  
  
  # number of vertices of elements
  n_vert <- ncol(elements_ind)-1
  colnames(elements_ind)<-c("Element", paste0("Node",1:n_vert))
  
  # calculates the mean coordinates (centroid) of each of the elements (prisms)
  element_center <- 
    matrix(ncol = 3, byrow = T,
           data = unlist(lapply(elements_ind[,1], 
                                function(x) colMeans(nodes_cor[as.numeric(elements_ind[x,c(-1,-ncol(elements_ind))]),]))
           )
    )
  
  if(zones){
    el_zone <- elements_ind[,ncol(elements_ind)]
  }else{
    el_zone = 1  
  }
  
  
  
  XYandZone <- cbind(element_center[,1:2],el_zone)
  
  #write new output file for ppk2facg
  write.table(element_center,out_el_center,row.names=FALSE,col.names=FALSE, quote = F, sep = "\t")
  write.table(XYandZone, out_xyzone,row.names=FALSE,col.names=FALSE, quote = F, sep = "\t")
}

##----------------------------------------------------------------------------##

#' Conductivity field for HydroGeoSphere
#' 
#' @description Read hydraulic conductivity field from fac2g and tranfer it to a readable HydroGeoSphere input
#'
#' @param wd character. Working directory.
#' @param file.in character. Name of input file = output file from ppk2facg
#' @param file.out character. Name of output file to be read by HGS
#'
#' @return Saves files
#' @export
#'
#' @author Christian Moeck
k2HGS <- function(file.in = "k_interpolated.txt", file.out = "k_calibrated.txt"){
  # read output from ppk2facg
  x <- scan(file.in)
  elements <- seq(1:length(x))
  y <- cbind(elements,x,x,x)
  # write new hydraulic conductivity field for HGS
  write.table(y, file.out, row.names=FALSE, col.names=FALSE, quote = F, sep = "\t")
}


##----------------------------------------------------------------------------##

xts2smp <- function(x, fn_out, verbose = T){
  x.mlt <- 
    zoo::fortify.zoo(x) %>%
    tidyr::pivot_longer(x.mlt, cols = -1, 
                               names_to = "Station", 
                               values_drop_na = T) %>%
    dplyr::arrange(Station, Index) %>%
    dplyr::transmute(Station = Station,
                     Date = format(Index, format = "%d/%m/%Y"),
                     Time = format(Index, format = "%H:%M:%S"),
                     value = value)

  write.table(x.mlt, file = fn_out, sep = "\t", row.names = F, 
              col.names = F, quote = F, )
  
  if(verbose)print(paste("Observed heads written to",fn_out))
}

##----------------------------------------------------------------------------##

read.smp <- function(file, out = "data.frame"){
  df <- 
    read.table(file) %>% 
    transmute(Date = as.POSIXct(paste(V2,V3), format = "%d/%m/%Y %H:%M:%S"), 
              Series = V1, 
              Value = V4)
  
  if(out %in% c("xts")){
    df <- pivot_wider(df, id_cols = Date, names_from = Series, values_from = Value)
    xts(df[,-1], order.by = df$Date, tzone = "Etc/GMT-1")
  } else{
    df
  }
}

##----------------------------------------------------------------------------##

#' Locate Startpoints by time
#'
#' @param x an xts object
#' @param on the periods startpoints to find as a character string
#' @param k along every k-tk element - see notes
#'
#' @details 
#'   \code{startpoints} returns a numeric vector corresponding to the first observation in each period specified by "on", with a 1 added to the begining of the vector.
#'   Valid values for argument "on" include: “us” (microseconds), “microseconds”, “ms” (milliseconds), “milliseconds”, “secs” (seconds), “seconds”, “mins” (minutes), “minutes”, “hours”, “days”, “weeks”, “months”, “quarters”, and “years”.
#' @return
#'   A numeric vector of startpoints beginging with 1.
#' @export
#'
#' @examples
startpoints <- function (x, on = "months", k = 1) {
  head(endpoints(x, on, k) + 1, -1)
}

##----------------------------------------------------------------------------##
#' Convert between real and simulation time
#'
#' @param x time to convert. A time based object or a numeric.
#' @param origin 
#' @param units 
#' @param tz 
#'
#' @return
#' @export
#'
#' @examples
T2SimT <- function(x, origin, units = "days", tz = "Etc/GMT-1"){
  # format all dates to the same format
  x <- sapply(x, function(x) format(as.POSIXct(x, tz = tz), format = "%Y-%m-%d %H:%M:%S"), USE.NAMES = F)
  origin <- format(as.POSIXct(origin, tz = tz), format = "%Y-%m-%d %H:%M:%S")
  # convert characters to POSIXct
  x <- as.POSIXct(x, tz = tz)
  origin <- as.POSIXct(origin, tz = tz)
  # subtract the origin from input
  as.double.difftime(x - origin,
                     units = units, 
                     tz = tz)
}
SimT2T <- function(x, origin, units = "days", tz = "Etc/GMT-1"){
  as.POSIXct(origin, tz = tz) + 
    lubridate::as.difftime(x, units = units)
}


#==============================================================================#

obs2smp <- function(dir = getwd(), var = c("head", "GWL", "depth", "Q", "discharge"), file, 
                    st,  subset, by, FUN = c("mean", "sum", "first", "last"), 
                    units = c("days", "seconds")){
  
  # args cleanup
  units <- match.arg(units)
  FUN <- match.arg(FUN)
  
  # select data source by the variable (var) required
  # datasets could easily be replaced by a file or other repository
  data <- switch(EXPR = var,
                 head = HOAL.data::GWL[subset, colnames(HOAL.data::GWL) %in% st],
                 GWL = HOAL.data::GWL[subset, colnames(HOAL.data::GWL) %in% st],
                 # sign of groundwater depth needs to be reversed
                 depth = - HOAL.data::GWdepth[subset, colnames(HOAL.data::GWdepth) %in% st],
                 # discharge needs to be converted from l/s to m3/units
                 Q = HOAL.data::HOAL.Discharge[subset, colnames(HOAL.data::HOAL.Discharge) %in% st] / 
                   1000 * as.double(period(1, units = units), units = "seconds"),
                 discharge = HOAL.data::HOAL.Discharge[subset, colnames(HOAL.data::HOAL.Discharge) %in% st] / 
                   1000 * as.double(period(1, units = units), units = units)
                )
  
  if(!all(st %in% colnames(data))) {
    warning(paste0("Stations: ", st[!st %in% colnames(data)], 
                   " were not found in observed database!"))
    }
  
  # subset the data by time and by station
  df <-
    data %>%
    to_timestep(by = by, FUN = FUN) %>%
    fortify.zoo(melt = T, names = c("Index", "variable", "value"),  ) %>%
    # # new time floored to the time step given by "by"
    # mutate(date = lubridate::floor_date(Index, unit = by)) %>%
    # group_by(variable, date) %>%
    # # apply aggregating function
    # summarize(value = switch(FUN,
    #                          "mean" = mean(value, na.rm =T),
    #                          "sum" = sum(value, na.rm = T),
    #                          "first" = dplyr::first(value, order_by = Index),
    #                          "last" = dplyr::last(value, order_by = Index)),
    #           .groups = "drop") %>%
    filter(!is.na(value)) %$%
    # format for PEST sample file (.smp)
    data.frame(Station = format(variable, width = 10),
               Date = format(Index, format = "%d/%m/%Y"),
               Time = format(Index, format = "%H:%M:%S"),
               value = format(value, digits = 6, scientific = T),
               stringsAsFactors = F) %>%
    # save to file
    write.table(., file = file, quote = F, sep = "\t", row.names = F, col.names = F)
  
  print(paste("Observations written to:", file))
}

#==============================================================================#

#' Converts HGS outputs to a PEST sample file (smp)
#'
#' @details This function is usefull when you want to convert the simulated data 
#'   from HGS into PEST sample file to compare it with the observed data. 
#'   This is a wrapper function for the hgs2smp and allows to convert all outputs 
#'   in a directory at the same time.Currently only head (absolute GWL), depth to
#'   the GW table and discharge are implemented. Other outputs could be read by 
#'   hgs2smp.
#'   
#' @param dir directory where HGS output files (.dat) are located.
#' @param var name of the variable to read. Accepts "head", "GWL", "depth", "Q", "discharge"
#' @param st character vector of station names to include
#' @param file name for the output file
#' @param date_start start time of simulation, i.e. when simulation time is 0
#' @param subset_date time period to include (e.g. "2017/2018-01-05")
#' @param fn_mesh optional filename of the HGS mesh file (produced by mesh to tecplot command in GROK). Only used when var = "depth"
#' @param ... further interpolation arguments to hgs2smp (FUN, by). Defaults are no interpolation.
#'
#' @return writes a smp file
#' @export
#'
sim2smp <- function(dir = getwd(), var = c("head", "GWL", "depth", "Q", "discharge"),
                    st, file, date_start, subset_date, fn_mesh = NULL, ...){
  
  # find output files
  if(var == "head" | var == "GWL"){
    files <- list.files(path = dir, pattern = "(observation_well_flow)(.+)(\\.dat)$",full.names = T)
    # keep only files of selected stations
    files <- files[str_extract(files,pattern = "(?<=well_flow.)([:alnum:]+)") %in% st]
    if(length(files) < 1) stop("No modeled heads found!")
    
    # Convert head data to Bore sample file
    hgs2smp(file_in = files, 
            file_out = file,
            var_name = "H",
            start_time = date_start,
            subset = subset_date,
            ...)
    paste("Simulated heads written to",file)
    
  }else if(var == "Q" | var == "discharge"){
    files <- list.files(path = dir, pattern = "(hydrograph)(.+)(\\.dat)$", full.names = T)
    # keep only files of selected stations
    files <- files[str_extract(files, pattern = "(?<=hydrograph\\.)(.+)(?=\\.dat)") %in% st]
    if(length(files) < 1) stop("No modeled discharges found!")
    
    # Convert head data to Bore sample file
    hgs2smp(file_in = files, 
            file_out = file,
            var_name = "Surface",
            start_time = date_start,
            subset = subset_date,
            ...)
    paste("Simulated discharge written to",file)
    
  }else if(var == "depth"){
    if(is.null(fn_mesh)) stop("No mesh filename ('fn_mesh') provided!")
    files <- list.files(path = dir, pattern = "(observation_well_flow)(.+)(\\.dat)$",full.names = T)
    # keep only files of selected stations
    files <- files[str_extract(files,pattern = "(?<=well_flow.)([:alnum:]+)") %in% st]
    if(length(files) < 1) stop("No modeled heads found!")
    
    # Convert head data to Bore sample file
    hgs2smp(file_in = files, 
            file_out = file,
            var_name = "H",
            start_time = date_start,
            subset = subset_date,
            fn_mesh = fn_mesh,
            ...)
    
    paste("Simulated depth written to",file)
  }else{
    stop(paste("Unknown variable name:", var))
  }

  
}
