#Examine LST file

library(tidyverse)
library(HOAL)
library(magrittr)

dir <- "D:/PhD/HOAL/modelling/HGS_model/HOAL_PEST_PP_3zn_run"

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

# Time step
ggplot(df_lst,
       aes(Time, log10(delta_t))) +
  geom_path()

# Max change
ggplot(df_lst,
       aes(Time)) +
  geom_path(aes(y = Head_Max.change, color = "Head")) +
  geom_path(aes(y = 100*Water.depth_Max.change, color = "Water Depth")) +
  geom_path(aes(y = 100*Saturation_Max.change, color = "Saturation")) 

# cutting timesteps
ggplot(df_lst,
       aes(Time, dT_cut))+
  geom_path()

df_wb <- read.tecplot(list.files(path = dir, pattern = "(water_balance.dat)", full.names = T))

left_join(df_lst, df_wb) %>%
  pivot_longer(cols = c(`outflow-boundary`,ET_PET, ET_AET, rain, dT_cut, delta_t)) %>%
  ggplot(., aes(Time, value, color = name)) +
  geom_path()+ 
  facet_wrap(name~., scales = "free_y", ncol = 1)

# water balance error
left_join(df_lst, df_wb) %>%
  ggplot(., aes(Time)) +
  geom_path(aes(y =`Error percent`))+
  geom_path(aes(y = -ET_AET/1000), color = "red") +
  geom_path(aes(y = rain/1000), color = "blue")

# comparison to observed
MW.d <- (to_timestep(HOAL.data::HOAL.Discharge[, "MW"],"days"))
fortify.zoo(MW.d["2016/2018"]) %>%
  mutate(Time = as.double(Index - as.POSIXct("1950-01-01", tz = "Etc/GMT-1"), unit = "days"),
         MW = MW/1000*86400) %>%
  full_join(.,df_wb) %T>% View %>%
  ggplot(., aes(Time)) +
  geom_path(aes(y=-`outflow-boundary`, color = "Sim")) +
  geom_path(aes(y = MW, color = "Obs"))

MW.d <- (to_timestep(HOAL.data::HOAL.Discharge[, "MW"],"days"))

fortify.zoo(MW.d["2013/2018"]) %>% 
  mutate(Time = as.double(Index - as.POSIXct("2013-01-01", tz = "Etc/GMT-1"), unit = "days"),
         MW = MW/1000*86400) %>% 
  full_join(.,wb_mp) %>%
  ggplot(., aes(Time)) + 
  geom_path(aes(y=-`outflow-boundary`, color = "Sim")) +
  geom_path(aes(y = MW, color = "Obs"))

fortify.zoo(MW.d["2013"]) %>% 
  mutate(Time = as.double(Index - as.POSIXct("2013-01-01", tz = "Etc/GMT-1"), unit = "days"),
         MW = MW/1000*86400) %>% 
  full_join(.,wb_mp) %$%
  hydroGOF::gof(sim =  -`outflow-boundary`, obs =  MW, na.rm=T)
  
