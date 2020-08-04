################################################################################
####
####          HGS model performance criteria
####
################################################################################

## Author: Lovrenc Pavlin
## Date: 5.5.2020

#### 0. Libraries ####
# scan for additional arguments
# Expecting 3 arguments: 
#   -fn_sim model output (simulated data)
#   -fn_obs observed data
#   -fn_out output file prefix 
args = commandArgs(trailingOnly = T)
args
# test if there are all required arguments
if(length(args) == 0) {
  stop("no arguments provided", call. = F)
} else if (length(args) != 3){
  stop("invalid number of arguments provided", call. = F)
} else{
  fn_sim <- args[1]
  fn_obs <- args[2]
  fn_out <- args[3]
}

#### 1. Functions ####
## Root mean square error
# Smaller is better
RMSE <- function(sim, obs){
  if(length(sim) != length(obs)) stop ('"sim" and "obs" must be of same length!'GWL)
  n = length(sim)
  sqrt(sum((sim - obs)^2, na.rm = T)/n)
}

## Nash-Sutcliffe efficiency
# (-Inf, 1], 1 is perfect
NSE <- function(sim, obs){
  if(length(sim) != length(obs)) stop ('"sim" and "obs" must be of same length!')
  1 - sum((obs - sim)^2, na.rm=T) / sum((obs - mean(obs))^2, na.rm=T)
}

## Percent of bias
# An optimal value would be 0% while a 
# positive value indicates an underestimation and 
# a negative value an overestimation
PBIAS <- function(sim, obs){
  if(length(sim) != length(obs)) stop ('"sim" and "obs" must be of same length!')
  sum(obs - sim, na.rm = T) * 100 / sum(obs, na.rm = T)
}

## kling-Gupta efficiency
KGE <- function(sim, obs){
  if(length(sim) != length(obs)) stop ('"sim" and "obs" must be of same length!')
  
  # means
  m_sim <- mean(sim, na.rm = T)
  m_obs <- mean(obs, na.rm = T)
  # Standard deviation
  sd_sim <- sd(sim, na.rm = T)
  sd_obs <- sd(obs, na.rm = T)
  # correlation (Pearson)
  r <- cor(sim, obs)
  
  # bias
  B <- m_sim / m_obs
  #variability ration
  G <- (sd_sim / m_sim) / (sd_obs / m_obs)
  
  sqrt((r - 1)^2 + (B - 1)^2 + (G - 1)^2)
}

## Coefficient of determination
R2 <- function(sim, obs){
  if(length(sim) != length(obs)) stop ('"sim" and "obs" must be of same length!')
  
  m_obs <- mean(obs, na.rm = T)
  # total sum of squares
  SStot <- sum((obs- m_obs)^2, na.rm = T)
  # regression sm of squares / explained sum of squares
  #SSreg <- sum((sim - m_obs)^2, na.rm = T)
  # sum of squares of residuals / residual sum of squares
  SSres <- sum((obs - sim)^2, na.rm = T)
  
  1 - SSres / SStot
}

#### 2. calculation ####
df_obs <- read.table(fn_obs, col.names = c("Station","Date","Time", "obs"),stringsAsFactors = F)
df_sim <- read.table(fn_sim, col.names = c("Station","Date","Time", "sim"),stringsAsFactors = F)
df <- dplyr::inner_join(df_sim,df_obs)

perf_all <- data.frame(Time = Sys.time(),
                       Station = "all",
                       RMSE = RMSE(df$sim, df$obs),
                       NSE = NSE(df$sim, df$obs),
                       PBIAS = PBIAS(df$sim, df$obs),
                       R2 = R2(df$sim, df$obs),
                       cor = cor(df$sim, df$obs),
                       KGE = KGE(df$sim, df$obs))

perf_st <- data.frame(Time = Sys.time(),
                       Station = unique(df$Station),
                       RMSE = sapply(unique(df$Station),
                                     function(x){RMSE(subset(df, Station == x)$sim, 
                                                      subset(df, Station == x)$obs)}),
                       NSE = sapply(unique(df$Station),
                                    function(x){NSE(subset(df, Station == x)$sim, 
                                                     subset(df, Station == x)$obs)}),
                       PBIAS = sapply(unique(df$Station),
                                      function(x){PBIAS(subset(df, Station == x)$sim, 
                                                       subset(df, Station == x)$obs)}),
                       R2 = sapply(unique(df$Station),
                                   function(x){R2(subset(df, Station == x)$sim, 
                                                    subset(df, Station == x)$obs)}),
                       cor = sapply(unique(df$Station),
                                    function(x){cor(subset(df, Station == x)$sim, 
                                                     subset(df, Station == x)$obs)}),
                       KGE = sapply(unique(df$Station),
                                    function(x){KGE(subset(df, Station == x)$sim, 
                                                     subset(df, Station == x)$obs)}))

perf_date <- data.frame(Time = Sys.time(),
                      Date = unique(df$Date),
                      RMSE = sapply(unique(df$Date),
                                    function(x){RMSE(subset(df, Date == x)$sim, 
                                                     subset(df, Date == x)$obs)}),
                      NSE = sapply(unique(df$Date),
                                   function(x){NSE(subset(df, Date == x)$sim, 
                                                   subset(df, Date == x)$obs)}),
                      PBIAS = sapply(unique(df$Date),
                                     function(x){PBIAS(subset(df, Date == x)$sim, 
                                                       subset(df, Date == x)$obs)}),
                      R2 = sapply(unique(df$Date),
                                  function(x){R2(subset(df, Date == x)$sim, 
                                                 subset(df, Date == x)$obs)}),
                      cor = sapply(unique(df$Date),
                                   function(x){cor(subset(df, Date == x)$sim, 
                                                   subset(df, Date == x)$obs)}),
                      KGE = sapply(unique(df$Date),
                                   function(x){KGE(subset(df, Date == x)$sim, 
                                                   subset(df, Date == x)$obs)}))

write.table(perf_all, paste0(fn_out,"_performance_all.csv"),row.names = F, quote = F, append = T, sep = ",\t")
write.table(perf_st, paste0(fn_out,"_performance_by_station.csv"),row.names = F, quote = F, append = T, sep = ",\t")
write.table(perf_date, paste0(fn_out,"_performance_by_date.csv"),row.names = F, quote = F, append = T, sep = ",\t")
