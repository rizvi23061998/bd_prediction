library(plyr)
library(xlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(xlsx)
library(doMC)
library(mgcv)
library(coda)
library(ggplot2)
registerDoMC(4)  #change the 2 to your number of CPU cores

rm(list=ls(all=TRUE))

source("R/model_functions.R")
source("R/load_data.R")


forecast_window <- 360
smc_recovery <- (1/14.04)
theta <- c(
  r0 = 2.5,
  population = total_population,
  init_cases = data$cases[1],
  betavol=.11,
  recover = smc_recovery #smc recovery rate
  # recover = (1/11.59) #ls recovery rate
  
)
theta_initNames <- c(names(theta),"sus","inf","rem","beta","cases")
theta[["beta"]] <- theta[["r0"]] * theta[['recover']]

data$case_data_conf <- data$cases
data$case_data_recovered <- data$cases
data$case_data_death <- data$cases

rec_type = "beta_seg_intel_2"
id = "test"
scaled_beta = 1

ids = c("no_red","10_red","20_red","30_red")
ids2 = c("no_inc","10_inc","20_inc","30_inc")
scaled_betas = c(1, 0.9, 0.8, 0.7)
scaled_gammas <- c(1,1.1,1.2,1.3) 

source("R/model_smc.R")
source("R/smc_plotter.R")
# source("R/smc_driver.R")
source("R/forecaster.R")

# lockdown_period <- 60
non_lockdown_scale <- 2
# lockdown_cycle <- 360/(lockdown_period+30)

relax_dates <- c(159,201,277,443)

for(ii in 1:length(ids)){
  id <- paste(ids[ii],"_",rec_type,sep = "")
  scaled_beta <- rep(scaled_betas[ii],forecast_window)
  ll <- relax_dates[ii] + 15
  if(ll < forecast_window){
    scaled_beta[(ll+1):(ll+8)] <- seq(scaled_beta[ii],non_lockdown_scale,le=8)
    scaled_beta[(ll+9):(ll+30)] <- non_lockdown_scale
    
  }
  
  # theta[["recover"]] <- smc_recovery * scaled_gammas[ii]
  print(id)
  # print(scaled_beta)
  smc_model_forecast(theta,2e3,.25)
  plot_outputs_forecast(1)
  
}


