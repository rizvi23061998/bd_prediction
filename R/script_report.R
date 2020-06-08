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

source("model_functions.R")
source("load_data.R")


forecast_window <- 90
smc_recovery <- (1/14.04)
theta <- c(
  r0 = 2.5,
  population = total_population,
  init_cases = data$cases[1],
  betavol=.11,
  report = 0.2419192,#6.1,
  recover = smc_recovery #smc recovery rate
  # recover = (1/11.59) #ls recovery rate
  
)
theta_initNames <- c(names(theta),"sus","inf","rem","beta",
                     "cases","waiting","reports")
theta[["beta"]] <- theta[["r0"]] * theta[['recover']]

data$case_data_conf <- data$cases
data$case_data_recovered <- data$cases
data$case_data_death <- data$cases

scaled_beta = 1
id="main"
beta_id = c("no_red","10_red","20_red","30_red")
gamma_id = c("no_inc","10_inc","20_inc","30_inc")


source("model_reported.R")
source("plotter_report.R")


# sl <- MLE_check_2D_r(p1_name = "report",p2_name = "recover",
#                      theta_tab1 = seq(1/20,1,le=50),
#                      theta_tab2 = seq(1/3,1/20,le=100),
#                      nn = 2e3)


# sl <- MLE_check_r(p_name = "report",theta_tab = seq(1/20,1,le=100))

# run_fits_r(100,2e3,0,.25,1)
# plot_outputs_r(1)

type = "beta_90"
# type = "beta_365"
scaled_betas = c(1, 0.9, 0.8, 0.7)
scaled_gammas <- c(1,1.1,1.2,1.3) 


for(ii in 1:length(scaled_betas)){
  id = paste(beta_id[ii],type,sep = "_")
  print(id)
  scaled_beta = scaled_betas[ii]
  # theta[["recover"]] <- smc_recovery * scaled_gammas[ii]
  smc_model_rf(theta,2e3,1)
  plot_outputs_rf(1)
}



