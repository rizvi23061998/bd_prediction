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


forecast_window <- 365
# smc_recovery <- (1/12.28542)#(1/14.04)
smc_recovery <- (1/9.421094)
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

# rec_type = "beta_365"
# rec_type = "gamma_365"
rec_type="both"

id = "test"
scaled_beta = 1#rep(1,forecast_window)

ids = c("no_red","10_red","20_red","30_red")
ids2 = c("no_inc","10_inc","20_inc","30_inc")
scaled_betas = c(1, 0.9, 0.8, 0.7)
scaled_gammas <- c(1,1.1,1.2,1.3) 

source("R/model_smc.R")
source("R/smc_plotter.R")
# source("R/smc_driver.R")
source("R/forecaster.R")

# for(ii in 1:length(ids)){
#   id <- paste(ids[ii],"_",rec_type,sep = "")
#   # id <- paste(ids2[ii],"_",rec_type,sep = "")
#   scaled_beta <- rep(scaled_betas[ii],forecast_window)
#   # scaled_beta <- rep(1,forecast_window)
# 
#   # theta[["recover"]] <- smc_recovery * scaled_gammas[ii]
#   print(id)
#   # print(scaled_beta)
#   smc_model_forecast(theta,2e3,.25)
#   plot_outputs_forecast(1)
# 
# }


for(ii in 2:3){
  for (jj  in 2:3)
  {
    id <- paste(ids[ii],"_",ids2[jj],"_",rec_type,sep = "")
    scaled_beta <- rep(scaled_betas[ii],forecast_window)

    theta[["recover"]] <- smc_recovery * scaled_gammas[jj]
    print(id)
    # print(scaled_beta)
    smc_model_forecast(theta,2e3,.25)
    plot_outputs_forecast(1)
  }
}



# sl <- MLE_check(p_name = "recover",theta_tab = seq((1/8),(1/15),le=100),nn = 2e3)
# # # # sl <- MLE_check(p_name = "betavol",theta_tab = seq(0.01,1,le=100),nn = 2e3)
# write.csv(sl,"recover_likelihoods.csv")
# plot(sl)
# recovery_val <- 1/sl[which.max(sl$lik),1]
# print(recovery_val)
# source("ls_driver.R")
# source("mle_driver.R")

# source("segment_wise_ls.R")
