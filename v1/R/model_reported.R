process_model_r <- function(t_start,t_end,dt,theta,simTab,simzetaA){
  
  # simTab <- storeL[,tt-1,]; t_start = 1; t_end = 2; dt = 0.1; simzetaA <- simzeta[1,]; travelF=theta[["travel_frac"]]
  # print(simzetaA)
  susceptible_t <- simTab[,"sus"] # input function
  # exposed_t <- simTab[,"exp"] # input function
  
  
  infectious_t <- simTab[,"inf"] # input function
  
  
  removed_t <- simTab[,"rem"] #input function
  
  waiting_t <- simTab[,"waiting"] # input function
  cases_t <- simTab[,"cases"] # input function
  reports_t <- simTab[,"reports"] # input function
  
  # scale transitions
  inf_rate <- (simzetaA/theta[["population"]])*dt
  
  rec_rate <- theta[["recover"]]*dt
  rep_rate <- theta[["report"]]*dt
  
  
  prob_rep <- exp(-theta[["report"]]*theta[["recover"]]) # probability case is reported rather than recovers
  
  # prob_rep_local <- exp(-theta[["report"]]*theta[["recover"]]) # probability case is reported rather than recovers
  # prob_rep_local <- 1 #changed = = = =   
  for(ii in seq((t_start+dt),t_end,dt) ){
    
    # transitions
    S_to_E1 <- susceptible_t*(infectious_t)*inf_rate # stochastic transmission
    
    
    # Delay until recovery
    
    I2_to_R <- infectious_t*rec_rate
    
    # Delay until reported
    W_to_Rep_local <- waiting_t*rep_rate
    
    # Process model for SEIR
    susceptible_t <- susceptible_t - S_to_E1
    
    infectious_t <- infectious_t  + S_to_E1  - I2_to_R
    
    removed_t <- removed_t + I2_to_R
    
    # Case tracking - including removal of cases within Q compartment
    # cases_t <- cases_t + S_to_E1
    waiting_t <- waiting_t + S_to_E1*prob_rep - W_to_Rep_local
    cases_t <- cases_t + S_to_E1*prob_rep
    reports_t <- reports_t + W_to_Rep_local
    
    # print(cases_t[1:5])
  }
  
  simTab[,"sus"] <- susceptible_t # output
  simTab[,"inf"] <- infectious_t # output
  simTab[,"rem"] <- removed_t
  simTab[,"cases"] <- cases_t
  
  simTab[,"waiting"] <- waiting_t  
  
  simTab[,"reports"] <- reports_t  
  
  simTab
  
}

# SMC function --------------------------------------------

smc_model_r <- function(theta,nn,dt=1){
  
  # nn = 100;   dt <- 0.25
  
  # Assumptions - using daily growth rate
  ttotal <- t_period
  t_length <- ttotal
  
  storeL <- array(0,dim=c(nn,t_length + forecast_window, length(theta_initNames)),dimnames = list(NULL,NULL,theta_initNames))
  
  # Add initial condition
  
  storeL[,1,"inf"] <- theta[["init_cases"]]
  
  storeL[,1,"sus"] <- theta[["population"]] - theta[["init_cases"]]
  storeL[,1,"cases"] <- theta[["init_cases"]]
  storeL[,1,"reports"] <- theta[["init_cases"]]
  
  #simzeta <- matrix(rlnorm(nn*t_length, mean = -theta[["betavol"]]^2/2, sd = theta[["betavol"]]),ncol=ttotal)
  simzeta <- matrix(rnorm(nn*t_length, mean = 0, sd = theta[["betavol"]]),nrow=ttotal)
  simzeta[1,] <- exp(simzeta[1,])*theta[["beta"]] # define IC
  
  # Fix R for forward simulation
  #simzeta[fix_r0_tt:ttotal,] <- log(theta[["r0_decline"]])
  
  # Latent variables
  S_traj = matrix(NA,ncol=1,nrow=ttotal + forecast_window)
  C_local_traj = matrix(NA,ncol=1,nrow=ttotal + forecast_window)
  I_traj = matrix(NA,ncol=1,nrow=ttotal + forecast_window)
  Rep_traj = matrix(NA,ncol=1,nrow=ttotal + forecast_window)
  beta_traj = matrix(NA,ncol=1,nrow=ttotal + forecast_window)
  W_traj <- matrix(NA,ncol=1,nrow=ttotal + forecast_window)
  w <- matrix(NA,nrow=nn,ncol=ttotal); w[,1] <- 1  # weights
  W <- matrix(NA,nrow=nn,ncol=ttotal)
  A <- matrix(NA,nrow=nn,ncol=ttotal) # particle parent matrix
  l_sample <- rep(NA,ttotal)
  lik_values <- rep(0,ttotal)
  
  # Iterate through steps
  
  for(tt in 2:(ttotal)){
    # Add random walk on transmission ?
    simzeta[tt,] <- simzeta[tt-1,]*exp(simzeta[tt,])
    
    # run process model
    storeL[,tt,] <- process_model_r(tt-1,tt,dt,theta,storeL[,tt-1,],simzeta[tt,])
    
    # calculate weights
    w[,tt] <- AssignWeights_r(data,storeL,nn,theta,tt)
    # print(w[,tt])
    # check likelihood isn't NA
    if(is.na(max(w[1:nn,tt])) | max(w[1:nn,tt]) == 0){
      likelihood0 = -Inf
      return(list(S_trace=S_traj,I_trace=I_traj,beta_trace=beta_traj,W_trace=W_traj,
                  Rep_trace = Rep_traj ,C_trace = C_local_traj,lik=likelihood0 ))
    }
    
    # normalise particle weights
    sum_weights <- sum(w[1:nn,tt])
    W[1:nn,tt] <- w[1:nn,tt]/sum_weights
    
    # resample particles by sampling parent particles according to weights:
    A[, tt] <- sample(1:nn,prob = W[1:nn,tt],replace = T)
    
    
    # Resample particles for corresponding variables
    storeL[,tt,] <- storeL[ A[, tt] ,tt,]
    simzeta[tt,] <- simzeta[tt, A[, tt]] #- needed for random walk on beta
    
    
  } # END PARTICLE LOOP
  
  # ---------- ttotal changed -----------------
  ttotal <- ttotal 
  # Estimate likelihood:
  for(tt in 1:ttotal){
    lik_values[tt] = log(sum(w[1:nn,tt])) # log-likelihoods
  }
  
  likelihood0 = -ttotal*log(nn)+ sum(lik_values) # add full averaged log-likelihoods
  
  # print(likelihood0)
  # Sample latent variables:
  locs <-  sample(1:nn,prob = W[1:nn,tt],replace = T)
  l_sample[ttotal] <- locs[1]
  
  S_traj[ttotal,] <- storeL[l_sample[ttotal],ttotal,"sus"]
  I_traj[ttotal,] <- storeL[l_sample[ttotal],ttotal,"inf"]
  beta_traj[ttotal,] <- simzeta[ttotal,l_sample[ttotal]]
  C_local_traj[ttotal,] <- storeL[l_sample[ttotal],ttotal,"cases"]
  Rep_traj[ttotal,] <- storeL[l_sample[ttotal],ttotal,"reports"]
  W_traj[ttotal,] <- storeL[l_sample[ttotal],ttotal,"waiting"]
  
  for(ii in seq(ttotal,2,-1)){
    l_sample[ii-1] <- A[l_sample[ii],ii] # have updated indexing
    C_local_traj[ii-1,] <- storeL[l_sample[ii-1],ii-1,"cases"]
    
    S_traj[ii-1,] <- storeL[l_sample[ii-1],ii-1,"sus"]
    
    I_traj[ii-1,] <- storeL[l_sample[ii-1],ii-1,"inf"]
    Rep_traj[ii-1,] <- storeL[l_sample[ii-1],ii-1,"reports"]
    W_traj[ii-1,] <- storeL[l_sample[ii-1],ii-1,"waiting"]
    beta_traj[ii-1,] <- simzeta[ii-1,l_sample[ii-1]]
  }
  
  new_beta <- (beta_traj[ttotal,] * scaled_beta)
  
  
  
  for (tt in (ttotal+1):(ttotal + forecast_window)){
    storeL[,tt,] <- process_model_r(tt-1,tt,dt,theta,storeL[,tt-1,],new_beta) 
    
    C_local_traj[tt,] <- storeL[l_sample[ttotal],tt,"cases"]
    Rep_traj[tt,] <- storeL[l_sample[ttotal],tt,"reports"]
    
    S_traj[tt,] <- storeL[l_sample[ttotal],tt,"sus"]
    
    I_traj[tt,] <- storeL[l_sample[ttotal],tt,"inf"]
    W_traj[tt,] <- storeL[l_sample[ttotal],tt,"waiting"]
    beta_traj[tt,] <- new_beta
    
  }
  
  
  
  
  # print(list(S_trace=S_traj,C_local_trace=C_local_traj,Rep_local_trace=Rep_local_traj,C_trace=C_traj,Rep_trace=Rep_traj,E_trace=E_traj,I_trace=I_traj,beta_trace=beta_traj,lik=likelihood0 ))
  return(list(S_trace=S_traj,I_trace=I_traj,Rep_trace=Rep_traj,W_trace=W_traj,
              C_trace=C_local_traj,beta_trace=beta_traj,lik=likelihood0 ))
  
  
}


# Likelihood calc for SMC --------------------------------------------

AssignWeights_r <- function(data_list,storeL,nn,theta,tt){
  
  # Gather data
  # print(data_list[1:5,])
  local_case_data_conf_tt <- data_list$case_data_conf[tt]
  
  local_case_removed_tt <- data_list$case_data_death[tt] + data_list$case_data_recovered[tt]
  
  # Gather variables
  # case_localDiff <- storeL[,tt,"cases"] - storeL[,tt-1,"cases"]
  case_localDiff <- storeL[,tt,"reports"] - storeL[,tt-1,"reports"]
  # rep_local <- storeL[,tt,"reports_local"]
  # repDiff_local <- storeL[,tt,"reports_local"] - storeL[,tt-1,"reports_local"]
  rem_val_diff <- storeL[,tt,"rem"] - storeL[,tt-1,"rem"]
  # Prevalence - scale by asymptomatics - second half only // storeL[,tt,"exp1"] + 
  inf_prev <-  (storeL[,tt,"inf"])#*(1-theta[["confirmed_prop"]])
  
  c_local_val <- pmax(0,case_localDiff)
  rem_val <- pmax(0,rem_val_diff)
  
  
  # Local confirmed cases (by confirmation) -- HOLD OUT FOR NOW AS LIKELIHOOD LOW
  
  if(!is.na(local_case_data_conf_tt)){
    expected_val <- c_local_val#rep_val_local * theta[["confirmed_prop"]] * theta[["local_rep_prop"]] # scale by reporting proportion and known onsets
    # loglikSum_local_conf <- dpois(local_case_data_conf_tt,lambda = expected_val,log=T)
    
    loglikSum_local_conf <- dnbinom(x=local_case_data_conf_tt,mu=expected_val,size=1,log=T)
  }else{
    loglikSum_local_conf <- 0
    print("0")
  }
  
  if(!is.na(local_case_removed_tt)){
    
    expected_val <- rem_val
    loglikSum_local_rem <- dnbinom(x = local_case_removed_tt, mu = expected_val, size = 1, log = T)
  }else{
    loglikSum_local_rem <- 0
  }
  
  
  loglikSum <- loglikSum_local_conf #+ loglikSum_local_rem
  
  # print(exp(loglikSum))
  exp(loglikSum) # convert to normal probability
  
}

MLE_check_r <- function(p_name = "recover", theta_tab,nn=1e3){
  
  # theta_tab <- seq(0.001,0.01,0.001)
  store_lik <- NULL
  
  for(ii in 1:length(theta_tab)){
    
    theta[[p_name]] <- theta_tab[ii]
    
    # Run SMC and output likelihooda
    output_smc <- smc_model(theta,
                            nn=1e3 # number of particles
    )
    store_lik <- rbind(store_lik,c(theta_tab[ii],output_smc$lik))
    
  }
  
  colnames(store_lik) <- c("param","lik")
  store_lik <- as.data.frame(store_lik)
  store_lik
}




MLE_check_2D_r <- function(p1_name = "local_rep_prop", p2_name = "confirmed_prop", 
                         theta_tab1, theta_tab2,nn=1e3,
                         filename = 1){
  
  # p1_name = "local_rep_prop"; p2_name = "confirmed_prop"; theta_tab1 = seq(0.01,0.05,0.01); theta_tab2 = seq(0.3,1,0.1)
  
  store_lik <- NULL
  
  for(ii in 1:length(theta_tab1)){
    
    for(jj in 1:length(theta_tab2)){
      
      theta[[p1_name]] <- theta_tab1[ii]
      theta[[p2_name]] <- theta_tab2[jj]
      
      # Run SMC and output likelihooda
      output_smc <- smc_model_r(theta,
                              nn=2e3 # number of particles
      )
      store_lik <- rbind(store_lik,c(theta_tab1[ii],theta_tab2[jj],output_smc$lik))
      
    } 
  }
  
  colnames(store_lik) <- c(p1_name,p2_name,"lik")
  store_lik <- as_tibble(store_lik)
  
  write_csv(store_lik,paste0("outputs/param_search_",filename,".csv"))
  store_lik
}

MLE_check_r <- function(p_name = "recover", theta_tab,nn=1e3){
  
  # theta_tab <- seq(0.001,0.01,0.001)
  store_lik <- NULL
  
  for(ii in 1:length(theta_tab)){
    
    theta[[p_name]] <- theta_tab[ii]
    
    # Run SMC and output likelihooda
    output_smc <- smc_model_r(theta,
                            nn=1e3 # number of particles
    )
    store_lik <- rbind(store_lik,c(theta_tab[ii],output_smc$lik))
    
  }
  
  colnames(store_lik) <- c("param","lik")
  store_lik <- as.data.frame(store_lik)
  store_lik
}

smc_model_rf <- function(theta,nn,dt=1){
  
  # nn = 100;   dt <- 0.25
  
  # Assumptions - using daily growth rate
  ttotal <- t_period
  t_length <- ttotal
  
  load("outputs/rds/results_report.RData")
  
  storeL <- array(0,dim=c(nn,t_length + forecast_window, length(theta_initNames)),dimnames = list(NULL,NULL,theta_initNames))
  
  # Add initial condition
  
  storeL[,1,"inf"] <- theta[["init_cases"]]
  
  storeL[,1,"sus"] <- theta[["population"]] - theta[["init_cases"]]
  storeL[,1,"cases"] <- theta[["init_cases"]]
  storeL[,1,"reports"] <- theta[["init_cases"]]
  
  #simzeta <- matrix(rlnorm(nn*t_length, mean = -theta[["betavol"]]^2/2, sd = theta[["betavol"]]),ncol=ttotal)
  simzeta <- matrix(rnorm(nn*t_length, mean = 0, sd = theta[["betavol"]]),nrow=ttotal)
  simzeta[1,] <- exp(simzeta[1,])*theta[["beta"]] # define IC
  
  # Fix R for forward simulation
  #simzeta[fix_r0_tt:ttotal,] <- log(theta[["r0_decline"]])
  
  # Latent variables
  new_beta <- (beta_traj[ttotal] * scaled_beta)
  
  
  storeL[,ttotal,"inf"] <- I_traj[ttotal]
  storeL[,ttotal,"sus"] <- S_traj[ttotal]
  storeL[,ttotal,"cases"] <- C_local_traj[ttotal]
  storeL[,ttotal,"reports"] <- Rep_traj[ttotal]
  storeL[,ttotal,"waiting"] <- W_traj[ttotal]
  
  
  for (tt in (ttotal+1):(ttotal + forecast_window)){
    storeL[,tt,] <- process_model_r(tt-1,tt,dt,theta,storeL[,tt-1,],new_beta) 
    Rep_traj[tt] <- storeL[1,tt,"reports"]
    C_local_traj[tt] <- storeL[1,tt,"cases"]
    
    S_traj[tt] <- storeL[1,tt,"sus"]
    
    I_traj[tt] <- storeL[1,tt,"inf"]
    W_traj[tt] <- storeL[1,tt,"waiting"]
    beta_traj[tt] <- new_beta
    
  }
  
  
  
  save(
    S_traj,
    I_traj,
    C_local_traj,
    Rep_traj,
    beta_traj,
    W_traj,
    file = "outputs/rds/results_rf.RData"
  )
  # print(list(S_trace=S_traj,C_local_trace=C_local_traj,Rep_local_trace=Rep_local_traj,C_trace=C_traj,Rep_trace=Rep_traj,E_trace=E_traj,I_trace=I_traj,beta_trace=beta_traj,lik=likelihood0 ))
  
  
  
  # print(list(S_trace=S_traj,C_local_trace=C_local_traj,Rep_local_trace=Rep_local_traj,C_trace=C_traj,Rep_trace=Rep_traj,E_trace=E_traj,I_trace=I_traj,beta_trace=beta_traj,lik=likelihood0 ))
  return(list(S_trace=S_traj,I_trace=I_traj,Rep_trace=Rep_traj,C_trace=C_local_traj,
              beta_trace=beta_traj ))
  
  
}

plot_outputs_rf <- function(filename="1"){
  
  filename="1"
  
  cut_off <- 0 #end_date - as.Date("2020-01-23")
  
  load("outputs/rds/results_rf.RData")
  
  
  S_plot = S_traj#S_plot[,!is.na(S_plot[t_period,])]
  I_plot = I_traj#[,!is.na(I_plot[t_period,])]
  C_plot = C_local_traj - c(0,head(C_local_traj,-1))#[,!is.na(C_plot[t_period,])]
  # C_local_plot_r_plot[,!is.na(Rep_plot[t_period,])]
  Rep_plot = Rep_traj - c(0,head(Rep_traj,-1))
  R0_plot = beta_traj * theta[["recover"]]
  date_range_f <- c(date_range,seq(end_date+1,end_date+forecast_window,by=1))
  
  
  aa <- cbind(date_range_f,round(C_plot),round(Rep_plot))
  aa <- as.data.frame(aa); colnames(aa) <- c("date","cases","reports"); aa$date <- as.Date(aa$date,origin="1970-01-01")
  cm <- as.data.frame(aa)
  # cm <- cbind(cm,data$case_data_conf)
  # colnames(cm)[3] <- "real"
  write.csv(cm,paste("outputs/csv/case/reports/case_model_",id,".csv",sep = ""))
  
  real_df <- data.frame("Dates" = date_range,"Inc" = (data$case_data_conf))
  pred_df <- data.frame("Dates" = date_range_f,"Inc_pred" = (round(C_plot)))
  plot_df <- join(real_df,pred_df,by = "Dates",type="right")
  plot_df <- cbind(plot_df,"Reports"=round(Rep_plot))
  
  
  color_val <- c("Real" = "blue","Predicted" = "red")
  # poly_df <- data.frame("Dates" = date(),Cases = double())
  
  saveRDS(plot_df, file = paste("outputs/rds/df/reports/df_",id,".rds",sep = ""))
  
  
}



