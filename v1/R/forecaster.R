
smc_model_forecast <- function(theta,nn,dt=1){
  
  filename = "1"
  # nn = 100;   dt <- 0.25
  load(paste0("outputs/rds/results",".RData"))
  # Assumptions - using daily growth rate
  ttotal <- t_period
  t_length <- ttotal
  
  storeL <- array(0,dim=c(nn,t_length + forecast_window, length(theta_initNames)),dimnames = list(NULL,NULL,theta_initNames))
  
  # Add initial condition
  
  storeL[,1,"inf"] <- theta[["init_cases"]]
  
  storeL[,1,"sus"] <- theta[["population"]] - theta[["init_cases"]]
  storeL[,1,"cases"] <- theta[["init_cases"]]
  
  #simzeta <- matrix(rlnorm(nn*t_length, mean = -theta[["betavol"]]^2/2, sd = theta[["betavol"]]),ncol=ttotal)
  simzeta <- matrix(rnorm(nn*t_length, mean = 0, sd = theta[["betavol"]]),nrow=ttotal)
  simzeta[1,] <- exp(simzeta[1,])*theta[["beta"]] # define IC
  
  
  # Fix R for forward simulation
  #simzeta[fix_r0_tt:ttotal,] <- log(theta[["r0_decline"]])
  
  # Latent variables
  # S_traj = matrix(NA,ncol=1,nrow=(ttotal + forecast_window))
  # C_local_traj = matrix(NA,ncol=1,nrow=(ttotal + forecast_window))
  # I_traj = matrix(NA,ncol=1,nrow=(ttotal + forecast_window))
  # beta_traj = matrix(NA,ncol=1,nrow=(ttotal + forecast_window))
  # 
  l_sample <- rep(NA,ttotal)
  lik_values <- rep(0,ttotal)
  
  # Iterate through steps
  
  beta_val <- mean(beta_traj[(ttotal-5):ttotal]) 
  
  new_beta <- (beta_traj[ttotal] * scaled_beta)
  
  
  storeL[,ttotal,"inf"] <- I_traj[ttotal]
  storeL[,ttotal,"sus"] <- S_traj[ttotal]
  storeL[,ttotal,"cases"] <- C_local_traj[ttotal]
  
  for (tt in (ttotal+1):(ttotal + forecast_window)){
    storeL[,tt,] <- process_model(tt-1,tt,dt,theta,storeL[,tt-1,],new_beta[tt-ttotal]) 
    
    C_local_traj[tt] <- storeL[1,tt,"cases"]
    
    S_traj[tt] <- storeL[1,tt,"sus"]
    
    I_traj[tt] <- storeL[1,tt,"inf"]
    beta_traj[tt] <- new_beta[tt-ttotal]
    
  }
  
  
  
  save(
    S_traj,
    I_traj,
    C_local_traj,
    beta_traj,
    file = "outputs/rds/results_f.RData"
  )
  # print(list(S_trace=S_traj,C_local_trace=C_local_traj,Rep_local_trace=Rep_local_traj,C_trace=C_traj,Rep_trace=Rep_traj,E_trace=E_traj,I_trace=I_traj,beta_trace=beta_traj,lik=likelihood0 ))
  return(list(S_trace=S_traj,I_trace=I_traj,C_trace=C_local_traj,beta_trace=beta_traj ))
  
  
}


plot_outputs_forecast <- function(filename="1"){
  
  filename="1"
  
  cut_off <- 0 #end_date - as.Date("2020-01-23")
  
  load(paste0("outputs/rds/results_f",".RData"))
  
  # Remove NA fits
  S_plot = S_traj#S_plot[,!is.na(S_plot[t_period,])]
  I_plot = I_traj#[,!is.na(I_plot[t_period,])]
  C_plot = C_local_traj - c(0,head(C_local_traj,-1))#[,!is.na(C_plot[t_period,])]
  # C_local_plot_r_plot[,!is.na(Rep_plot[t_period,])]
  R0_plot = beta_traj * theta[["recover"]]
  
  # Calculate quantiles
  # S_quantile <- apply(S_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))})#/theta[["population"]] # proportion
  # Inf_quantile <- apply(I_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))})#/theta[["population"]] # proportion
  # 
  date_range_f <- c(date_range,seq(end_date+1,end_date+forecast_window,by=1))
  # Case_local_quantile_raw <- apply(C_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))}) 
  
  # R0_quantile <- apply(R0_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))}) #*S_plot/theta[["pop_travel"]]
  
  
  # if(!is_forecast){
  
  # }  
  # DEBUG - output data 
  # 
  
  C_plot <- round(C_plot)
  aa <- cbind(date_range_f,C_plot)
  aa <- as.data.frame(aa); colnames(aa) <- c("date","cases"); aa$date <- as.Date(aa$date,origin="1970-01-01")
  cm <- as.data.frame(aa)
  # cm <- cbind(cm,data$case_data_conf)
  # colnames(cm)[3] <- "real"
  write.csv(cm,paste("outputs/csv/case/case_model_",id,".csv",sep = ""))
  # tiff(paste("outputs/img/case/case_model_",id,".tiff",sep = ""),height = 3000,width = 3000,units = "px",res=300)
  # plot(date_range,cumsum(case_data_Ezhou),type = 'p',col = "blue",ylab = "Cumulative Incidence",pch = 16, frame = FALSE)
  # lines(date_range,cumsum(round(Case_local_quantile_raw[3,])),col = "red")
  real_df <- data.frame("Dates" = date_range,"Inc" = (data$case_data_conf))
  pred_df <- data.frame("Dates" = date_range_f,"Inc_pred" = (C_plot))
  plot_df <- join(real_df,pred_df,by = "Dates",type="right")                         
  color_val <- c("Real" = "blue","Predicted" = "red")
  # poly_df <- data.frame("Dates" = date(),Cases = double())
  # poly_df <- cbind('Dates' = c(date_range_f,rev(date_range_f)),
  #                  'Cases' = c(cumsum(Case_local_quantile_raw[2,]),rev(cumsum(Case_local_quantile_raw[4,]))))
  # poly_df <- as.data.frame(poly_df)
  # poly_df$Dates <- as.Date(poly_df$Dates,origin = "1970-01-01")
  saveRDS(plot_df, file = paste("outputs/rds/df/df_",id,".rds",sep = ""))
  g_case <- ggplot()+ #ggplot(data = poly_df)+
    # geom_polygon(aes(x=Dates,y= Cases),fill = "#c9e5ff")+
    geom_point(data = plot_df,aes(x=Dates,y=Inc,color="Real")) + 
    
    geom_line(data=plot_df,aes(x=Dates,y=Inc_pred,color="Predicted")) +
    labs(x = "Dates", y= "Daily Incidence",color="") + 
    scale_color_manual(values = color_val)
  
  
  # idx_pred <- date_range[length(date_range)-forecast_window]
  # abline(v = idx_pred,col = "red")
  # print(round(Case_local_quantile_raw[3,]))
  print(g_case)
  # dev.off()
  # saveRDS(g_case,paste("outputs/rds/case/case_gg_",id,".rds",sep = ""))

  
  
}


