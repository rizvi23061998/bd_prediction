# Helper functions --------------------------------------------

c.text<-function(x,sigF=3){
  bp1=signif(c(median(x),quantile(x,0.025),quantile(x,0.975)),sigF)
  paste(bp1[1]," (",bp1[2],"-",bp1[3],")",sep="")
}

c.nume<-function(x){
  bp1=c(median(x),quantile(x,0.025),quantile(x,0.975))
  as.numeric(bp1)
}

bin_conf <- function(x,n){
  htest <- binom.test(x,n)
  h_out <- c(x/n, htest$conf.int[1], htest$conf.int[2])
  h_out
}

# Run SMC to get bootstrap estimates --------------------------------------------

run_fits_r <- function(rep_plot,nn,cut_off,dt,filename="1"){
  
  out_rep <- foreach(kk = 1:rep_plot) %dopar% {
    output_smc <- smc_model_r(theta,nn,dt)
    output_smc
  }
  
  S_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  I_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  # EE_out_plot = matrix(NA,ncol=rep_plot,nrow=t_period)
  II_out_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  C_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  W_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  # C_local_plot_real = matrix(NA,ncol=rep_plot,nrow=t_period)
  # C_local_plot_raw = matrix(NA,ncol=rep_plot,nrow=t_period)
  # Rep_local_plot = matrix(NA,ncol=rep_plot,nrow=t_period)
  # C_plot = matrix(NA,ncol=rep_plot,nrow=t_period)
  Rep_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  R0_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  
  for(kk in 1:rep_plot){
    output_smc <- out_rep[[kk]]
    if(output_smc$lik != - Inf){
      I_plot[,kk] <- output_smc$I_trace
      # II_out_plot[,kk] <- output_smc$I_trace
      # I_plot[,kk] <- (output_smc$C_local_trace - c(0,head(output_smc$I_trace,-1)))
      # I_plot[,kk] <- output_smc$E_trace + output_smc$I_trace*(1-theta[["confirmed_prop"]])
      S_plot[,kk] <- output_smc$S_trace
      # case_local_pos <- theta[["confirmed_prop"]]*theta[["local_rep_prop"]]*(output_smc$C_local_trace - c(0,head(output_smc$C_local_trace,-1)))
      C_plot[,kk] <- (output_smc$C_trace - c(0,head(output_smc$C_trace,-1)))
      Rep_plot[,kk] <- (output_smc$Rep_trace - c(0,head(output_smc$Rep_trace,-1)))
      W_plot[,kk] <- (output_smc$W_trace - c(0,head(output_smc$W_trace,-1)))
      # C_local_plot[,kk] <- rpois(length(case_local_pos),lambda=case_local_pos)
      
      # case_local_pos <- theta[["confirmed_prop"]]*(output_smc$C_local_trace - c(0,head(output_smc$C_local_trace,-1)))
      # C_local_plot_raw[,kk] <- rpois(length(case_local_pos),lambda=case_local_pos)
      
      # rep_local_pos <- theta[["confirmed_prop"]]*(output_smc$Rep_local_trace - c(0,head(output_smc$Rep_local_trace,-1))) # ALL CASES: theta[["local_rep_prop"]]*
      # Rep_local_plot[,kk] <- rpois(length(rep_local_pos),lambda=rep_local_pos)
      
      # C_plot[,kk] <- theta[["confirmed_prop"]]*fit_int_cases(output_smc$C_trace - c(0,head(output_smc$C_trace,-1)))
      # Rep_plot[,kk] <- theta[["confirmed_prop"]]*fit_int_cases(output_smc$Rep_trace - c(0,head(output_smc$Rep_trace,-1))) # case difference
      R0_plot[,kk] <- output_smc$beta_trace/(theta[["recover"]])
    }
  }
  
  save(
    S_plot,
    I_plot,
    # EE_out_plot,
    II_out_plot,
    # C_local_plot,
    # C_local_plot_raw,
    C_plot,
    W_plot,
    # Rep_local_plot,
    # C_plot,
    Rep_plot,
    R0_plot,
    file=paste0("outputs/bootstrap_fit_r_",filename,".RData")) 
  
}

# Plot outputs from SMC --------------------------------------------


plot_outputs_r <- function(filename="1"){
  
  filename="1"
  
  cut_off <- 0 #end_date - as.Date("2020-01-23")
  
  load(paste0("outputs/bootstrap_fit_r_",filename,".RData"))
  
  
  # write_csv(as_tibble(date_range[date_range<=as.Date("2020-01-23")]),"outputs/posterior_dates.csv")
  # write_csv(as_tibble(EE_out_plot[date_range<=as.Date("2020-01-23"),]),"outputs/posterior_E.csv")
  # write_csv(as_tibble(II_out_plot[date_range<=as.Date("2020-01-23"),]),"outputs/posterior_I.csv")
  
  # Remove NA fits
  S_plot = S_plot[,!is.na(S_plot[t_period,])]
  I_plot = I_plot[,!is.na(I_plot[t_period,])]
  C_plot = C_plot[,!is.na(C_plot[t_period,])]
  W_plot = W_plot[,!is.na(W_plot[t_period,])]
  # C_local_plot_raw = C_local_plot_raw[,!is.na(C_local_plot_raw[t_period,])] # output raw cases
  # Rep_local_plot = Rep_local_plot[,!is.na(Rep_local_plot[t_period,])]
  # C_plot = C_plot[,!is.na(C_plot[t_period,])]
  Rep_plot = Rep_plot[,!is.na(Rep_plot[t_period,])]
  R0_plot = R0_plot[,!is.na(R0_plot[t_period,])]
  
  # Calculate quantiles
  S_quantile <- apply(S_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))})#/theta[["population"]] # proportion
  Inf_quantile <- apply(I_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))})#/theta[["population"]] # proportion
  
  date_range_f <- c(date_range,seq(end_date+1,end_date+forecast_window,by=1))
  Case_local_quantile_raw <- apply(C_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))}) 
  
  R0_quantile <- apply(R0_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))}) #*S_plot/theta[["pop_travel"]]
  Rep_quantile <- apply(Rep_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))}) 
  W_quantile <- apply(W_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))})#/theta[["population"]] # proportion
  
  S_traj <- round(S_quantile[3,])
  I_traj <- round(Inf_quantile[3,])
  C_local_traj <- cumsum(round(Case_local_quantile_raw[3,]))
  Rep_traj <- cumsum(round(Rep_quantile[3,]))
  beta_traj <- R0_quantile[3,]*theta[["recover"]]
  W_traj <- cumsum(W_quantile[3,])
  # 
  # # if(!is_forecast){
  save(
    S_traj,
    I_traj,
    C_local_traj,
    W_traj,
    Rep_traj,
    beta_traj,
    file = "outputs/rds/results_report.RData"
  )
  # }  
  # DEBUG - output data 
  # 
  
  aa <- cbind(date_range_f,round(Case_local_quantile_raw[3,]),round(Rep_quantile[3,]))
  aa <- as.data.frame(aa)
  colnames(aa) <- c("date","cases","reports")
  aa$date <- as.Date(aa$date,origin="1970-01-01")
  cm <- as.data.frame(aa)
  # cm <- cbind(cm,data$case_data_conf)
  # colnames(cm)[3] <- "real"
  write.csv(cm,paste("outputs/csv/case/reports/case_model_",id,".csv",sep = ""))
  tiff(paste("outputs/img/case/reports/case_model_",id,".tiff",sep = ""),height = 3000,width = 3000,units = "px",res=300)
  # plot(date_range,cumsum(case_data_Ezhou),type = 'p',col = "blue",ylab = "Cumulative Incidence",pch = 16, frame = FALSE)
  # lines(date_range,cumsum(round(Case_local_quantile_raw[3,])),col = "red")
  real_df <- data.frame("Dates" = date_range,"Inc" = (data$case_data_conf))
  pred_df <- data.frame("Dates" = date_range_f,"Inc_pred" = (round(Case_local_quantile_raw[3,])))
  plot_df <- join(real_df,pred_df,by = "Dates",type="right")
  plot_df <- cbind(plot_df,"Reports"=round(Rep_quantile[3,]))
  
  
  color_val <- c("Real" = "blue","Predicted" = "red")
  # poly_df <- data.frame("Dates" = date(),Cases = double())
  poly_df <- cbind('Dates' = c(date_range_f,rev(date_range_f)),
                   'Cases' = c(cumsum(Case_local_quantile_raw[2,]),rev(cumsum(Case_local_quantile_raw[4,]))))
  poly_df <- as.data.frame(poly_df)
  poly_df$Dates <- as.Date(poly_df$Dates,origin = "1970-01-01")
  saveRDS(plot_df, file = paste("outputs/rds/df/reports/df_",id,".rds",sep = ""))
  g_case <- ggplot(data = poly_df)+
    geom_polygon(aes(x=Dates,y= Cases),fill = "#c9e5ff")+
    geom_point(data = plot_df,aes(x=Dates,y=Inc,color="Real")) + 
    
    geom_line(data=plot_df,aes(x=Dates,y=Inc_pred,color="Predicted")) +
    labs(x = "Dates", y= "Daily Incidence",color="") + 
    scale_color_manual(values = color_val)
  
  
  # idx_pred <- date_range[length(date_range)-forecast_window]
  # abline(v = idx_pred,col = "red")
  # print(round(Case_local_quantile_raw[3,]))
  print(g_case)
  dev.off()
  
  # Rep_local_quantile <- apply(Rep_local_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))}) 
  
  
  
  
  # Remove final few points (as estimation less reliable)
  S_quantileA <- S_quantile[,1:(ncol(R0_quantile)-cut_off)]
  Inf_quantileA <- Inf_quantile[,1:(ncol(R0_quantile)-cut_off)]
  R0_quantileA <- R0_quantile[,1:(ncol(R0_quantile)-cut_off)]
  date_rangeA <- date_range_f[1:(length(date_range_f)-cut_off)]
  # Case_local_quantile_onsetA <- Case_local_quantile_onset[,1:(length(date_range)-cut_off)]
  
  # forecast window
  date_rangeF <- date_range_f[date_range_f>end_date]#max(cases_Wuhan$date)]
  yyF <- rep(0,length(date_rangeF))
  
  # - - - - - - - 
  # Calculate daily incidence
  #Case_diff_quantile <- Case_quantile[,1:ncol(Case_quantile)] - cbind(c(0,0,0),Case_quantile[,1:(ncol(Case_quantile)-1)])
  
  par(mar=c(2,3,1,1),mgp=c(2,0.55,0)) #mfrow=c(4,2),
  layout(matrix(c(1,1,1,2,3,4,5,6,7), 3, 3, byrow = TRUE))
  
  # Plot outputs
  a_col <- 0.4 # alpha
  xMin1 <- start_date#as.Date("2020-01-24") #min(as.Date("2019-12-01")) 
  xMin <- xMin1 #min(as.Date("2020-01-01"))
  xMax <- end_date-1 #max(date_range)
  
  
  # Plot reproduction number
  xMaxR <- end_date+forecast_window#as.Date("2020-04-07")
  date_rangeB <- date_rangeA#[date_rangeA>as.Date("2019-12-15")]
  R0_quantileB <- R0_quantileA#[,date_rangeA>as.Date("2019-12-15")]
  xMax1 <- xMax #as.Date("2020-02-01") #xMax #xMax #
  
  dt <- sapply(date_rangeB, format,"%Y-%m-%d")
  rt <- cbind(date = dt,rt = R0_quantileB[3,])
  print("last r_t")
  print(rt[nrow(rt),2])
  write.csv(rt,paste("outputs/csv/r_t/r_t_",id,".csv",sep = ''),row.names = F)
  plot(date_rangeB,R0_quantileB[1,],col="white",ylim=c(0,8),xlim=c(xMin1,xMaxR),xlab="",ylab=expression(paste(R[t])))
  polygon(c(date_rangeF,rev(date_rangeF)),c(yyF,rev(yyF+1e5)),lty=0,col=rgb(0.9,0.9,0.9))
  
  polygon(c(date_rangeB,rev(date_rangeB)),c(R0_quantileB[2,],rev(R0_quantileB[4,])),lty=0,col=rgb(0,0.3,1,0.35))
  polygon(c(date_rangeB,rev(date_rangeB)),c(R0_quantileB[1,],rev(R0_quantileB[5,])),lty=0,col=rgb(0,0.3,1,0.2))
  lines(date_rangeB,R0_quantileB[3,],type="l",col=rgb(0,0,1),xaxt="n",yaxt="n",xlab="",ylab="")
  lines(date_rangeB,1+0*R0_quantileB[3,],lty=2)
  
  #text(labels="model",x=xMin1,y=9,adj=0,col="blue")
  
  # lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(0,10),col="red")
  
  title(LETTERS[1],adj=0); letR = 2
  
  #dev.copy(png,paste("plots/cases_inference.png",sep=""),units="cm",width=18,height=18,res=150)
  dev.copy(tiff,paste("outputs/r_t/r_t_",id,".tiff",sep=""),units = 'px',
           width=2000,height=2000,res=300)
  dev.off()
  
  
}



