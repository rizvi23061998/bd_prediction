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

run_fits <- function(rep_plot,nn,cut_off,dt,filename="1"){
  
  out_rep <- foreach(kk = 1:rep_plot) %dopar% {
    output_smc <- smc_model(theta,nn,dt)
    output_smc
  }
  
  S_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  I_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  # EE_out_plot = matrix(NA,ncol=rep_plot,nrow=t_period)
  II_out_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  C_plot = matrix(NA,ncol=rep_plot,nrow=t_period+forecast_window)
  # C_local_plot_real = matrix(NA,ncol=rep_plot,nrow=t_period)
  # C_local_plot_raw = matrix(NA,ncol=rep_plot,nrow=t_period)
  # Rep_local_plot = matrix(NA,ncol=rep_plot,nrow=t_period)
  # C_plot = matrix(NA,ncol=rep_plot,nrow=t_period)
  # Rep_plot = matrix(NA,ncol=rep_plot,nrow=t_period)
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
    # Rep_local_plot,
    # C_plot,
    # Rep_plot,
    R0_plot,
    file=paste0("outputs/bootstrap_fit_",filename,".RData")) 
  
}

# Plot outputs from SMC --------------------------------------------


plot_outputs <- function(filename="1"){
  
  filename="1"
  
  cut_off <- 0 #end_date - as.Date("2020-01-23")
  
  load(paste0("outputs/bootstrap_fit_",filename,".RData"))
  
  
  # write_csv(as_tibble(date_range[date_range<=as.Date("2020-01-23")]),"outputs/posterior_dates.csv")
  # write_csv(as_tibble(EE_out_plot[date_range<=as.Date("2020-01-23"),]),"outputs/posterior_E.csv")
  # write_csv(as_tibble(II_out_plot[date_range<=as.Date("2020-01-23"),]),"outputs/posterior_I.csv")
  
  # Remove NA fits
  S_plot = S_plot[,!is.na(S_plot[t_period,])]
  I_plot = I_plot[,!is.na(I_plot[t_period,])]
  C_plot = C_plot[,!is.na(C_plot[t_period,])]
  # C_local_plot_raw = C_local_plot_raw[,!is.na(C_local_plot_raw[t_period,])] # output raw cases
  # Rep_local_plot = Rep_local_plot[,!is.na(Rep_local_plot[t_period,])]
  # C_plot = C_plot[,!is.na(C_plot[t_period,])]
  # Rep_plot = Rep_plot[,!is.na(Rep_plot[t_period,])]
  R0_plot = R0_plot[,!is.na(R0_plot[t_period,])]
  
  # Calculate quantiles
  S_quantile <- apply(S_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))})#/theta[["population"]] # proportion
  Inf_quantile <- apply(I_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))})#/theta[["population"]] # proportion
  
  date_range_f <- c(date_range,seq(end_date+1,end_date+forecast_window,by=1))
  Case_local_quantile_raw <- apply(C_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))}) 
  
  R0_quantile <- apply(R0_plot,1,function(x){quantile(x,c(0.025,0.25,0.5,0.75,0.975))}) #*S_plot/theta[["pop_travel"]]
  
  S_traj <- round(S_quantile[3,])
  I_traj <- round(Inf_quantile[3,])
  C_local_traj <- cumsum(round(Case_local_quantile_raw[3,]))
  beta_traj <- R0_quantile[3,]*theta[["recover"]]
  
  # if(!is_forecast){
    save(
      S_traj,
      I_traj,
      C_local_traj,
      beta_traj,
      file = "outputs/rds/results.RData"
    )
  # }  
  # DEBUG - output data 
  # 
  
  aa <- cbind(date_range_f,round(Case_local_quantile_raw[3,]))
  aa <- as.data.frame(aa); colnames(aa) <- c("date","cases"); aa$date <- as.Date(aa$date,origin="1970-01-01")
  cm <- as.data.frame(aa)
  # cm <- cbind(cm,data$case_data_conf)
  # colnames(cm)[3] <- "real"
  write.csv(cm,paste("outputs/csv/case/case_model_",id,".csv",sep = ""))
  # tiff(paste("outputs/img/case/case_model_",id,".tiff",sep = ""),height = 3000,width = 3000,units = "px",res=300)
  
  
  real_df <- data.frame("Dates" = date_range,"Inc" = (data$case_data_conf))
  pred_df <- data.frame("Dates" = date_range_f,"Inc_pred" = (round(Case_local_quantile_raw[3,])))
  plot_df <- join(real_df,pred_df,by = "Dates",type="right")                         
  color_val <- c("Real" = "blue","Predicted" = "red")
  
  poly_df <- cbind('Dates' = c(date_range_f,rev(date_range_f)),
                   'Cases' = c(cumsum(Case_local_quantile_raw[2,]),rev(cumsum(Case_local_quantile_raw[4,]))))
  poly_df <- as.data.frame(poly_df)
  poly_df$Dates <- as.Date(poly_df$Dates,origin = "1970-01-01")
  saveRDS(plot_df, file = paste("outputs/rds/df/df_",id,".rds",sep = ""))
 
  date_rangeA <- date_range_f[1:(length(date_range_f)-cut_off)]
  # Case_local_quantile_onsetA <- Case_local_quantile_onset[,1:(length(date_range)-cut_off)]
  
  # forecast window
  date_rangeF <- date_range_f[date_range_f>end_date]#max(cases_Wuhan$date)]
  yyF <- rep(0,length(date_rangeF))
  
  # Plot outputs
  # a_col <- 0.4 # alpha
  xMin1 <- start_date#as.Date("2020-01-24") #min(as.Date("2019-12-01")) 
  xMin <- xMin1 #min(as.Date("2020-01-01"))
  xMax <- end_date-1 #max(date_range)
  
  
  # Plot reproduction number
  xMaxR <- end_date+forecast_window#as.Date("2020-04-07")
  date_rangeB <- date_range#date_rangeA#[date_rangeA>as.Date("2019-12-15")]
  R0_quantileB <- R0_quantile[,1:t_period]#[,date_rangeA>as.Date("2019-12-15")]
  xMax1 <- xMax #as.Date("2020-02-01") #xMax #xMax #
  
  dt <- sapply(date_rangeB, format,"%Y-%m-%d")
  rt <- cbind("date" = dt,"rt" = R0_quantileB[3,])
  print("last r_t")
  print(rt[nrow(rt),2])
  write.csv(rt,paste("outputs/csv/r_t/r_t_",id,".csv",sep = ''),row.names = F)
  
  rt_df <- cbind('Dates' = c(date_rangeB,rev(date_rangeB)),
                 'ci50' = c(R0_quantileB[2,],rev(R0_quantileB[4,])),
                 'ci95' = c(R0_quantileB[1,],rev(R0_quantileB[5,])),
                 'ci0' = c(R0_quantileB[3,],rev(R0_quantileB[3,])))
  
  rt_df <- as.data.frame(rt_df)
  # rt_df$Dates <- sapply(rt_df$Dates, as.Date,origin = "1970-01-01")
  rt_df$Dates <- as.Date(rt_df$Dates,format="%Y-%m-%d",origin = "1970-01-01")
  g_rt <- ggplot(data = rt_df)+
          geom_polygon(aes(x = Dates,y = ci50),fill = "#004DFF59")+
          geom_polygon(aes(x = Dates,y = ci95),fill = "#004DFF33")+
          geom_line(aes(x = Dates,y=ci0),col = "blue")+
          scale_x_date(date_breaks = "2 week", date_labels = "%d %b")+
          labs(x="Dates",y=expression(paste(R[t])))
  
  
  tiff(paste("outputs/r_t/r_t_",id,".tiff",sep=""),units = 'px',
          width=2000,height=2000,res=300)
  print(g_rt)
  dev.off()
  # plot(date_rangeB,R0_quantileB[,],col="white",xlab="",ylab=expression(paste(R[t])))
  # # polygon(c(date_rangeF,rev(date_rangeF)),c(yyF,rev(yyF+1e5)),lty=0,col=rgb(0.9,0.9,0.9))
  # 
  # polygon(c(date_rangeB,rev(date_rangeB)),c(R0_quantileB[2,],rev(R0_quantileB[4,])),lty=0,col=rgb(0,0.3,1,0.35))
  # # polygon(c(date_rangeB,rev(date_rangeB)),c(R0_quantileB[1,],rev(R0_quantileB[5,])),lty=0,col=rgb(0,0.3,1,0.2))
  # lines(date_rangeB,R0_quantileB[3,],type="l",col=rgb(0,0,1),xaxt="n",yaxt="n",xlab="",ylab="")
  # lines(date_rangeB,1+0*R0_quantileB[3,],lty=2)
  # 
  # #text(labels="model",x=xMin1,y=9,adj=0,col="blue")
  # 
  # # lines(c(wuhan_travel_restrictions,wuhan_travel_restrictions),c(0,10),col="red")
  # 
  # title(LETTERS[1],adj=0)
  # 
  # # dev.copy(tiff,paste("outputs/r_t/r_t_",id,".tiff",sep=""),units = 'px',
  # #          width=2000,height=2000,res=300)
  # dev.off()
  
  
}



# Plot dispersion --------------------------------------------

plot_dispersion <- function(filename="1"){
  
  # filename="1"
  
  load(paste0("outputs/bootstrap_fit_",filename,".RData"))
  
  # Extract R credible interval
  period_interest <- as.Date(c(start_date,start_date+14))
  
  par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(2,0.7,0))
  
  index_pick <- match(period_interest,date_range)
  
  R0_all <- R0_plot[index_pick[1]:index_pick[2],]
  #dim(R0_all) <- NULL # collapse data
  
  med_R0 <- apply(R0_all,2,function(x){quantile(x,0.5)})
  
  R0_CrI <- quantile(med_R0,c(0.025,0.25,0.5,0.75,0.975))
  
  # print(med_R0)
  print(R0_CrI)
  print(class(R0_CrI))
  fname <- paste('outputs/r0_',id,".csv",sep = "")
  write.csv(as.data.frame(R0_CrI),fname)
  
  MERS_k <- 0.26
  SARS_k <- 0.16
  k_seq <- seq(0.01,1,0.01)
  
  # Outbreak calcs
  R0_med <- 1-sapply(k_seq,function(x){numerical_solver(R0_CrI[3],x)})
  R0_CrI_1 <- 1-sapply(k_seq,function(x){numerical_solver(R0_CrI[1],x)})
  R0_CrI_2 <- 1-sapply(k_seq,function(x){numerical_solver(R0_CrI[5],x)})
  R0_CrI_1_50 <- 1-sapply(k_seq,function(x){numerical_solver(R0_CrI[2],x)})
  R0_CrI_2_50 <- 1-sapply(k_seq,function(x){numerical_solver(R0_CrI[4],x)})
  
  # Estimate range
  c(R0_med[k_seq==SARS_k],R0_med[k_seq==MERS_k])
  
  # Plot results
  plot(k_seq,R0_med,type="l",ylim=c(0,1),xlab=c("extent of homogeneity in transmission"),ylab="probability of large outbreak",col="white",xaxs="i",yaxs="i")
  polygon(c(k_seq,rev(k_seq)),c(R0_CrI_1,rev(R0_CrI_2)),lty=0,col=rgb(0,0.3,1,0.35))
  polygon(c(k_seq,rev(k_seq)),c(R0_CrI_1_50,rev(R0_CrI_2_50)),lty=0,col=rgb(0,0.3,1,0.35))
  lines(k_seq,R0_med,col="blue")
  
  lines(c(MERS_k,MERS_k),c(-1,10),lty=2); text(labels="MERS-CoV",x=1.02*MERS_k,y=0.7,adj=0,col="black")
  lines(c(SARS_k,SARS_k),c(-1,10),lty=2); text(labels="SARS",x=1.02*SARS_k,y=0.6,adj=0,col="black")
  title(LETTERS[1],adj=0)
  
  n_seq <- seq(0,10,1)
  ext_m <- 1-(1-R0_med[k_seq==SARS_k])^n_seq
  ext_1 <- 1-(1-R0_CrI_1[k_seq==SARS_k])^n_seq
  ext_2 <- 1-(1-R0_CrI_2[k_seq==SARS_k])^n_seq
  ext_11 <- 1-(1-R0_CrI_1_50[k_seq==SARS_k])^n_seq
  ext_22 <- 1-(1-R0_CrI_2_50[k_seq==SARS_k])^n_seq
  
  plot(n_seq,ext_m,type="l",ylim=c(0,1),xlim=c(-0.5,10.5),xlab=c("number of introductions"),ylab="probability of large outbreak",col="white",xaxs="i",yaxs="i")
  points(n_seq,ext_m,col="blue",pch=19)
  for(ii in 1:length(n_seq)){
    lines(c(n_seq[ii],n_seq[ii]),c(ext_1[ii],ext_2[ii]),col="blue")
  }
  
  
  title(LETTERS[2],adj=0)
  
  
  # dev.copy(pdf,paste("plots/Figure_3.pdf",sep=""),width=8,height=3)
  #dev.copy(png,paste("plots/calc_1.png",sep=""),units="cm",width=20,height=10,res=150)
  # dev.off()
  
  
}

# Output R0 estimates over time --------------------------------------------

r0_value_output <- function(filename="1"){
  
  # filename="1"
  
  load(paste0("outputs/bootstrap_fit_",filename,".RData"))
  
  write_csv(as_tibble(R0_plot),"out_R0.csv")
  
  write_csv(as_tibble(date_range),"out_date.csv")
  
  # Extract R0 estimates
  
  period_interest <- start_date + 10#as.Date("2020-01-15")
  
  R0_plot = R0_plot[,!is.na(R0_plot[t_period,])]
  
  med_R0 <- apply(R0_plot,1,function(x){quantile(x,c(0.5))})
  c.text(med_R0[match(period_interest,date_range)])
  
  #file_out <- as_tibble(cbind(date_range,c.text(t(med_R0))))
  
  # Median R0 after before closure
  
  period_interest <- c(start_date+1,start_date+11)#as.Date(c("2020-01-01","2020-01-23"))
  index_pick <- match(period_interest,date_range)
  R0_all <- R0_plot[index_pick[1]:index_pick[2],]
  
  med_R03 <- apply(R0_all,1,function(x){quantile(x,c(0.5))})
  
  # Median R0 range before closure
  period_interest <- c(start_date + 5,start_date + 5)#as.Date(c("2020-01-16","2020-01-16"))
  index_pick <- match(period_interest,date_range)
  R0_all <- R0_plot[index_pick[1]:index_pick[2],]
  
  med_R02 <- apply(R0_all,1,function(x){quantile(x,c(0.5))})
  med_R02 <- R0_all; dim(med_R02) <- NULL
  
  
  # Median R0 after before closure
  
  period_interest <- as.Date(c("2020-01-31","2020-01-31"))
  index_pick <- match(period_interest,date_range)
  R0_all <- R0_plot[index_pick[1]:index_pick[2],]
  
  #med_R0 <- apply(R0_all,1,function(x){quantile(x,c(0.5))})
  med_R0 <- R0_all; dim(med_R0) <- NULL
  
  out_r <- cbind(c.text(med_R02),c.text(med_R0),c.text(med_R03))
  out_r <- as_tibble(out_r); names(out_r) <- c("before","after")
  
  write_csv(out_r,"outputs/before_after_R.csv")
  
}




# Plot profile likelihoods ------------------------------------------------

profile_plot <- function(p1_name = "local_rep_prop", 
                         p2_name = "confirmed_prop", 
                         p3_name = "betavol", 
                         filename=1){
  
  # filename=1
  s_out <- read_csv(paste0("outputs/param_search_",filename,".csv"))
  s_out <- s_out %>% mutate(param_s = NA)
  #s_out[max(s_out$lik)==s_out$lik,] # maximum likelihood
  
  # Define parameter names
  
  # - - -
  # Calculate profiles
  
  for(kk in 1:3){ # iterate over parameters
    
    # Define parameter of interest
    if(kk==1){s_out$param_s <- s_out$param1}
    if(kk==2){s_out$param_s <- s_out$param2}
    if(kk==3){s_out$param_s <- s_out$param3}
    
    # Iterate over values and extract profile:
    unique_val <- unique(s_out$param_s)
    
    max_prof <- NULL
    for(ii in 1:length(unique_val)){
      max_lik_ii <-  s_out %>% filter(param_s == unique_val[ii]) %>% select(lik) %>% max()
      max_prof <- rbind(max_prof,c(unique_val[ii],max_lik_ii))
    }
    max_prof <- as_tibble(max_prof); names(max_prof) <- c("param","lik")
    
    # Plot profile for kk:
    par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(2,0.7,0))
    plot(max_prof$param,max_prof$lik,ylim=c(max(max_prof$lik)-4,max(max_prof$lik))+1,xlab="value",ylab="log likelihood",pch=19)
    
    
    # Add spline function
    max_prof2 <- max_prof[max_prof$lik>(max(max_prof$lik)-3),] # select near MLE
    model.likelihood <- gam(lik ~ s(param,k=3) , data = max_prof2) 
    if(kk<3){x.param <- seq(min(max_prof$param),max(max_prof$param),0.0001)}
    if(kk==3){x.param <- seq(min(max_prof$param),1.2*max(max_prof$param),0.0001)}
    y.predict <- predict(model.likelihood, list(param=x.param), type = "link", se.fit = TRUE)
    y.pred.out <- y.predict$fit
    
    lines(c(0,1e2),c(1,1)*(max(y.pred.out)-1.92),lty=2)
    
    lines(x.param,y.pred.out)
    mle_val <-x.param[y.pred.out==max(y.pred.out)]
    calc_95 <- x.param[y.pred.out>max(y.pred.out)-1.92]; calc_95 <- signif(c(mle_val,min(calc_95),max(calc_95)),3)
    
    text(x=min(max_prof2$param),y=max(max_prof2$lik)+0.5, labels = paste0(calc_95[1]," (95% CI: ",calc_95[2],"-",calc_95[3],")"),adj=0)
    
    #dev.copy(png,paste("plots/param_rel_",kk,".png",sep=""),units="cm",width=10,height=10,res=150)
    dev.copy(pdf,paste("plots/param_rel_",kk,".pdf",sep=""),width=5,height=3)
    dev.off()
    
  } # end param loop
  
  
}


# Plot distributions ------------------------------------------------------

plot_distn <- function(){
  
  xx <- seq(0,20,0.1)
  yy_recover <- dgamma(xx,shape=2,rate=2/(1/theta[["recover"]]))
  yy_incubation <- dgamma(xx,shape=2,rate=2/(1/theta[["incubation"]]))
  yy_report <- dexp(xx,rate=theta[["report"]])
  
  par(mfrow=c(1,3),mar=c(3,3,1,1),mgp=c(2,0.7,0)) #mfrow=c(4,2),
  plot(xx,yy_incubation,xlab="incubation period",ylab="probability density",type="l",yaxs="i",lwd=2)
  title(LETTERS[1],adj=0)
  plot(xx,yy_recover,xlab="infectious period",ylab="probability density",type="l",yaxs="i",lwd=2)
  title(LETTERS[2],adj=0)
  plot(xx,yy_report,xlab="delay onset-to-confirmation",ylab="probability density",type="l",yaxs="i",lwd=2)
  title(LETTERS[3],adj=0)
  
  dev.copy(pdf,paste("plots/Figure_S1.pdf",sep=""),width=8,height=3)
  dev.off()
  
}



# Calculate new oubtreak probability --------------------------------------

numerical_solver <- function(r0, k){
  
  fun <- function (s) {(1 + (r0/k)*(1 - s))^(-k) - s}
  solutions <- rootSolve::multiroot(fun, c(0, 1))$root
  
  realistic_sol <- min(solutions)
  return(realistic_sol)
  
}
