SE_2d <- function(beta_val,gamma_val){
  # store_ss <- NULL
  # print(gamma_val)
  # print(beta_val)
  out_ls1 <- foreach(ii = 1:length(beta_val)) %dopar% {
    store_ss <- NULL
    for (jj in 1:length(gamma_val)){
      
      ss_val <- ss(beta = beta_val[ii],gamma = gamma_val[jj])  
      # print(ss_val)
      store_ss <- rbind(store_ss,c(beta_val[ii],gamma_val[jj],ss_val))
    }
    store_ss  
  }
  
  store_ss <- NULL
  
  for(ii in 1:length(beta_val)){
    store_ss <- rbind(store_ss,out_ls1[[ii]])
  }
  store_ss
    
}





beta_val <- seq((.5/20),(5/3),le = 1000)
gamma_val <- seq((1/20),(1/2),le = 100)

segments <- c(22,35,47)

full_data <- data

data <- full_data[1:segments[1],] 

ss1 <- SE_2d(beta_val = beta_val, gamma_val = gamma_val)

write.csv(ss1,"outputs/se1.csv")

data <- full_data[(segments[1]+1):segments[2],] 
data$cases[1] <- sum(full_data$cases[1:(segments[1]+1)])
ss2 <- SE_2d(beta_val = beta_val, gamma_val = gamma_val)

write.csv(ss2,"outputs/se2.csv")


data <- full_data[(segments[2]+1):segments[3],] 
data$cases[1] <- sum(full_data$cases[1:(segments[2]+1)])
ss3 <- SE_2d(beta_val = beta_val, gamma_val = gamma_val)

write.csv(ss3,"outputs/se3.csv")


data <- full_data[(segments[3]+1):nrow(full_data),] 
data$cases[1] <- sum(full_data$cases[1:(segments[3]+1)])
ss4 <- SE_2d(beta_val = beta_val, gamma_val = gamma_val)

write.csv(ss4,"outputs/se4.csv")


