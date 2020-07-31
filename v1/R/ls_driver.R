
N<- total_population

#searching for beta
beta_val <- seq(from = (.5/10), to = (5/10), le = 100)
ss_val <- sapply(beta_val, ss, gamma = 1/10)
(min_ss_val <- min(ss_val))

beta_hat <- beta_val[ss_val == min_ss_val]


#  ----------------- plotting beta hat --------------------
plot(beta_val, ss_val, type = "l", lwd = 2,
     xlab = expression(paste("infectious contact rate ", beta)),
     ylab = "sum of squares")
# adding the minimal value of the sum of squares:
abline(h = min_ss_val, lty = 2, col = "grey")
# adding the estimate of beta:
abline(v = beta_hat, lty = 2, col = "grey")



#  ------------------ searching for gamma ------------------
gamma_val <- seq(from = 1/3, to = 1/14, le = 1000)
ss_val <- sapply(gamma_val, function(x) ss(beta_hat, x))
(min_ss_val <- min(ss_val))
(gamma_hat <- gamma_val[ss_val == min_ss_val])

plot(gamma_val, ss_val, type = "l", lwd = 2,
     xlab = expression(paste("recovery rate ", gamma)),
     ylab = "sum of squares")
abline(h = min_ss_val, lty = 2, col = "grey")
abline(v = gamma_hat, lty = 2, col = "grey")

starting_param_val <- c(beta_hat,gamma_hat)
ss_optim <- optim(starting_param_val, ss2,method = "Nelder-Mead")
# print(ss_optim)

predictions <- sir_1(beta = ss_optim$par[1],gamma = ss_optim$par[2],S0 = N-1,I0=data$cases[1],R0=0,times = data$days,N= N)
preds <- round(c(N,head(predictions$S,-1)) - predictions$S)
png("least_square.png")
plot(data$days,cumsum(data$cases),type = 'p',col='blue',xlab = "Days",ylab ="Cumulative cases",
     main = "Least Square Prediction")
lines(data$days,cumsum(preds),col = 'red')
dev.off()
last_day <- data$days[length(data$days)]
pred_days <- c(data$days,seq(last_day+1,last_day+forecast_window))
date_range <- seq(start_date,end_date+forecast_window,by=1)

predictions <- sir_1(beta = ss_optim$par[1],gamma = ss_optim$par[2],
                    S0 = N-data$cases[1],
                    I0=data$cases[1],
                    R0=0,
                    times = pred_days,N= N)
preds <- c(N,head(predictions$S,-1)) - predictions$S

# plot(date_range,cumsum(preds),type='l',col = 'red')
# points(date_range[1:last_day],cumsum(data$cases),col='blue')
# abline(v = date_range[last_day])

pred_days <- date_range[(last_day+1):(last_day+forecast_window)] %>% sapply(format,'%Y-%m-%d')
result <- cbind(
                pred_days,
                round(preds[(last_day+1):(last_day+forecast_window)]),
                cumsum(round(preds))[(last_day+1):(last_day+forecast_window)]
                )
print(result)
colnames(result) <- c("Date","Daily Infection","Cumulative Infection")
# write.xlsx(result,"prediction_least_square.xlsx")
