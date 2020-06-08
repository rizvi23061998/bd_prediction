library(bbmle)

gamma_val <- seq(from = 0.001, to = 0.11, le = 1000)
mle_val <- sapply(gamma_val, mLL,beta = .11,sigma = 1)
(max_mle_val <- min(mle_val))

gamma_hat <- gamma_val[mle_val == max_mle_val]

plot(gamma_val, mle_val, type = "l", lwd = 2,
     xlab = expression(paste("recovery rate ", gamma)),
     ylab = "Likelihood")
abline(h = max_mle_val, lty = 2, col = "grey")
abline(v = gamma_hat, lty = 2, col = "grey")

starting_param_val <- list(beta = .2, gamma = gamma_hat, sigma = 1)
estimates <- mle2(minuslogl = mLL, start = lapply(starting_param_val, log),
                  method = "Nelder-Mead", data = c(data,  N=total_population))
print(summary(estimates))

beta_mle <- exp(estimates@coef)[["beta"]]
gamma_mle <- exp(estimates@coef)[["gamma"]]

print(beta_mle)
print(gamma_mle)

prediction <- sir_1(beta = beta_mle,gamma = gamma_mle,S0 = N-1,I0=data$cases[1],R0=0,times = data$days,N= N)
preds <- round(prediction[,3])
png("likelihood_cum.png")
plot(data$days,cumsum(data$cases),type = 'p',col='blue',xlab = "Days",ylab ="Cumulative cases",
     main = "Max Likelihood Prediction")
lines(data$days,cumsum(preds),col = 'red')
dev.off()


last_day <- data$days[length(data$days)]
pred_days <- c(data$days,seq(last_day+1,last_day+forecast_window))
date_range <- seq(start_date,end_date+forecast_window,by=1)

prediction <- sir_1(beta = beta_mle,gamma = gamma_mle,S0 = N-data$cases[1],I0=data$cases[1],R0=0,times = pred_days,N= total_population)
preds <- round(prediction[,3])



# plot(date_range,cumsum(preds),type='l',col = 'red')
# points(date_range[1:last_day],cumsum(data$cases),col='blue')
# abline(v = date_range[last_day])

pred_days <- date_range[(last_day+1):(last_day+forecast_window)] %>% sapply(format,'%Y-%m-%d')
result <- cbind(pred_days,round(prediction[(last_day+1):(last_day+forecast_window),3]),cumsum(round(prediction[,3]))[(last_day+1):(last_day+forecast_window)])
print(result)
colnames(result) <- c("Date","Daily Infection","Cumulative Infection")
# write.xlsx(result,"prediction_max_likelihood_.xlsx")
