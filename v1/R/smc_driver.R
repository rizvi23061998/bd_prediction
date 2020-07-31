output_smc <- smc_model(theta,2e3,dt=.25)
print(output_smc$lik)

# cum_case <- cumsum(data$cases)

# plot(data$days,cum_case,type = 'p')
# lines(data$days,round(output_smc$I_trace[,1]),col='red')

run_fits(100,2e3, 0,.1,1)

plot_outputs(1)

plot_dispersion(1)
