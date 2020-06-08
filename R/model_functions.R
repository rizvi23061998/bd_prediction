sir_1 <- function(beta, gamma, S0, I0, R0, times,N) {
  require(deSolve) # for the "ode" function
  
  # the differential equations:
  sir_equations <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS <- -beta * I * S / N
      dI <-  beta * I * S / N - gamma * I
      dR <-  gamma * I
      return(list(c(dS, dI, dR)))
    })
  }
  
  # the parameters values:
  parameters_values <- c(beta  = beta, gamma = gamma)
  
  # the initial values of variables:
  initial_values <- c(S = S0, I = I0, R = R0)
  
  # solving
  out <- ode(initial_values, times, sir_equations, parameters_values)
  
  # returning the output:
  as.data.frame(out)
}


ss <- function(beta, gamma ) {
  I0 <- data$cases[1]
  times <- data$days
  predictions <- sir_1(beta = beta, gamma = gamma,   # parameters
                       S0 = N - I0, I0 = I0, R0 = 0, # variables' intial values
                       times = times,N)                # time points
  predictions <-  c(N,head(predictions$S,-1)) - predictions$S
  predictions <- predictions[-1]
  observations <- data$cases[-1] # the fit is done on the other data points
  
  # observations <- cumsum(observations)
  # predictions <- cumsum(predictions)
  
  sum((observations - predictions)^2)
}

ss2 <- function(x) {
  ss(beta = x[1], gamma = x[2])
}

mLL <- function(beta, gamma, sigma, cases=data$cases,days=data$days, N=total_population ) {
  # print(N)
  beta <- exp(beta) # to make sure that the parameters are positive
  gamma <- exp(gamma)
  sigma <- exp(sigma)
  I0 <- cases[1] # initial number of infectious
  observations <- cases[-1] # the fit is done on the other data points
  predictions <- sir_1(beta = beta, gamma = gamma,
                       S0 = N - I0, I0 = I0, R0 = 0, times = days, N)
  predictions <- predictions$I[-1] # removing the first point too
  # returning minus log-likelihood:
  # -sum(dpois(x = observations, lambda = predictions, log = TRUE))
  observations <- cumsum(observations)
  predictions <- cumsum(predictions)
  lik = -sum(dnorm(x = observations, mean = predictions, sd = sigma, log = TRUE))
  lik
}


  