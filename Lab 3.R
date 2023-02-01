library(deSolve)
library(tidyverse)

# Part 1: Fid the final epidemic size using simulation

SIR = function(t, state, parameters) {
  with (as.list(c(state, parameters)),{
    dS = -beta*S*I/N;
    dI = beta*S*I/N - gamma*I;
    dR = gamma*I;
    list(c(dS, dI, dR))
  })
}

N = 1e5; # population size
I0 = 10; # initial No. of Infectious people
S0 = N - I0;  # initial No. of Susceptible people
R0 = 0;
state = c(S = S0, I = I0, R = R0);  # store the inital conditions I & S together 
parameters = c(beta = .5, gamma = .3);  # store the model parameters, unit: per day

times = seq(0, 365, by = 1);  # set simulation time steps: 0 to 100 days here

# Step 3: call the ode function to generate the simulation
sim = ode(y = state, times = times, func = SIR, parms = parameters);

# What is the final epidemic size? (i.e. R at the end of the epidemic)
sim[sim[, 'time'] == 365]

# Plot R over time
plot(x = sim[, 1], y = sim[, 4], xlab = 'Day', ylab = 'Number of recovery', type = 'l')


# Part 2: Test the epidemic threshold
parameters = c(beta = 0.5, gamma = 0.3);
sim = ode(y = state, times = times, func = SIR, parms = parameters);
plot(x = sim[, 1], y = sim[, 3], xlab = 'Day', ylab = 'Number of infectios', type = 'l')

parameters = c(beta = 0.5, gamma = 0.4);
sim = ode(y = state, times = times, func = SIR, parms = parameters);
plot(x = sim[, 1], y = sim[, 3], xlab = 'Day', ylab = 'Number of infectios', type = 'l')

parameters = c(beta = 0.5, gamma = 0.5);
sim = ode(y = state, times = times, func = SIR, parms = parameters);
plot(x = sim[, 1], y = sim[, 3], xlab = 'Day', ylab = 'Number of infectios', type = 'l')

parameters = c(beta = 0.5, gamma = 0.6);
sim = ode(y = state, times = times, func = SIR, parms = parameters);
plot(x = sim[, 1], y = sim[, 3], xlab = 'Day', ylab = 'Number of infectios', type = 'l')


# Part 3: Test the exponential period
N = 1e5; 
I0 = 10; 
S0 = N - I0;  
R0 = 0;
state = c(S = S0, I = I0, R = R0);   
parameters = c(beta = .5, gamma = .3);

times = seq(0, 100, by = 1); 

plo

