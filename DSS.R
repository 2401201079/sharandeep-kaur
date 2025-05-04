# -------------------------------
# Disease Spread Simulation (SIR Model)
# -------------------------------

# Install required packages (run once if not installed)
# install.packages("deSolve")
# install.packages("ggplot2")
# install.packages("reshape2")

# Load libraries
library(deSolve)
library(ggplot2)
library(reshape2)

# 1. Define the SIR model equations
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# 2. Set initial conditions and parameters
N <- 1000                              # Total population
initial_state <- c(S = 999, I = 1, R = 0)  # Initial values
parameters <- c(beta = 0.3, gamma = 0.1)   # Infection and recovery rates
times <- seq(0, 160, by = 1)              # Time steps (days)

# 3. Solve the SIR model using ode()
sir_output <- ode(y = initial_state, times = times, func = sir_model, parms = parameters)

# 4. Convert output to data frame
sir_output_df <- as.data.frame(sir_output)

# 5. Reshape data for plotting
sir_melted <- melt(sir_output_df, id = "time")

# 6. Plot the SIR model output
ggplot(data = sir_melted, aes(x = time, y = value, color = variable)) +
  geom_line(size = 1.2) +
  labs(title = "SIR Model Simulation of Disease Spread",
       x = "Time (Days)",
       y = "Number of Individuals",
       color = "Compartment") +
  theme_minimal()
