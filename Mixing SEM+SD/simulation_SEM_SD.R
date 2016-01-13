#
# Mixing up Standard Deviations and Standard Errors
# Harms, C. (2016)
#
# Email: christopher.harms@uni-bonn.de
# 
# All rights reserved.
#
# simulation_SEM_SD.R
#
# Version 1.0
# Date: 2016-01-13
#


# Reported descriptives from the paper
input.mean  <- 7.60
input.n     <- 21
input.sd    <- 0.36
input.min   <- 2.59
input.max   <- 10.04

# "True" Standard Deviation if reported values are actually SEM
input.sem   <- input.sd * sqrt(input.n)

# Configuration for simulation
value.prec  <- 2
simulations <- 1000000

# Initialization
std.dev <- rep(NA, simulations)
std.dev.sem <- rep(NA, simulations)
counter <- 0
counter.sem <- 0

for (i in 1:simulations)
{
  # Simulate samples: one for the originally reported values, one for the re-calculated values
  # Normality of data is assumed
  smpl <- round(c(rnorm(input.n-2, input.mean, input.sd), input.min, input.max), value.prec)
  smpl.sem <- round(c(rnorm(input.n-2, input.mean, input.sem), input.min, input.max), value.prec)
  # Accept & Reject values based on range reported in paper
  for (j in 1:input.n) {
    while ((smpl[j] < input.min) || (smpl[j] > input.max)) {
      smpl[j] <- round(rnorm(1, input.mean, input.sd), value.prec)
    }
    while ((smpl.sem[j] < input.min) || (smpl.sem[j] > input.max)) {
      smpl.sem[j] <- round(rnorm(1, input.mean, input.sem), value.prec)
    }
  }
  std.dev[i] <- sd(smpl)
  std.dev.sem[i] <- sd(smpl.sem)
  # Count hits (SD_sim <= SD_report / SD_sim <= SD_calculated)
  if (std.dev[i] <= input.sd)
    counter <- counter+1
  if (std.dev.sem[i] <= input.sem)
    counter.sem <- counter.sem+1
}

# Result output
counter/simulations
counter.sem/simulations
hist(std.dev)
abline(v=input.sd, col="red")
hist(std.dev.sem)
abline(v=input.sem, col="red")