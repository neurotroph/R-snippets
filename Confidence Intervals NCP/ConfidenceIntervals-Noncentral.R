# ConfidenceIntervals-Noncentral.R
# Harms, Christopher (2016)
# christopher.harms@uni-bonn.de
# github: @neurotroph
# v1.0 (27.09.2016)
#
# This script contains two functions to calculate the bounds of a confidence interval
# for the non-centrality parameter of the noncentral F- and t-Distribution. The bounds
# can be used to construct a confidence interval for effect size measures, an approach
# detailed by Steiger & Foulandi (1997) in Harlow et al. (1997) and used for ANOVA
# designs in Steiger (2004).
#
# References:
#   Harlow, L. L., Mulaik, S. A., & Steiger, J. H. (Eds.). (1997). What If There Were
#     No Significance Tests? Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#   Steiger, J. H. (2004). Beyond the F Test: Effect Size Confidence Intervals and Tests
#     of Close Fit in the Analysis of Variance and Contrast Analysis. Psychological
#     Methods, 9(2), 164–182. http://doi.org/10.1037/1082-989X.9.2.164

confint.ncp.t <- function(t.obs, df, alpha = .95, tol=1e-9) {
  # Calculates lower and upper bound for an (100*alpha)% confidence interval
  # on the non-centrality parameter of Student's t-Distribution
  #
  # Args:
  #   t.obs: Obversed t-Value
  #   df: Degrees of Freedom
  #   alpha: Area that should be covered by the Confidence Interval. Default: .95
  #   tol: Precision of bounds. Default: 1e-9
  #
  # Returns:
  #   List containing:
  #     $lower: lower bound of the (100*alpha)% confidence interval
  #     $upper: upper bound of the (100*alpha)% confidence interval

  delta.upper = NA
  delta.lower = NA
  
  t = pt(t.obs, df)
  t.center = t.obs
  
  alpha = 1-alpha
  
  if (t < alpha/2) {
      delta.lower = 0
      delta.upper = 0
  } else {
    # delta.upper = find \delta, so that
    # pt(t.obs, df, delta) == alpha/2
    delta.upper = optimize(function(x) {
      abs(pt(t.obs, df, x)-(alpha/2))
    }, interval=c(t.center, t.center*3), tol=tol)$minimum
  }
  if (t < 1-alpha/2) {
    delta.lower = 0
  } else {
    # delta.lower = find \delta, so that
    # pt(t.obs, df, delta) == 1-alpha/2
    delta.lower = optimize(function(x) {
      abs(pt(t.obs, df, x)-(1-alpha/2))
    }, interval=c(t.center-(t.center*2), t.center), tol=tol)$minimum
  }
  
  list(lower = delta.lower, upper = delta.upper)
}

confint.ncp.f <- function(f.obs, df.effect, df.error, alpha=.95, tol=1e-9) {
  # Calculates lower and upper bound for an (100*alpha)% confidence interval
  # on the non-centrality parameter of the F-distribution
  #
  # Args:
  #   F.obs: Obversed F-Value
  #   df.effect: Degrees of Freedom (denominator)
  #   df.error: Degrees of Freedom (nominator)
  #   alpha: Area that should be covered by the Confidence Interval. Default: .95
  #   tol: Precision of bounds. Default: 1e-9
  #
  # Returns:
  #   List containing:
  #     $lower: lower bound of the (100*alpha)% confidence interval
  #     $upper: upper bound of the (100*alpha)% confidence interval

  lambda.upper = NA
  lambda.lower = NA
  lambda.center = f.obs
  
  p = pf(f.obs, df.effect, df.error)
  alpha=1-alpha
  
  if (p < alpha/2) {
    lambda.lower = 0
    lambda.upper = 0
    
    return()
  } else {
    # lambda.upper = find \lambda, so that
    # pf(f.obs, df.effect, df.error, lambda) == alpha/2  
    lambda.upper = optimize(function(x) {
      abs(pf(f.obs, df.effect, df.error, x)-(alpha/2))
    }, interval=c(lambda.center, lambda.center+df.effect+df.error), tol=tol)$minimum
  }
  if (p < 1-alpha/2) {
    lambda.lower = 0
  } else {
    # lambda.lower = find \lambda, so that
    # pf(f.obs, df.effect, df.error, lambda) == 1-alpha/2
    lambda.lower = optimize(function(x) {
      abs(pf(f.obs, df.effect, df.error, x)-(1-alpha/2))
    }, interval=c(0, lambda.center), tol=tol)$minimum
  }
  
  list(lower=lambda.lower, upper=lambda.upper)
}
