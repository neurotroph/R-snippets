# Confidence Intervals for Noncentrality Parameters
This script contains two functions to calculate the bounds of a confidence interval for the non-centrality parameter of the noncentral F- and t-Distribution. The bounds can be used to construct a confidence interval for effect size measures, an approach detailed by Steiger & Foulandi (1997) in Harlow et al. (1997) and used for ANOVA designs in Steiger (2004).

## Example
    > source('ConfidenceIntervals-Noncentral.R')
    > confint.ncp.f(13.18, 1, 314)
    $lower
    [1] 2.713216

    $upper
    [1] 31.44969

    > confint.ncp.t(3.04, 314, alpha=.99)
    $lower
    [1] 0.4429486

    $upper
    [1] 5.632345

## Dependencies
None.

## References
* Harlow, L. L., Mulaik, S. A., & Steiger, J. H. (Eds.). (1997). What If There Were No Significance Tests? Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
* Steiger, J. H. (2004). Beyond the F Test: Effect Size Confidence Intervals and Tests of Close Fit in the Analysis of Variance and Contrast Analysis. Psychological Methods, 9(2), 164–182. http://doi.org/10.1037/1082-989X.9.2.164