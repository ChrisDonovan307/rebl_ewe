# get_cis

# gets bootstrapped confidence intervals for an indirect path
# determined using condIndFX_dyn **which assumes a moderation structure**

#          EWE ---> PD
#          Attr ^

# this function gets CIs for a specific path using bootstrapLavaan
# it also sets seed at 42 each time

get_cis <-
  function(slope_fit,
           coef_fit,
           func = condIndFX_dyn,
           R = 10,
           leg1 = NULL,
           leg2 = NULL,
           leg3 = NULL,
           leg4 = NULL) {
    
    if (R < 1000){
      warning(paste0("Warning: R is set to ", R, ". At least 1,000 bootstraps needed for proper confidence intervals"))
    }
    
    set.seed(42)
    bootOut <-
      bootstrapLavaan(
        slope_fit,
        R = R,
        FUN = func,
        coef_fit = coef_fit,
        leg1 = leg1,
        leg2 = leg2,
        leg3 = leg3,
        leg4 = leg4
      )
    
    cis <- apply(bootOut, MARGIN = 2, FUN = quantile, probs = c(.025, .975))
    as.data.frame(cis)
  }