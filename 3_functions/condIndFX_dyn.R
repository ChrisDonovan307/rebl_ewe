# condIndFX_dyn

# This can be used to calculate an indirect effect
# using simple slopes and up to four legs through model
# NOTE - it assumes the basic moderation structure of:

#          EWE ---> PD
#          Attr ^

# Any deviations and this will not work!

condIndFX_dyn <- function(
    slope_fit,
    coef_fit,
    leg1 = NULL,
    leg2 = NULL,
    leg3 = NULL,
    leg4 = NULL) {
  
  # Get simple slopes from semTools package, +-1 SD for moderator
  condFX <-
    probe2WayMC(
      slope_fit,
      nameX = c("eweExpGen", "attribution", "int"),
      nameY = "psychDist",
      modVar = "attribution",
      valProbe = c(-1, 0, 1)
    )
  
  # Retrieve coefficients dynamically based on variable names
  # this is dumb but it will work
  coef_1 <- ifelse(is.null(leg1), 1, coef(coef_fit)[[leg1]])
  coef_2 <- ifelse(is.null(leg2), 1, coef(coef_fit)[[leg2]])
  coef_3 <- ifelse(is.null(leg3), 1, coef(coef_fit)[[leg3]])
  coef_4 <- ifelse(is.null(leg4), 1, coef(coef_fit)[[leg4]])
  
  # calculate indirect effect through up to four legs
  indFX <- condFX$SimpleSlope$est * coef_1 * coef_2 * coef_3 * coef_4
  
  # give names to moderator levels
  names(indFX) <- paste("attr =", condFX$SimpleSlope$attribution)
  
  indFX
}