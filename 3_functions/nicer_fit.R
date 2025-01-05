#' nicer fit
#' 2024-05-29
#' 
#' nice_fit doesn't report srmr for some reason. We are just wrapping it to add
#' the srmr from lavaan output.

pacman::p_load(
  dplyr,
  lavaan,
  lavaanExtra
)

nicer_fit <- function(model, ...) {
  nice_fit(model, ...) %>% 
    mutate(srmr = format(round(fitMeasures(model)[['srmr']], 3), nsmall = 3), .before = aic)
}
