#' bootstrap conditional effects
#' 2024-05-19
#' 
#' Alterations of Terrence Jorgensen's functions to bootstrap indrect and total
#' effects.


pacman::p_load(semTools)


# Slopes ------------------------------------------------------------------


# Conditional indirect effects for slopes
get_cond_ind_effects <- function(slope_fit, 
                                 int_fit, 
                                 group = group,
                                 pd_to_rebl = pd_to_rebl) {
  if (!is.null(group) & !is.na(group)) {
    condFX <-
      probe2WayMC(
        slope_fit,
        nameX = c("eewe", "att", "int"),
        nameY = "pd",
        modVar = "att",
        valProbe = c(-1, 0, 1)
      )
  } else {
    condFX <-
      probe2WayMC(
        slope_fit,
        nameX = c("eewe", "att", "int"),
        nameY = "pd",
        modVar = "att",
        valProbe = c(-1, 0, 1),
        group = group
      )
  }
  
  indFX <- condFX$SimpleSlope$est * coef(int_fit)[[pd_to_rebl]]
  
  names(indFX) <- paste("attr =", condFX$SimpleSlope$att)
  
  indFX
}



# Total Effects -----------------------------------------------------------


get_cond_total_effects <- function(slope_fit, 
                                   int_fit, 
                                   group = group,
                                   pd_to_rebl = pd_to_rebl,
                                   eewe_to_rebl = eewe_to_rebl) {
  if (!is.null(group) & !is.na(group)) {
    condFX <-
      probe2WayMC(
        slope_fit,
        nameX = c("eewe", "att", "int"),
        nameY = "pd",
        modVar = "att",
        valProbe = c(-1, 0, 1)
      )
  } else {
    condFX <-
      probe2WayMC(
        slope_fit,
        nameX = c("eewe", "att", "int"),
        nameY = "pd",
        modVar = "att",
        valProbe = c(-1, 0, 1),
        group = group
      )
  }

    total_effects <- (condFX$SimpleSlope$est * coef(int_fit)[[pd_to_rebl]]) + 
    coef(int_fit)[[eewe_to_rebl]]
  
  names(total_effects) <- paste("attr =", condFX$SimpleSlope$att)
  
  total_effects
}



# Both --------------------------------------------------------------------


bootstrap_cis_groups <- function(slope_fit,
                                 int_fit,
                                 which_survey,
                                 n_boot = 1000,
                                 seed = 42,
                                 group = NULL,
                                 pd_to_rebl = NULL,
                                 eewe_to_rebl = NULL) {
  

  # List for both sets of results
  results <- list()
  
  # Indirect -----
  # Bootstrap 1000 using function above
  set.seed(42)
  get_time('\nStarting bootstrap for indirect effects at')
  
  indirect_boot <- bootstrapLavaan(slope_fit,
                                   R = n_boot,
                                   FUN = get_cond_ind_effects,
                                   int_fit = int_fit,
                                   group = group,
                                   pd_to_rebl = pd_to_rebl)
  # Now get confidence intervals
  indirect_cis <- apply(
    indirect_boot,
    MARGIN = 2,
    FUN = quantile,
    probs = c(.025, .975),
    na.rm = TRUE
  )
  
  # Name it and save it
  indirect_ci_name <- paste0(which_survey, '_indirect_cis')
  results[[indirect_ci_name]] <- indirect_cis
  cat('\nBootstrapping for indirect effects complete')
  
  # Total -----
  # Bootstrap 1000 using function above
  set.seed(42)
  get_time('\nStarting bootstrap for direct effects at')
  total_boot <- bootstrapLavaan(slope_fit,
                                R = n_boot,
                                FUN = get_cond_total_effects,
                                int_fit = int_fit,
                                group = group,
                                pd_to_rebl = pd_to_rebl,
                                eewe_to_rebl = eewe_to_rebl)
  
  # Now get confidence intervals
  total_cis <- apply(
    total_boot,
    MARGIN = 2,
    FUN = quantile,
    probs = c(.025, .975),
    na.rm = TRUE
  )
  
  # Name it and add to results
  total_ci_name <- paste0(which_survey, '_total_cis')
  results[[total_ci_name]] <- total_cis
  cat('\nBootstrapping for total effects complete')
  
  return(results)
}



# Both for single model ---------------------------------------------------


bootstrap_cis <- function(slope_fit,
                          int_fit,
                          which_survey,
                          n_boot = 1000,
                          seed = 42,
                          pd_to_rebl,
                          eewe_to_rebl,
                          group = NA) {
  # List for both sets of results
  results <- list()
  
  # Indirect -----
  # Bootstrap 1000 using function above
  set.seed(42)
  get_time('\nStarting bootstrap for indirect effects at')
  
  indirect_boot <- bootstrapLavaan(slope_fit,
                                   R = n_boot,
                                   FUN = get_cond_ind_effects,
                                   int_fit = int_fit,
                                   pd_to_rebl = pd_to_rebl,
                                   group = group)
  # Now get confidence intervals
  indirect_cis <- apply(
    indirect_boot,
    MARGIN = 2,
    FUN = quantile,
    probs = c(.025, .975),
    na.rm = TRUE
  )
  
  # Name it and save it
  indirect_ci_name <- paste0(which_survey, '_indirect_cis')
  results[[indirect_ci_name]] <- indirect_cis
  cat('\nBootstrapping for indirect effects complete')
  
  # Total -----
  # Bootstrap 1000 using function above
  set.seed(42)
  get_time('\nStarting bootstrap for direct effects at')
  total_boot <- bootstrapLavaan(slope_fit,
                                R = n_boot,
                                FUN = get_cond_total_effects,
                                int_fit = int_fit, 
                                pd_to_rebl = pd_to_rebl,
                                eewe_to_rebl = eewe_to_rebl,
                                group = group)
  
  # Now get confidence intervals
  total_cis <- apply(
    total_boot,
    MARGIN = 2,
    FUN = quantile,
    probs = c(.025, .975),
    na.rm = TRUE
  )
  
  # Name it and add to results
  total_ci_name <- paste0(which_survey, '_total_cis')
  results[[total_ci_name]] <- total_cis
  cat('\nBootstrapping for total effects complete')
  
  return(results)
}





probe_interaction <- function(slope_fit, 
                              int_fit, 
                              which_survey,
                              folder) {
  
  # Results list
  results <- list()
  
  # Make plot
  probe <- 
    probe2WayMC(
      slope_fit,
      nameX = c("eewe", "att", "int"),
      nameY = "pd",
      modVar = "att",
      valProbe = c(-1, 0, 1)
    )
  
  filename <- paste0(folder, which_survey, '_probe_plot.png')

  png(filename = filename,
    res = 300,
    units = 'in',
    width = 6,
    height = 5
  )
  
  plot_probe <- plotProbe(
    probe,
    xlim = c(-3, 3),
    xlab = "Experience with Extreme Weather Events",
    ylab = "Psych Distance",
    legendArgs = list(title = "Attribution", lwd = 2)
  )
  dev.off()
  cat('\nSaved', paste0(folder, which_survey, '_probe_plot.png'), '\n')
  
  # Get simple slopes for model
  simple_slope_name <- paste0(which_survey, '_simple_slopes')
  results[[simple_slope_name]] <- probe$SimpleSlope
  
  # Conditional indirect effects of eewe on rebl
  indirect_effect_name <- paste0(which_survey, '_indirect_effect')
  results[[indirect_effect_name]] <- probe$SimpleSlope$est * coef(int_fit)[["a"]]
  
  # Conditional total effects of eewe on rebl
  total_effect_name <- paste0(which_survey, '_total_effect')
  results[[total_effect_name]] <- probe$SimpleSlope$est * coef(int_fit)[["a"]] + 
      coef(int_fit)[["b"]]
  
  # appended_results <- c(results_list, results)
  return(results)
}



# Probe Interaction TF ----------------------------------------------------


# Variation for the TA MGA analysis

probe_interaction_tf <- function(slope_fit,
                                 int_fit,
                                 which_survey,
                                 folder) {
  
  # Results list
  results <- list()
  
  ## TRUE -----
  # Make plot
  probe1 <- 
    probe2WayMC(
      slope_fit,
      nameX = c("eewe", "att", "int"),
      nameY = "pd",
      modVar = "att",
      valProbe = c(-1, 0, 1),
      group = 1
    )
  
  filename1 <- paste0(folder, which_survey, '_true_probe_plot.png')
  
  png(filename = filename1,
      res = 300,
      units = 'in',
      width = 6,
      height = 5
  )
  
  plot_probe1 <- plotProbe(
    probe1,
    xlim = c(-3, 3),
    xlab = "Experience with Extreme Weather Events",
    ylab = "Psych Distance",
    legendArgs = list(title = "Attribution", lwd = 2)
  )
  dev.off()
  cat('\nSaved', paste0(folder, which_survey, '_true_probe_plot.png'), '\n')
  
  # Get simple slopes for model
  true_simple_slope_name <- paste0(which_survey, '_true_simple_slopes')
  results[[true_simple_slope_name]] <- probe1$SimpleSlope
  
  # Conditional indirect effects of eewe on rebl
  true_indirect_effect_name <- paste0(which_survey, '_true_indirect_effect')
  results[[true_indirect_effect_name]] <- probe1$SimpleSlope$est * coef(int_fit)[["a1"]]
  
  # Conditional total effects of eewe on rebl
  true_total_effect_name <- paste0(which_survey, '_true_total_effect')
  results[[true_total_effect_name]] <- probe1$SimpleSlope$est * coef(int_fit)[["a1"]] + 
    coef(int_fit)[["b1"]]
  
  
  ## FALSE -----
  # Make plot
  probe2 <- 
    probe2WayMC(
      slope_fit,
      nameX = c("eewe", "att", "int"),
      nameY = "pd",
      modVar = "att",
      valProbe = c(-1, 0, 1),
      group = 2
    )
  
  filename2 <- paste0(folder, which_survey, '_false_probe_plot.png')
  
  png(filename = filename2,
      res = 300,
      units = 'in',
      width = 6,
      height = 5
  )
  
  plot_probe2 <- plotProbe(
    probe2,
    xlim = c(-3, 3),
    xlab = "Experience with Extreme Weather Events",
    ylab = "Psych Distance",
    legendArgs = list(title = "Attribution", lwd = 2)
  )
  dev.off()
  cat('\nSaved', paste0(folder, which_survey, '_false_probe_plot.png'), '\n')
  
  # Get simple slopes for model
  false_imple_slope_name <- paste0(which_survey, '_false_simple_slopes')
  results[[false_imple_slope_name]] <- probe2$SimpleSlope
  
  # Conditional indirect effects of eewe on rebl
  false_indirect_effect_name <- paste0(which_survey, '_false_indirect_effect')
  results[[false_indirect_effect_name]] <- probe2$SimpleSlope$est * coef(int_fit)[["a2"]]
  
  # Conditional total effects of eewe on rebl
  false_total_effect_name <- paste0(which_survey, '_false_total_effect')
  results[[false_total_effect_name]] <- probe2$SimpleSlope$est * coef(int_fit)[["a2"]] + 
    coef(int_fit)[["b2"]]
  
  # appended_results <- c(results_list, results)
  return(results)
}




# Bootstrap Parameters ----------------------------------------------------


#' Function to bootstrap parameters for the MGA by region. Hopefully we can make
#' this work for any parameter we want, so we can look at regressions, means, 
#' and even intercepts.


#' 