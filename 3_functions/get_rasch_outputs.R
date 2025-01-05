#' Get Rasch Outputs
#' 2024-04-24

#' Get all the diagrams and fits and such from RM.

#' Inputs
#'    1. CML models from eRm
#'    2. Clean Surveys to yoink prolificIDs
#'    3. A path to the folder where you want diagrams to be saved.
#'    4. A path to the folder where you want csvs saved.
#'    5. Vector of final REBL items

#' Outputs
#'    1. PI Maps saved to folder
#'    2. PW Maps saved to folder
#'    3. ICC Plots saved to folder
#'    3. REBL Score Histogram saved to folder
#'    4. Person Fit DF saved as csv and also object as output
#'    5. Item Fit DF saved as csv and also object as output



# Packages ----------------------------------------------------------------


pacman::p_load(dplyr,
               eRm,
               readr,
               ggplot2,
               purrr,
               tibble,
               ggpubr,
               stringr)

# Load a version of plotPImap that lets you change the margins
source('3_functions/plotPImap2.R')



# Function ----------------------------------------------------------------


get_rasch_outputs <- function(models,
                              surveys,
                              plot_path,
                              csv_path,
                              rebl_items,
                              seed = 42) {
  
  # Housekeeping -----
  results <- list()
  cat('\nSaving outputs:')
      
  
  # PI Maps  -----
  iwalk(models, ~ {
    png(
      filename = paste0(plot_path, 'pi_map_', .y, '.png'),
      width = 3000,
      height = 2500,
      units = 'px',
      res = 300
    )
    plotPImap2(
      .x, 
      sorted = TRUE,
      main = NA,
      latdim = 'Latent Trait',
      pplabel = 'Latent Trait\nDistribution',
      cex.gen = 0.8,
      margins = c(3, 11, 0, 1)
    )
    dev.off()
    cat('\n', paste0(plot_path, 'pi_map_', .y, '.png'))
  })
  
  # PW Item Maps -----
  iwalk(models, ~ {
    png(filename = paste0(plot_path, 'pw_item_map_', .y, '.png'),
        width = 3000,
        height = 2500,
        units = 'px',
        res = 300)
    plotPWmap(
      .x, 
      pmap = FALSE, 
      imap = TRUE,
      mainitem = NA,
      latdim = 'Item Difficulty',
      tlab = 'Infit t Statistic',
      cex.pch = 1.25,
      cex.gen = 1
    )
    dev.off()
    cat('\n',
        paste0(plot_path, 'pw_item_map_', .y, '.png'))
  })
  
  # PW Person Maps -----
  iwalk(models, ~ {
    png(filename = paste0(plot_path, 'pw_person_map_', .y, '.png'),
        width = 3000,
        height = 2500,
        units = 'px',
        res = 300)
    plotPWmap(
      .x, 
      pmap = TRUE, 
      imap = FALSE,
      mainperson = NA,
      latdim = 'Latent Trait',
      cex.pch = 1.25,
      cex.gen = 1
    )
    dev.off()
    cat('\n',
        paste0(plot_path, 'pw_person_map_', .y, '.png'))
  })
  
  # Sampled Person Item Map -----
  iwalk(models, ~ {

    png(filename = paste0(plot_path, 'sampled_person_item_map_', .y, '.png'),
        width = 3000,
        height = 2500,
        units = 'px',
        res = 300)
    set.seed(seed)
    plotPWmap(
      .x, 
      pmap = TRUE, 
      imap = TRUE,
      item.subset = seq(1, length(.x$betapar), 2),
      person.subset = sample(1:nrow(.x$X), size = 25, replace = FALSE),
      latdim = 'Latent Trait',
      cex.pch = 1.25,
      cex.gen = 1
    )
    dev.off()
    cat('\n',
        paste0(plot_path, 'sampled_person_item_map_', .y, '.png'))
  })
  
  # Person Fit -----
  # Get all person fit stats, including parameters (rebl) and prolificID
  # Note that st.res (residuals) are nested matrices
  results$person_fit <- map2(models, surveys, ~ {
    pp <- person.parameter(.x)
    
    # Now just person scores with person # and prolific ID
    person_scores <- coef(pp) %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      setNames(c('person', 'rebl_score')) %>% 
      mutate(prolificID = .y$prolificID)
    
    # All other fit statistics for all people. 
    # Separate so that we can pull names from it later
    pfit <- personfit(pp)[1:7] %>% 
      .[names(.) != 'st.res']

    # Continue getting person fit, join with person_scores    
    person_fit <- pfit %>% 
      map(as.vector) %>% 
      bind_rows() %>%
      as.data.frame() %>%
      mutate(person = names(pfit$p.fit)) %>% 
      inner_join(person_scores, by = 'person')
    
    if (nrow(person_scores) != nrow(person_fit)) {
      lost <- person_scores %>%
        anti_join(person_fit, by = 'person') %>% 
        select(person, prolificID) %>% 
        as.vector()
      
      for (i in 1:length(lost$person)) {
        warning(
          lost$person[i],
          ' was lost because they had no pfit statistic. (prolificID: ',
          lost$prolificID[i],
          ')',
          call. = FALSE
        )
      }
    }
    return(person_fit)    
  })
  
  # Now save these as separate CSVs.
  walk2(results$person_fit, names(results$person_fit), ~ {
    
    .x %>% 
      write_csv(file = paste0(csv_path, 'person_fit_', .y, '.csv'))
    
    cat('\n',
        paste0(csv_path, 'person_fit_', .y, '.csv'))
  })
  
  
  # REBL Hist All -----
  # Get min and max scores from all person fits to set x axis limits
  lower_limit <- map_dbl(results$person_fit, ~ {
    min(.x$rebl_score)
  }) %>% min()
  
  upper_limit <- map_dbl(results$person_fit, ~ {
    max(.x$rebl_score)
  }) %>% max()
  
  # Make a hist for each survey with common x axis limits
  rebl_hists <- map2(results$person_fit, names(results$person_fit), ~ {
    
    .x %>% 
      as.data.frame() %>% 
      ggplot(aes(x = rebl_score)) +
      geom_histogram(binwidth = 0.1,
                     color = "black",
                     fill = "grey") +
      theme_classic() +
      labs(x = 'REBL Score', 
           y = 'Count',
           title = .y) +
      theme(text = element_text(size = 16)) +
      coord_cartesian(ylim = c(0, 125), 
                      xlim = c(lower_limit, upper_limit)) +
      scale_x_continuous(breaks = seq(floor(lower_limit),
                                      ceiling(upper_limit),
                                      1))
  })
  
  # Save arrange them and save it
  arranged <- ggarrange(plotlist = rebl_hists,
                        ncol = 1,
                        nrow = length(models))
  
  ggsave(
    filename = paste0(plot_path, 'rebl_hist_all.png'),
    plot = arranged,
    width = 3000,
    height = 2500,
    units = 'px',
    dpi = 300
  )
  
  cat('\n', paste0(plot_path, 'rebl_hist_all.png'))
  
  # REBL Hists Indy -----
  
  # Individual Histograms without setting axes equal
  rebl_hists_indy <- map2(results$person_fit, names(results$person_fit), ~ {
    
    .x %>% 
      as.data.frame() %>% 
      ggplot(aes(x = rebl_score)) +
      geom_histogram(binwidth = 0.3,
                     color = "black",
                     fill = "grey") +
      theme_classic() +
      labs(x = 'REBL Score', 
           y = 'Count') +
      theme(text = element_text(size = 16))
  })
  
  # Save individual histograms
  walk2(rebl_hists_indy, names(models), ~ {
    ggsave(
      filename = paste0(plot_path, 'rebl_hist_', .y, '.png'),
      plot = .x,
      width = 3000,
      height = 2500,
      units = 'px',
      dpi = 300
    ) 
    cat('\n', paste0(plot_path, 'rebl_hist_', .y, '.png'))
  })
  
  
  # ICC Plots -----
  iwalk(models, ~ {
    
    png(filename = paste0(plot_path, 'icc_plot_', .y, '.png'),
        width = 3000,
        height = 2500,
        units = 'px',
        res = 300)
    
    plotINFO(
      .x,
      type = 'item',
      legpos = NULL,
      mainI = NA,
      xlab = 'REBL Score',
      ylabI = 'Item Information'
    )
    
    dev.off()
    
    cat('\n',
        paste0(plot_path, 'icc_plot_', .y, '.png'))
  })
  
  
  # Joint ICC Plots -----
  iwalk(models, ~ {
    
    png(filename = paste0(plot_path, 'joint_icc_plot_', .y, '.png'),
        width = 7,
        height = 6,
        units = 'in',
        res = 300)
    
    par(mar = c(5, 4, 1, 1) + 0.1)
    
    plotjointICC(
      .x,
      legend = TRUE,
      legpos = 'bottomright',
      lwd = 2,
      main = NULL,
      xlab = 'REBL Score',
      ylab = 'Probability to Perform REBL Item',
      xlim = c(-5, 6)
    )
    dev.off()
    
    cat('\n', paste0(plot_path, 'joint_icc_plot_', .y, '.png'))
  })
  
  # Item Fit Stats -----
  #' Getting all item info for all surveys. Includes fit, infit, outfit, 
  #' discrimination, difficulty, percent_eco, item parameters, SEs, residuals
  results$item_fit <- map2(models, surveys, ~ {
    
    # Get DF of percent eco for REBL items
    percent_eco_df <- .y %>%
      select(any_of(rebl_items)) %>%
      colMeans(na.rm = TRUE) %>%
      as.data.frame() %>%
      setNames('percent_eco') %>%
      rownames_to_column(var = 'rebl_item')
    
    # Get DF of most item fit statistics
    item_fit_df <- .x %>%
      person.parameter() %>%
      itemfit() %>% 
      .[-3] %>% 
      map(as.vector) %>%
      as.data.frame() %>% 
      mutate(rebl_item = percent_eco_df$rebl_item)
    
    #' Get DF of item difficulties. There is one less than item fit, I think
    #' because they are all relative to the first?
    item_difficulty_df <- data.frame(eta = .x$etapar,
                                     eta_se = .x$se.eta) %>%
      rownames_to_column(var = 'rebl_item') %>% 
      mutate(rebl_item = str_remove(rebl_item, '_'))
  
    # Join all three DFs together
    percent_eco_df %>%
      full_join(item_fit_df, by = 'rebl_item') %>%
      full_join(item_difficulty_df, by = 'rebl_item')
  })
  
  # Now save item fit stats these as 3 separate csvs.
  walk2(results$item_fit, names(results$item_fit), ~ {
    
    .x %>% 
      write_csv(file = paste0(csv_path, 'item_fit_', .y, '.csv'))
    
    cat('\n',
        paste0(csv_path, 'item_fit_', .y, '.csv'))
  })
  
  # Describe object contents
  cat('\n\nResults object contains:',
      paste(names(results), collapse = ', '),
      '\n')
  
  # Output item fit and person fit 
  return(results)
}


