# run_miss_forest
# 2024.08.04

# Runs missForest algorithm for data imputation. It is set up to run using a 
# hyperparameter tuning grid rather than by choosing specific values of mtry
# and ntree. Default is to run parallel and set seed as 42 with each iteration.
# Note that default is also run with variablewise = FALSE, but if we want 
# error estimates for specific variables, we will also have to run it that as 
# TRUE, otherwise we just get aggregate estimate of error. 

# Inputs: A data frame (cannot be a tibble!) that contains missing data. All 
# vars must either be factors or numeric. No vars can have a single output (like
# a factor var with only a single type of response). 

# Outputs: A list object that contains 1) an imputed data frame and 2) an 
# estimate of error. NRMSE for continuous, PFC for factor. Can have both with
# mixed data. Use this with choose_best_runs() below to pull out the best ones
# and get a cute printout of which variables were best. Finally, use the 
# add_imputed_to_og() function to get nice clean datasets for analysis.

# NOTE: While missForest cannot run with single-outcome vars, it also struggles
# with vars that have VERY little variance or VERY high missingness. I'm not 
# quite sure what the cutoffs are, but consider this when troubleshooting.



# Packages ----------------------------------------------------------------


pacman::p_load(missForest,
               purrr,
               dplyr,
               tibble)



# run_miss_forest ---------------------------------------------------------


run_miss_forest <- function(df,
                            tuning_grid = tuning_grid,
                            verbose = TRUE,
                            variablewise = FALSE,
                            parallelize = 'variables',
                            seed = 42,
                            ...) {
  
  # Initialize results list
  results <- list()
  
  for (i in 1:nrow(tuning_grid)) {
    
    # Set seed before every run
    set.seed(seed)
    
    tic()
    
    # Run missForest using parameters from tuning grid, parallelize across vars
    imp <- missForest(df,
                      mtry = tuning_grid$mtry[i],
                      ntree = tuning_grid$ntree[i],
                      verbose = verbose,
                      variablewise = variablewise,
                      parallelize = parallelize,
                      ...)
    
    # Print a message after each iteration so we know what's happening
    cat('\n~~~~~~~ Run', i, 'of', nrow(tuning_grid), 'complete ~~~~~~~\n\n')
    
    toc()
    
    # Save imp to results list
    results[[i]] <- imp
    
  }
  
  return(results)
  
}



# choose_best_runs --------------------------------------------------------


# This is a little helper function to pull the best runs out of the list of 
# all the imputations in each survey. 

# Inputs: All imputed datasets for all three surveys. It will need to use the 
# tuning grid also to figure out what the best hyper parameters were.

# Outputs: A list of three lists, one for each survey. Each sublist has an 
# imputed dataset and the error. 

choose_best_runs <- function(imp_surveys,
                             tuning_grid = full_grid,
                             variablewise = TRUE) {
  if (variablewise == TRUE) {
  ## For variablewise -----
    
    results <- map2(imp_surveys, names(imp_surveys), ~ {
      
      # Get mean PFC and add to each output
      imp_surveys <- map(.x, \(imp_output) {
        imp_output[['mean_PFC']] <- mean(imp_output$OOBerror)
        imp_output[['mean_nonzero_PFC']] <- mean(
          imp_output$OOBerror[imp_output$OOBerror != 0]
          )
        return(imp_output)
      })
      
      # Index of which run was the best (to then pull parameter values)
      index <- which.min(map_dbl(imp_surveys, \(single_imp) single_imp$mean_PFC))
      
      # Yoink mtry and ntree based on index
      mtry <- tuning_grid$mtry[index]
      ntree <- tuning_grid$ntree[index]
      
      # Get error of best run
      error <- imp_surveys[[index]]$mean_PFC
      
      cat(
        '\nThe best run for ',
        .y,
        ' was number ',
        index,
        ' with a PFC error of ',
        error,
        '. mtry = ',
        mtry,
        ', and ntree = ',
        ntree,
        '.\n',
        sep = ''
      )
      
      # Add values of mtry and ntree to result list - not kept anywhere else
      # Otherwise we will lose which hyper parameters we used
      imp_surveys[[index]][['tuning']] <- tuning_grid[index, ]
      
      return(imp_surveys[[index]])
      
    })
    
    return(results)
    
  } else if (variablewise == FALSE) {
    
    ## For not variablewise -----
    results <- map2(imp_surveys, names(imp_surveys), ~ {
      index <- which.min(map_dbl(.x, \(single_imp) single_imp$OOBerror))
      mtry <- tuning_grid$mtry[index]
      ntree <- tuning_grid$ntree[index]
      error <- .x[[index]]$OOBerror
      
      cat(
        '\nThe best run for ',
        .y,
        ' was number ',
        index,
        ' with a PFC error of ',
        error,
        '. mtry = ',
        mtry,
        ', and ntree = ',
        ntree,
        '.\n',
        sep = ''
      )
      
      # Add values of mtry and ntree to result list - not kept anywhere else
      # Otherwise we will lose which hyper parameters we used
      .x[[index]][['tuning']] <- tuning_grid[index, ]
      
      # return(best_imputation)
      return(.x[[index]])
      
    })
    
    return(results)
    
  }
}




# add_imputed_to_og -------------------------------------------------------


# Another helper function to put the imputed data back into the original data.
# First, it removes demographics from the imputed datasets, because we don't 
# want imputed demographics. Then it takes the names from the remaining imputed
# variables and removes those from the original survey data before binding 
# columns to put them all back together. This is to be used after the 
# choose_best_runs() function to actually get our clean datasets for analysis.
# It also converts factors back to numeric and subtracts 1 because of the way 
# R indexes starting at 1. This puts them back into 0 to 1 or 0 to 6 scales

# Inputs:
  # List of surveys before any imputation (og_dfs)
  # List of best outputs from missForest algorithm, one per survey, each with
    # the ximp, OOB, and hyperparameter values

# Outputs: 
  # A list of the new, imputed surveys, and a readout of NAs before and after


add_imputed_to_og <- function(og_dfs,
                              best_imp_outputs) {
  
  # First we have to pull out just the imputed data frame from the list
  best_imp_dfs <- map(best_imp_outputs, ~ .x$ximp)

  # Remove demographics from best imputed datasets because we don't want 
  # to have imputed race or income, etc.
  imputed_vars_only <- map(best_imp_dfs, ~ {
    .x |>
      select(-any_of(
        c(
          'age',
          'gender',
          'race',
          'education',
          'rurality',
          'income',
          'children',
          'election',
          'politics'
        )
      ))
  })
  
  # Change imputed vars into numeric from factor. Factor was needed for miss
  # Forest, but we need numeric for analyses. We are also subtracting one 
  # because R is 1-indexed, so converting back to numeric turns 0s and 1s into 
  # 1s and 2s. But we want the original 0s and 1s for analysis. 
  imputed_vars_only <- map(imputed_vars_only, ~ {
    .x |>
      mutate(
        across(matches('ccStatements'), as.character),
        across(matches('^values'), ~ as.numeric(.x) - 2),
        across(where(is.factor), ~ as.numeric(.x) - 1))
  })
  
  # Record number of missing in original dataset to compare later
  og_missing <- map(og_dfs, ~ sum(is.na(.x)))
  
  # Get names of imputed variables
  imp_var_names <- map(imputed_vars_only, names)
  
  # Remove the vars from surveys that were imputed so we don't have doubles
  og_dfs <- map2(og_dfs, imp_var_names, ~ {
    .x |> 
      select(-any_of(.y))
  })
  
  # Recombine og_dfs with imputed_vars_only to get complete set
  result <- map2(og_dfs, imputed_vars_only, ~ {
    bind_cols(.x, .y)
  })
  
  # Get number of missing in result to compare to og_dfs
  result_missing <- map(result, ~ sum(is.na(.x)))
  
  # Readout about missing data before and after
  cat(
    '\nOriginal surveys had', 
    paste(og_missing, collapse = '/'),
    'NAs.\nThe new datasets have', 
    paste(result_missing, collapse = '/'),
    'NAs.\n'
  )
  
  return(result)
  
}



# Another version for EWE - impute politics -------------------------------



add_imp_to_og_keep_politics <- function(og_dfs, best_imp_outputs) {
  # First we have to pull out just the imputed data frame from the list
  best_imp_dfs <- map(best_imp_outputs, ~ .x$ximp)
  
  # Remove demographics from best imputed datasets because we don't want
  # to have imputed race or income, etc.
  imputed_vars_only <- map(best_imp_dfs, ~ {
    .x |>
      select(-any_of(
        c(
          'age',
          'gender',
          'race',
          'education',
          'rurality',
          'income',
          'children',
          'election'
        )
      ))
  })
  
  # Change imputed vars into numeric from factor. Factor was needed for miss
  # Forest, but we need numeric for analyses. We are also subtracting one
  # because R is 1-indexed, so converting back to numeric turns 0s and 1s into
  # 1s and 2s. But we want the original 0s and 1s for analysis.
  imputed_vars_only <- map(imputed_vars_only, ~ {
    .x |>
      mutate(
        across(matches('ccStatements'), as.character),
        across(matches('^values'), ~ as.numeric(.x) - 2),
        across(where(is.factor), ~ as.numeric(.x) - 1)
      )
  })
  
  # Record number of missing in original dataset to compare later
  og_missing <- map(og_dfs, ~ sum(is.na(.x)))
  
  # Get names of imputed variables
  imp_var_names <- map(imputed_vars_only, names)
  
  # Remove the vars from surveys that were imputed so we don't have doubles
  og_dfs <- map2(og_dfs, imp_var_names, ~ {
    .x |>
      select(-any_of(.y))
  })
  
  # Recombine og_dfs with imputed_vars_only to get complete set
  result <- map2(og_dfs, imputed_vars_only, ~ {
    bind_cols(.x, .y)
  })
  
  # Get number of missing in result to compare to og_dfs
  result_missing <- map(result, ~ sum(is.na(.x)))
  
  # Readout about missing data before and after
  cat(
    '\nOriginal surveys had',
    paste(og_missing, collapse = '/'),
    'NAs.\nThe new datasets have',
    paste(result_missing, collapse = '/'),
    'NAs.\n'
  )
  
  return(result)
  
}
