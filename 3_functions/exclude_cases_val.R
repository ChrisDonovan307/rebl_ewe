# exclude_cases.R
# 2024.04.06

# Turning case exclusion into a function that can apply to any REBL survey. It
# removes the 2 rows of Qualtrics nonsense, removes survey previews, and filters
# by consent, start date, end date, and duration.

# Inputs:
  # start_date ('YYYY-MM-DD') inclusive
  # end_date ('YYYY-MM-DD') inclusive
  # bs_check_1 (variable name as character)
  # bs_check_2 (variable name as character)
  # attention_check_cutoff (integer)
  # duration_percentile (numeric, 0 to 1)
  # completion_cutoff (numeric, 0 to 1)

# Output: 
  # New filtered DF, along with a read out of how many were filtered at
  # each step. 

# Notes:
  # It requires the same variable names! Make sure they are named the same way.
  # It requires user to figure out a reasonable number of NAs for that survey.



# Required packages -------------------------------------------------------


pacman::p_load(
  dplyr,
  lubridate,
  stringr,
  tibble
)



# Function ----------------------------------------------------------------


exclude_cases_val <- function(df,
                          start_date = NULL,
                          end_date = NULL,
                          bs_check_1 = NULL,
                          bs_check_2 = NULL,
                          vegan_meat_bs_check = NULL,
                          attention_check_cutoff = NULL,
                          reasonable_na_count = NULL,
                          completion_cutoff = NULL,
                          duration_percentile = NULL)
{
  
  cat('\n~~~~~ Output from exclude_cases ~~~~~\n')
  
  # Initial Dimensions
  cat('\nInitial dimensions are',
      nrow(df),
      'rows and',
      ncol(df),
      'columns.\n')
  
  # Lose Javascript nonsense -----
  # If there is gross javascript shit in 2nd row, lose first 2 rows.
  # Legacy of Qualtrics output. First row is repeat of name, 2nd row is Java
  if(str_detect(df[2, 1], 'ImportId') == TRUE) {
    
    df <- df[-c(1:2), ]
    
    cat(
      '\n',
      nrow(df),
      ' rows remain after removing 2 rows of Qualtrics junk.\n',
      sep = ''
      )
  } else {
    
    cat('\nQualtrics Javascript junk already removed!\n')
    
  }
  
  # Previews -----
  df <- df |> 
    filter(Status != 'Survey Preview')
  
  cat(
    '\n',
    nrow(df),
    ' recruits began the study (not Survey Previews).\n',
    sep = ''
    )

  
  # Recaptcha -----
  df <- df |> 
    filter(Q_RecaptchaScore > 0.5)
  
  cat(
    '\n',
    nrow(df),
    ' participants were not bots! (Recaptcha score > 0.5).\n',
    sep = ''
  )
  
  # Consent -----
  df <- filter(df, consent == 'Yes')
  cat(
    '\n*',
    nrow(df),
    ' participants consented to the study.*\n',
    sep = ''
  )
  
  # Preserve number who consented for later
  consented <- df
  
  
  # start_date and end_date -----

  if (!is.null(start_date) & !is.null(end_date)) {
    
    # Convert to posix
    df <- df |>
      mutate(across(c(StartDate, EndDate), as_date))
    
    # Filter by start and end dates
    df <-
      filter(df, EndDate <= ymd(end_date) & StartDate >= ymd(start_date))
    
    cat(
      '\n',
      nrow(df),
      ' participants took the survey between ',
      start_date,
      ' and ',
      end_date,
      '.\n',
      sep = ''
    )
  } else {
    
    cat('\nDid not filter by date.\n')
    
  }
  
  # Save number who started to calculate percentage
  started_in_window <- nrow(df)

    
  # How many completed -----
  df <- filter(df, Finished == 'True')
  
  # Completion rate
  completed <- nrow(df)
  completion_rate <- round(completed/started_in_window, 3)
  
  cat(
    '\n',
    nrow(df),
    ' participants finished the survey, for a ',
    completion_rate,
    ' completion rate.\n',
    sep = ''
  )
  
  # Attention Checks -----
  df <- df |> 
    mutate(pass_count = rowSums(across(starts_with('attention')))) |> 
    filter(pass_count >= attention_check_cutoff)
  
  cat(
    '\n',
    nrow(df),
    ' participants passed at least ',
    attention_check_cutoff,
    ' out of the ',
    sum(str_count(names(df), 'ttention')),
    ' attention checks.\n',
    sep = ''
  )
  
  # BS Check -----
  if (!is.null(bs_check_1) & !is.null(bs_check_2)) {
    
    df <- df |> 
      filter(df[[bs_check_1]] == df[[bs_check_2]])
    
    cat(
      '\n',
      nrow(df),
      ' participants remain after foraging BS check (',
      bs_check_1,
      ' == ',
      bs_check_2,
      ').\n',
      sep = ''
    )
    
  } else {
    
    cat(
      '\nNo foraging BS check provided.\n'
    )
    
  }
  
  # Vegan Meat BS Check -----
  # Remove people who said they were vegan but ate meat
  # Also remove people who said they didn't eat meat but then said they did
  if (vegan_meat_bs_check == TRUE) {
    
    df <- df |> 
      filter(!(foodVegan_ == 1 & (foodBeef_r == 0 |
                                  foodMeat_r == 0 |
                                  foodMeatEveryDay_r == 0))) |> 
      filter(!(foodMeat_r == 1 & (foodBeef_r == 0 | 
                                  foodMeatEveryDay_r == 0)))
    
    cat(
      '\n',
      nrow(df),
      ' participants remain after filtering by vegan/meat and "no meat"/meat ',
      'BS checks.\n',
      sep = ''
    )
  } else {
    
    cat(
      '\nNo vegan/meat BS check provided.\n'
    )
    
  }
  
  
  
  # Completion -----
  if (!is.null(reasonable_na_count) & !is.null(completion_cutoff)) {
    
    # Filter by proportion complete
    df <- df %>%
      mutate(
        na_count = rowSums(
          is.na(.[, (which(names(.) == "prolificID") + 1):(ncol(.) - 1)])
          ),
        prop_complete = (ncol(.) - na_count + reasonable_na_count) / ncol(.)) %>% 
      filter(prop_complete >= completion_cutoff)
    
    cat(
      '\n',
      nrow(df),
      ' participants remain after filtering by completion (proportion complete >= ',
      completion_cutoff,
      '). This accounts for ',
      reasonable_na_count,
      ' reasonable NAs. Note we are only counting from REBL items ',
      'through end of demographics.\n',
      sep = ''
    )
    
  } else {
    cat(
      '\nNot filtering by question completion (NAs).\n'
    )
  }
  
  
  # Duration -----
  df <- df |> 
    mutate(duration = as.numeric(duration)) |> 
    filter(duration > quantile(as.numeric(consented$duration), 
                               duration_percentile,
                               na.rm = TRUE))
  
  cat(
    '\n',
    nrow(df),
    ' participants remain after removing the fastest ',
    duration_percentile * 100,
    '%. Note that this is based on the speed of those in the original, ',
    'unfiltered dataset.\n',
    sep = ''
  )
  
  
  # Final readout -----
  kept <- round(((nrow(df)/nrow(consented)) * 100), 2)
  cat(
    '\nOut of the original ',
    nrow(consented),
    ' participants (not bots, not previews, did consent), we kept ',
    nrow(df),
    ' (',
    kept,
    '%).\n\n',
    sep = ''
  )
  
  
  # Remove extraneous columns and return -----
  df <- df |>
    select(-(pass_count:last_col()))
  
  return(df)
  
}
