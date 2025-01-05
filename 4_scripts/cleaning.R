# Cleaning
# 2024-12-21

# Pull surveys from REBL repo, reduce to relevant vars, clean up names,
# split for EFA and CFA groups



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr
)

# Clean surveys before imputing
surveys_clean <- readRDS('2_clean/rebl/all_surveys_excluded.rds') %>% 
  discard(names(.) == 'survey_1')
map(surveys_clean, get_str)

# Final list of 24
rebl_items <- readRDS('2_clean/rebl/rebl_items_final.rds')



# Cleaning ----------------------------------------------------------------


## Clean up surveys
demo_vars <- c(
  'age',
  'gender',
  'race',
  'education',
  'rurality',
  'income',
  'children',
  'election',
  'politics',
  'pol',
  'BIPOC'
)

dat <- map(surveys_clean, ~ {
  df <- .x %>% 
    select(
      prolificID, 
      all_of(rebl_items),
      any_of(demo_vars),
      matches('\\d$')
    ) %>% 
    setNames(c(
      names(.) %>% 
        str_remove('_r$') %>% 
        str_remove('_$') %>% 
        str_replace('eweExpGen', 'eewe') %>% 
        str_replace('attributionGen', 'att') %>% 
        str_replace('attributionSpec', 'atsp') %>% 
        str_replace('psychDist', 'pd') %>% 
        str_replace('worry', 'worr') %>% 
        str_replace('socialNorms', 'norm') %>% 
        str_replace('ccBelief', 'ccb') %>% 
        str_replace('efficacy', 'eff') %>% 
        str_replace('enviroID', 'eid') %>% 
        str_replace('greenGlow', 'glow')
    ))
  
  if ('politics' %in% names(df)) {
    df <- df %>% 
      mutate(pol = case_when(
        str_detect(politics, '^Very liberal') ~ 1,
        str_detect(politics, '^Liberal') ~ 2,
        str_detect(politics, '^Somewhat liberal') ~ 3,
        str_detect(politics, '^Moderate') ~ 4,
        str_detect(politics, '^Somewhat conservative') ~ 5,
        str_detect(politics, '^Conservative') ~ 6,
        str_detect(politics, '^Very conservative') ~ 7,
        .default = NA
      ))
  }
  return(df)
})
map(dat, get_str)

# Save these as proper clean surveys
saveRDS(dat, '2_clean/clean_surveys.rds')


## Clean up REBL items themselves
clean_items <- rebl_items %>% 
  str_remove('_r$|_$')

# Save these
saveRDS(clean_items, '2_clean/rebl_items.rds')


## Combine surveys 2a and 2b with just factor questions and prolificID
# Later we can add their REBL scores to this.
full <- inner_join(
  dat$survey_2a, 
  dat$survey_2b, 
  by = 'prolificID',
  suffix = c('_1', '_2')
)
get_str(full)
# 810 people who took both - this is our effective sample size

# Save this
saveRDS(full, '2_clean/full.rds')



# EFA CFA Split -----------------------------------------------------------


# Split first panel in half, make efa and cfa groups
set.seed(42)
efa <- slice_sample(full, prop = 0.5)
cfa <- anti_join(full, efa, by = 'prolificID')
nrow(efa)
nrow(cfa)
# 405 each

# Save these
saveRDS(efa, '2_clean/efa_df.rds')
saveRDS(cfa, '2_clean/cfa_df.rds')


clear_data()
