# Impute and Rasch modeling
# 2024-12-21

# Making DFs for both survey 2 (REBL from 2a, factor questions from both) and 
# survey 3. They need ID, 24 REBL items, selected eewe/pd/att items, REBL
# scores, and also product indicators.

# Sending only survey 2 through the wringer, then run validation set with that
# model. Only imputing REBL items.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr,
  ltm,
  eRm,
  missForest,
  skimr
)

source('3_functions/run_miss_forest.R')

# Surveys 2 and 3
dat <- list(
  s2 = readRDS('2_clean/cfa_df.rds'),
  s3 = readRDS('2_clean/clean_surveys.rds')[['survey_3']]
)
map(dat, get_str)

# All clean REBL items
rebl_items <- readRDS('2_clean/rebl_items.rds')

# Initialize results list
results <- list()



# Clean -------------------------------------------------------------------


# Reduce surveys to relevant columns only
dat <- map(dat, ~ {
  .x %>% 
    setNames(c(str_remove(names(.), '_1$'))) %>% 
    select(
      prolificID,
      all_of(rebl_items),
      pol,
      matches('eewe[1,3,5]|att[1-5]|pd[1,2,5]')
    )
})
map(dat, get_str)
# We will use these again later to append REBL scores and get product indicators

# Check missing data - for reporting in paper
map(dat, ~ {
  df <- select(.x, -prolificID)
  mean(is.na(df)) * 100
})
# 0.021% missing in s2, 0.019% missing in s3

# Now get data with only rebl items so we can run models
to_impute <- map(dat, ~ {
  .x %>% 
    select(prolificID, all_of(rebl_items)) %>% 
    column_to_rownames('prolificID') %>% 
    mutate(across(everything(), ~ as.factor(.x)))
})
map(to_impute, get_str)
map(to_impute, rownames)



# Imputation --------------------------------------------------------------


# Tuning Grid
tuning_grid <- expand.grid(
  mtry = c(7, 10, 15),
  ntree = c(100, 250, 500)
)

all_runs <- imap(to_impute, \(df, name) {
  get_time(paste('Starting', name, 'at'))
  map(1:nrow(tuning_grid), \(row){
    set.seed(42)
    missForest(
      xmis = df,
      mtry = tuning_grid[row, 'mtry'],
      ntree = tuning_grid[row, 'ntree'],
      variablewise = TRUE
    )
  })
})
get_str(all_runs)

best_runs <- choose_best_runs(all_runs, tuning_grid = tuning_grid)
get_str(best_runs)

# Save best runs so we can get imputation stats
saveRDS(best_runs, '5_objects/rebl_imputations_best_runs.R')

# Check imputation error
map(best_runs, ~ .x$mean_nonzero_PFC)
# $s2
# [1] 0.1971947
# $s3
# [1] 0.18065

# Pull out those two best DFs so we can run Rasch models
rebl_imp <- map(best_runs, ~ {
  .x$ximp %>% 
    mutate(across(everything(), ~ as.numeric(.x) - 1))
})
get_str(rebl_imp)
map(rebl_imp, rownames)

# Save this as intermediate object
saveRDS(rebl_imp, '5_objects/rebl_imputations.R')



# Rasch Modeling ----------------------------------------------------------

# Load imputations from above so we don't have to run again
rebl_imp <- readRDS('5_objects/rebl_imputations.R')


## CML Rasch ---------------------------------------------------------------


fit0 <- RM(rebl_imp$s2)
summary(fit0)

(lr <- LRtest(fit0))
# Passes invariance
# Andersen LR-test: 
# LR-value: 29.057 
# Chi-square df: 23 
# p-value:  0.178 

fit0 %>% 
  person.parameter() %>% 
  gofIRT()
# Goodness-of-Fit Results:
# Collapsed Deviance = 346.825 (df = 408, p-value = 0.987)
# Pearson R2: 0.491
# Area Under ROC: 0.905

plotGOF(lr)



### Check -------------------------------------------------------------------


percent_eco_df <- rebl_imp$s2 %>%
  select(any_of(rebl_items)) %>%
  colMeans(na.rm = TRUE) %>%
  as.data.frame() %>%
  setNames('percent_eco') %>%
  rownames_to_column(var = 'rebl_item')

# Get DF of most item fit statistics
item_fit_df <- fit0 %>%
  person.parameter() %>%
  itemfit() %>% 
  .[-3] %>% 
  map(as.vector) %>%
  as.data.frame() %>% 
  mutate(rebl_item = percent_eco_df$rebl_item)

#' Get DF of item difficulties. There is one less than item fit, I think
#' because they are all relative to the first?
item_difficulty_df <- data.frame(eta = fit0$etapar,
                                 eta_se = fit0$se.eta) %>%
  rownames_to_column(var = 'rebl_item') %>% 
  mutate(rebl_item = str_remove(rebl_item, '_'))

# Join all three DFs together
percent_eco_df %>%
  full_join(item_fit_df, by = 'rebl_item') %>%
  full_join(item_difficulty_df, by = 'rebl_item')



## MML Constrained ---------------------------------------------------------


fit1 <- rasch(rebl_imp$s2, constraint = cbind(length(rebl_imp$s2) + 1, 1))
summary(fit1)
coef(fit1)
margins(fit1)

# Goodness-of-Fit using Bootstrap
set.seed(42)
# GoF.rasch(fit1)
# Bootstrap Goodness-of-Fit using Pearson chi-squared
# Call:
#   rasch(data = rebl_imp$s2, constraint = cbind(length(rebl_imp$s2) +     1, 1))
# Tobs: 19728667 
# # data-sets: 50 
# p-value: 0.04 

item.fit(fit1)$p.values %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('REBL Item', 'P')) %>% 
  arrange(P)



## MML Unconstrained -------------------------------------------------------


fit2 <- rasch(rebl_imp$s2)
summary(fit2)
margins(fit2)
coef(fit2)

set.seed(42)
GoF.rasch(fit2)
# Bootstrap Goodness-of-Fit using Pearson chi-squared
# Call:
#   rasch(data = rebl_imp$s2)
# Tobs: 17323267 
# # data-sets: 50 
# p-value: 0.06 

item.fit(fit2)$p.values %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('REBL Item', 'P')) %>% 
  arrange(P)

item.fit(fit2)$p.values %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('REBL Item', 'P')) %>% 
  arrange(P)

# Compare unconstrained to constrained
anova(fit1, fit2)
# Likelihood Ratio Table
# AIC     BIC  log.Lik   LRT df p.value
# fit1 8032.60 8128.69 -3992.30                 
# fit2 8021.03 8121.12 -3985.51 13.57  1  <0.001

# Unconstrained is better than than constrained
# Discrimination is not 1, it is 0.8119



## MML 2PL -----------------------------------------------------------------


# Two parameter logistic
fit3 <- ltm(rebl_imp$s2 ~ z1)
summary(fit3)
margins(fit3)
coef(fit3)
# Discrimination varies

item.fit(fit3)$p.values %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('REBL Item', 'P')) %>% 
  arrange(P)
# Worse, more than half misfitting

# Check unconstrained Rasch vs 2PL
anova(fit2, fit3)
# Fits worse, but not significantly different than unconstrained Rasch
# Fewer big residuals though

# Try two latest parameters
fit3.5 <- ltm(rebl_imp$s2 ~ z1 * z2)
summary(fit3.5)
margins(fit3.5)
coef(fit3.5)

# Compare
anova(fit2, fit3.5)
anova(fit3, fit3.5)
# Significantly better than unconstrained and 2PL with one latent variable
# Margins still show some problematic items though - but fewer than others

fit3.75 <- ltm(rebl_imp$s2 ~ z1 + z2)
summary(fit3.75)
margins(fit3.75)
coef(fit3.75)

# Compare
anova(fit2, fit3.75)
anova(fit3, fit3.75)
# Fits better than unconstrained and tpl with 1 latent var
# This makes no theoretical sense though - what would the other latent var be?

# anova(fit3.5, fit3.75) # can't compare - not nested



## MML TPM -----------------------------------------------------------------


fit4 <- tpm(rebl_imp$s2, type = "rasch", max.guessing = 1)
summary(fit4)
coef(fit4)
# discrimination is set to 1.02 for all items, but difficulty and guessing vary

item.fit(fit4)$p.values %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('REBL Item', 'P')) %>% 
  mutate(P = round(P, 3)) %>% 
  arrange(P)
# About half of items are misfitting

# Compare to unconstrained Rasch without guessing
anova(fit2, fit4)
# Looks like fit is a bit worse, but not significantly different from 2PL

# Try different difficulty parameters - estimating rebl, guessing, discrim
set.seed(42)
fit5 <- tpm(rebl_imp$s2, type = "latent.trait", start.val = 'random')
summary(fit5)

get_str(factor.scores(fit5))


item.fit(fit5)$p.values %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('REBL Item', 'P')) %>% 
  mutate(P = round(P, 3)) %>% 
  arrange(P)

anova(fit4, fit5)
# Not significantly different, but fit4 is marginally better



# REBL Scores -------------------------------------------------------------
## tpl ---------------------------------------------------------------------


# Using two parameter logistic model to use as REBL scores
# Now we want them for both datasets
models <- map(rebl_imp, ~ ltm(.x ~ z1))
map(models, summary)
map(models, coef)
get_str(models[[1]])

# Save models
results$tpl_models <- models

# Get scores
tpl_factor_scores <- map(models, ltm::factor.scores)

# Test function
tpl_scores <- map2(rebl_imp, tpl_factor_scores, ~ {
  fix_lumped_scores(.x, .y, rebl_items) %>% 
    rename(
      tpl_exp = Exp,
      tpl_rebl = z1,
      tpl_se = se.z1
    )
})
get_str(tpl_scores)



## mml unconstrained -------------------------------------------------------


uncon <- map(rebl_imp, ~ rasch(.x))
get_str(uncon)

# Save models
results$uncon_models <- uncon

# Get scores
uncon_factor_scores <- map(uncon, ~ ltm::factor.scores(.x))
get_str(uncon_factor_scores)

# Map it
uncon_scores <- map2(rebl_imp, uncon_factor_scores, ~ {
  fix_lumped_scores(.x, .y, rebl_items) %>% 
    rename(
      uncon_exp = Exp,
      uncon_rebl = z1,
      uncon_se = se.z1
    )
})
get_str(uncon_scores)



## Combine -----------------------------------------------------------------


# Combining tpl and uncon scores together into one DF
scores_df <- pmap(list(dat, tpl_scores, uncon_scores), \(x, y, z) {
  inner_join(x, y, by = 'prolificID') %>% 
    inner_join(z, by = 'prolificID')
})
map(scores_df, get_str)
# This has all survey data, tpl scores, and uncon scores



# Save --------------------------------------------------------------------


# Models and score DFs
saveRDS(results, '5_objects/models.rds')
saveRDS(scores_df, '2_clean/rebl_score_dfs.rds')
clear_data()

