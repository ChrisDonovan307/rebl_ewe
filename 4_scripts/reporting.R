#' Reporting 
#' 2024-12-22

#' Wrangling sem outputs from original and validation surveys. Includes:
#'  - fit from main models
#'  - parameter estimates for int models
#'  - simple slopes with p values
#'  - conditional direct and indirect effects of eewe on rebl
#'  - bootstrapped CIs for direct and indirect effects of eewe on rebl



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr,
  lavaanPlot,
  crosstable,
  xtable,
  tibble,
  ggplot2,
  readxl,
  ltm
)

source('3_functions/print_xtable.R')
dat <- readRDS('6_outputs/sem_outputs.rds')
names(dat)

# Directory to save all outputs - now redundant because they are the same
figure_dir <- '../ewe_paper/'
table_dir <- '../ewe_paper/'

# List of results to save to docx
results <- list()



# SEM Outputs -------------------------------------------------------------
## Residual Matrices -------------------------------------------------------


## OG
dat$og_int_residuals$cov.z %>% 
  as.data.frame() %>% 
  round(3) %>% 
  format(nsmall = 3) %>% 
  setNames(c(names(.)[1:9], 'e1.a4', 'e3.a2', 'e5.a3', 'rebl', 'pol')) %>% 
  {.[upper.tri(.)] <- ''; .} %>%
  {rownames(.) <- names(.); .} %>% 
  rownames_to_column('var') %>% 
  print_xtable(
    paste0(table_dir, 'og_vcovz.tex'),
    caption = 'Normalized residual variance-covariance matrix for two-parameter logistic model for Study 1.',
    label = 'og_vcovz',
    font_size = 7,
    spacing = 8,
    rotate = TRUE
  )


## Val
dat$val_int_residuals$cov.z %>% 
  as.data.frame() %>% 
  round(3) %>% 
  format(nsmall = 3) %>% 
  setNames(c(names(.)[1:9], 'e1.a4', 'e5.a3', 'e3.a2', 'rebl', 'pol')) %>%
  {.[upper.tri(.)] <- ''; .} %>%
  {rownames(.) <- names(.); .} %>% 
  rownames_to_column('var') %>%
  print_xtable(
    paste0(table_dir, 'val_vcovz.tex'),
    caption = 'Normalized residual variance-covariance matrix for two-parameter logistic model for Study 2.',
    label = 'val_covz',
    font_size = 7,
    spacing = 8,
    rotate = TRUE
  ) 
 


## Effect sizes and confidence intervals -----------------------------------


### OG
dat$og_probe$og_simple_slopes %>%
  select(att) %>%
  bind_cols(
    dat$og_probe$og_indirect_effect,
    t(dat$og_cis$og_indirect_cis),
    dat$og_probe$og_total_effect,
    t(dat$og_cis$og_total_cis)
  ) %>%
  setNames(c('att', 'ie', 'ie.2.5', 'ie.97.5', 'te', 'te.2.5', 'te.97.5')) %>%
  mutate(across(c(ie:last_col()), ~ format(round(., 3), nsmall = 3))) %>% 
  print_xtable(
    path = paste0(table_dir, 'og_effects.tex'),
    label = 'og_effects',
    caption = paste(
      'Indirect and total effects of EEWE on REBL with bootstrapped confidence intervals at varying levels of ATT for Study 1.',
      'att = attribution of extreme weather events to climate change;',
      'ie/te = indirect/total effect of experience with extreme weather events on REBL Score.'
    ) 
  )

### Val
dat$val_probe$val_simple_slopes %>% 
  select(att) %>% 
  bind_cols(
    dat$val_probe$val_indirect_effect,
    t(dat$val_cis$val_indirect_cis),
    dat$val_probe$val_total_effect,
    t(dat$val_cis$val_total_cis)
  ) %>%
  setNames(c('att', 'ie', 'ie.2.5', 'ie.97.5', 'te', 'te.2.5', 'te.97.5')) %>% 
  mutate(across(c(ie:last_col()), ~ format(round(., 3), nsmall = 3))) %>% 
  print_xtable(
    path = paste0(table_dir, 'val_effects.tex'),
    digits = 0,
    label = 'val_effects',
    caption = paste(
      'Indirect and total effects of EEWE on REBL with bootstrapped confidence intervals at varying levels of ATT for Study 2.',
      'att = attribution;',
      'ie/te = indirect/total effect of experience with extreme weather events on REBL score.'
    )  
  ) 



## Simple Slopes -----------------------------------------------------------


### OG
dat$og_probe$og_simple_slopes %>% 
  mutate(across(c(est:last_col()), ~ format(round(., 3), nsmall = 3))) %>% 
  print_xtable(
    path = paste0(table_dir, 'og_slopes.tex'),
    digits = 0,
    label = 'og_slopes',
    caption = 'Simple slopes of the effect of attribution on psychological distance for Study 1.'
  ) 

### Val
dat$val_probe$val_simple_slopes %>% 
  mutate(across(c(est:last_col()), ~ format(round(., 3), nsmall = 3))) %>% 
  print_xtable(
    path = paste0(table_dir, 'val_slopes.tex'),
    digits = 0,
    label = 'val_slopes',
    caption = 'Simple slopes of the effect of attribution on psychological distance for Study 2.'
  ) 



## Model Fit ---------------------------------------------------------------


get_str(dat$og_main_fit)
dat$og_main_fit

# Make SRMR values numeric across all result files
dat <- imap(dat, ~ {
  if (str_detect(.y, ('og_main_fit|og_cfa_main_fit|val_main_fit|val_cfa_main_fit'))) {
    new_df <- .x %>% 
      mutate(srmr = as.numeric(srmr))
    return(new_df)
  } else {
    return(.x)
  }
})

# Main fits first, no CFA
bind_rows(dat$og_main_fit, dat$val_main_fit) %>%
  setNames(str_to_lower(names(.))) %>%
  select(-matches('lower|upper')) %>%
  mutate(
    across(c(chisq, chi2.df:last_col()), ~ format(round(., 3), nsmall = 3))
  ) %>%
  print_xtable(
    path = paste0(table_dir, 'fit_main_models.tex'),
    label = 'fit_main_models',
    font_size = 8,
    caption = 'Model fit for main effects models in both studies.'
  )
  
# CFA models as well
bind_rows(
  dat$og_cfa_main_fit, 
  dat$val_main_fit, 
  dat$val_cfa_main_fit
) %>%
  setNames(str_to_lower(names(.))) %>%
  select(-matches('lower|upper')) %>%
  mutate(
    model = case_when(
      model == 'validation' ~ 'validation main',
      .default = model
    ) %>% 
      str_replace('original', 'S1') %>% 
      str_replace('validation', 'S2') %>% 
      str_replace('cfa', 'CFA'),
    srmr = as.numeric(srmr),
    across(c(chisq, chi2.df:last_col()), ~ format(round(., 3), nsmall = 3))
  ) %>%
  print_xtable(
    path = paste0(table_dir, 'fit_cfa_models.tex'),
    font_size = 10,
    label = 'fit_cfa_models',
    rotate = TRUE,
    caption = 'Model fit for main effects models and CFA models in both studies.'
  )



## Parameters --------------------------------------------------------------


# NOTE: Long table captions are not working properly. Have to adjust manually, 
# So commented out for now until we want to change them. In latex table, we have
# to move the caption down below the table to right above the label.
parameter_caption <- paste(
  "Factor loadings are denoted by: =\\textasciitilde.",
  "Regressions are denoted by: \\textasciitilde. ",
  "Correlations are denoted by: \\textasciitilde\\textasciitilde.",
  "Means are denoted by: \\textasciitilde1.",
  "R-squared values are denoted by: r2."
)

### OG
# dat$og_int_parameters %>%
#   select(-label) %>%
#   mutate(
#     across(where(is.numeric), ~ format(round(., 3), nsmall = 3)),
#     across(everything(), ~ ifelse(str_detect(., 'NA'), ' ', .))
#   ) %>%
#   print_xtable(
#     path = paste0(table_dir, 'og_int_pars.tex'),
#     font_size = 10,
#     spacing = 8,
#     label = 'og_int_pars',
#     caption = paste(
#       'Model parameter estimates for the interaction model in Study 1.',
#       parameter_caption
#     ),
#     environment = 'longtable'
#   )


### Val
# dat$val_int_parameters %>%
#   select(-label) %>%
#   mutate(
#     across(where(is.numeric), ~ format(round(., 3), nsmall = 3)),
#     across(everything(), ~ ifelse(str_detect(., 'NA'), ' ', .))
#   ) %>%
#   print_xtable(
#     path = paste0(table_dir, 'val_int_pars.tex'),
#     font_size = 10,
#     spacing = 8,
#     label = 'val_int_pars',
#     caption = paste(
#       'Model parameter estimates for the interaction model in Study 2.',
#       parameter_caption
#     ),
#     environment = 'longtable'
#   )



## Diagrams ----------------------------------------------------------------


# Labels for OG and Val SEMs
label_list <- list(
  'og' = list(
    rebl = 'REBL',
    eewe1.att4 = 'e1.a4',
    eewe3.att2 = 'e3.a2',
    eewe5.att3 = 'e5.a3',
    att = 'ATT',
    eewe = 'EEWE',
    pd = 'PD',
    int = 'INT',
    pol = 'POL'
  ),
  'val' = list(
    rebl = 'REBL',
    eewe1.att4 = 'e1.a4',
    eewe5.att3 = 'e5.a3',
    eewe3.att2 = 'e3.a2',
    att = 'ATT',
    eewe = 'EEWE',
    pd = 'PD',
    int = 'INT',
    pol = 'POL'
  )
)

# Edge options - order is regression, latent, covariance
e_opts <- formatting(
  list(color = "black", penwidth = 3),
  list(color = "grey"),
  list(color = "grey"),
  type = "edge"
)

# Node options - making latent variables thicker to stand out. Latent then obs
n_opts <- formatting(
  list(
    color = "black",
    penwidth = 3,
    fontname = 'Times New Roman'
  ),
  list(color = "black", fontname = 'Times New Roman'),
  type = "node"
)

# Grab both int_models to save them
int_models <- list(
  'og' = dat$fit_og_int,
  'val' = dat$fit_val_int
)

# Name list
names <- list('og', 'val')

# For now, no covariances shown because it's messy af
pwalk(list(int_models, label_list, names), \(x, y, z) {
  lavaanPlot2(
    x,
    include = "covs",
    labels = y,
    graph_options = list(rankdir = 'TB'),
    node_options = n_opts,
    edge_options = e_opts,
    stars = c('regress', 'covs', 'latent'),
    coef_labels = TRUE
  ) %>%
    save_png(
      paste0(
        '../ewe_paper/figures/',
        z,
        '_sem_diagram.png'
      ),
    width = 1000
  )
})




## Sensitivity -------------------------------------------------------------


sens <- dat$sensitivity
get_str(sens)

# Get a DF with summary plus a percent change column
sum <- sens$sens.summary %>% 
  mutate(perc.diff = abs((model.est - mean.est.sens) / model.est * 100)) %>% 
  select(-model.pvalue) %>% 
  setNames(c('Est', 'Mean', 'Min', 'Max', 'Change')) %>% 
  mutate(across(everything(), ~ format(round(.x, 3), nsmall = 3))) %>% 
  rownames_to_column('Path')
sum

# Make table
sum %>% 
  print_xtable(
    path = paste0(table_dir, 'sensitivity.tex'),
    font_size = 10,
    spacing = 10,
    label = 'sensitivity',
    caption = paste(
      'Sensitivity analysis on Study 2 main effects model.',
      'Est is the standardized parameter estimate;',
      'Mean, Min, and Max refer to standardized parameter estimates from the sensitivity analysis;',
      'Change is the absolute value of the percent change in the parameter estimate between the original model and sensitivity model.'
    )
  )



## Question Text -----------------------------------------------------------


# Pull renamed data from REBL repo, no exclusions or cleaning, just named
raw <- readRDS('5_objects/from_rebl/all_surveys_renamed.rds')

# Just want names from surveys 2a and 2b
dat <- full_join(
  raw$survey_2a, 
  raw$survey_2b, 
  by = 'prolificID',
  relationship = 'many-to-many'
) %>% 
  select(matches('eweExpG|attributionG|psych|worr|socialNo|ccBel|enviro|^eff')) %>% 
  slice(1) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('Item', 'Text'))
get_str(dat)  

# Now add a column showing which ones ended up being used in study 2
keepers <- c(
  paste0('attributionGen', 3:5),
  paste0('eweExpGen', c(1, 3, 5)),
  paste0('psychDist', c(1, 2, 5))
)

clean <- dat %>% 
  mutate('Study 2' = case_when(
    Item %in% keepers ~ 'X',
    .default = ' '
  ), .after = Item) %>% 
  mutate(
    Item = Item %>% 
     str_replace('eweExpGen', 'eewe') %>% 
     str_replace('attributionGen', 'att') %>% 
     str_replace('psychDist', 'pd')
  )
get_str(clean)


# NOTE: commenting this out because it is a longtable again. Have to fix it
# manually every time we save it by putting caption above label

# clean %>%
#   print_xtable(
#     path = paste0(table_dir, 'indicator_text.tex'),
#     font_size = 10,
#     spacing = 10,
#     label = 'indicator_text',
#     caption = paste(
#       'Indicator item text for all indicators in Study 1 and selections of indicators for Study 2.'
#     ),
#     align = c('l', 'l', 'l', 'p{9cm}'),
#     environment = 'longtable'
#   )



# Survey Counts -----------------------------------------------------------


# NOTE: This is old - not sure if we need this

#' Need to figure out how many people WOULD have been in both 2a and 2b before
#' exclusions so we can see how many we really excluded
# surveys <- readRDS('5_objects/cleaning/all_surveys_prepped.rds')
# names(surveys)
# map(surveys, dim)

# Inner join on 2a and 2b
# test <- inner_join(surveys$survey_2a, surveys$survey_2b, by = 'prolificID')
# dim(test)
# 1098



# Demographic Table -------------------------------------------------------
## Joins -------------------------------------------------------------------



### Survey 2
s2 <- readRDS('2_clean/full.rds')
get_str(s2)

s2_demos <- s2 %>% 
  select(age, gender, education, income, politics, race)

### Survey 3
s3 <- readRDS('2_clean/clean_surveys.rds')[['survey_3']]
get_str(s3)

s3_demos <- s3 %>% 
  select(age, gender, education, income, politics, race)
get_str(s3_demos)

### Put them together and recode together
demos <- map2(list(s2_demos, s3_demos), c('Study 1', 'Study 2'), ~ {
  .x %>% 
    mutate(
      age = case_when(
        age == 21.0 ~ '18 to 24',
        age == 29.5 ~ '25 to 34',
        age == 39.5 ~ '35 to 44',
        age == 49.5 ~ '45 to 54',
        age == 59.5 ~ '55 to 64',
        age == 69.5 ~ '65 to 74',
        age == 75.0 ~ '75 or older',
        .default = NA
      ),
      gender = case_when(
        gender == 1 ~ 'Female',
        gender == 0 ~ 'Male',
        .default = NA
      ),
      education = case_when(
        education == 1 ~ 'Bachelor\'s Degree',
        education == 0 ~ 'No Bachelor\'s Degree',
        .default = NA
      ),
      income = case_when(
        income == 12500 ~ 'Less than $25,000',
        income == 37500 ~ 'Between $25,000 and $50,000',
        income == 75000 ~ 'Between $50,000 and $75,000',
        income == 150000 ~ 'Between $100,000 and $200,000',
        income == 200000 ~ 'More than $200,000',
        .default = NA
      ) %>% 
        factor(
          levels = c(
            'Less than $25,000',
            'Between $25,000 and $50,000',
            'Between $50,000 and $75,000',
            'Between $100,000 and $200,000',
            'More than $200,000'
          )
        ),
      race = case_when(
        race == 'hispanic' ~ 'Hispanic or Latino',
        race == 'native' ~ 'Native or Indigenous',
        race == 'two_or_more' ~ 'Two or More Races',
        .default = str_to_title(race)
      ),
      politics = factor(
        politics,
        levels = c(
          'Very liberal',
          'Liberal',
          'Somewhat liberal',
          'Moderate',
          'Somewhat conservative',
          'Conservative',
          'Very conservative'
        )
      ),
      Survey = .y
    ) %>% 
    setNames(c(names(.) %>% str_to_title))
})
map(demos, get_str)

# Now combine them for demo table
both_demos <- bind_rows(demos)
get_str(both_demos)
# Ready to go



## Table -------------------------------------------------------------------


# Function to collapse rows
collapse_demos <- function(df, variable){
  group_var <- enquo(variable)
  df %>%
    group_by(!! group_var) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
    select(-c(groupRow))
}

# Crosstable to get percentages
demos_for_table <- both_demos %>%
  crosstable(
    showNA = 'ifany',
    by = 'Survey',
    label = TRUE,
    percent_pattern = "{n} ({p_col})",
    percent_digits = 1
  ) %>% 
  select(-1) %>% 
  setNames(c('Demographic', 'Category', 'Study 1', 'Study 2')) %>% 
  collapse_demos(Demographic)
get_str(demos_for_table)

# Get indices of where to put horizontal lines when we print to split demos
relevant_demos <- unique(demos_for_table$Demographic) %>% 
  keep(~ str_length(.x) > 0)
indices <- map_dbl(relevant_demos, ~ which(demos_for_table$Demographic == .x) - 1)

# Print to latex with xtable
demos_for_table %>% 
  print_xtable(
    paste0(table_dir, 'demos.tex'),
    caption = 'Demographic table for both studies.',
    label = 'demos',
    font_size = 8,
    spacing = 8,
    rotate = FALSE,
    hlines = c(-1, indices, nrow(demos_for_table))
  )



# Rasch Outputs -----------------------------------------------------------
## REBL Item Text ----------------------------------------------------------


# Get final set of items
rebl_items <- readRDS('2_clean/rebl_items.rds')

# Get REBL Item text and filter down to the 24
text <- readRDS('2_clean/rebl_item_text.rds')
clean_text <- text %>% 
  select(rebl_item, surveys, question_text) %>% 
  mutate(rebl_item = str_remove(rebl_item, '_r$|_$')) %>% 
  filter(rebl_item %in% rebl_items)
clean_text

# Make a few edits for dupes to get from 26 to 24
cleaner_text <- clean_text %>% 
  filter(
    !(rebl_item == 'foodMeat' & surveys == '1'),
    !(rebl_item == 'socialGroup' & surveys == '2b')
  ) %>% 
  mutate(
    question_text = paste0(
      '...',
      str_split_i(question_text, '... - ', 2)
    )
    # num = row_number()
  ) %>%
  select(rebl_item, question_text) %>% 
  setNames(c('REBL Item', 'In the last week, have you...'))
cleaner_text

# Make xtable
cleaner_text %>% 
  print_xtable(
    path = paste0(table_dir, 'rebl_text.tex'),
    caption = 'REBL items and question text.',
    label = 'rebl_text',
    align = 'ccp{8cm}',
    rownames = TRUE,
    font_size = 10,
    spacing = 10
  )



## REBL Hists --------------------------------------------------------------


score_dfs <- readRDS('2_clean/rebl_score_dfs.rds')
get_str(score_dfs, 3)

# Keep only the score vars
scores <- list(
  select(score_dfs[[1]], uncon_rebl),
  select(score_dfs[[2]], uncon_rebl)
)

# Make a hist for each survey with common x axis limits
rebl_hists <- map(scores, ~ {
  .x %>% 
    as.data.frame() %>% 
    ggplot(aes(x = uncon_rebl)) +
    geom_histogram(
      binwidth = 0.3,
      color = "black",
      fill = "grey"
    ) +
    theme_classic() +
    labs(x = 'REBL Score', 
         y = 'Count') +
    theme(text = element_text(size = 12))
}) %>% 
  setNames(c('og', 'val'))

rebl_hists

iwalk(rebl_hists, ~ {
  ggsave(
    filename = paste0(figure_dir, .y, '_rebl_hist.png'),
    plot = .x,
    width = 1600,
    height = 1200,
    units = 'px',
    dpi = 300
  )
})



## REBL Parameters ---------------------------------------------------------
### Diff and Mean -----------------------------------------------------------


models <- readRDS('5_objects/models.rds')
get_str(models)

# Keep only the uncon models
models <- models$uncon_models
get_str(models)

# Get DFs of difficulty and discrimination
item_dfs <- map(models, ~ {
  df <- .x %>% 
    coef(prob = TRUE) %>% 
    as.data.frame() %>% 
    round(3) %>% 
    rownames_to_column() %>% 
    setNames(c(
      'REBL Item',
      'Difficulty',
      'Discrimination',
      'Mean'
    )) %>%
    select(-Discrimination) %>%
    arrange(Difficulty)
}) 
get_str(item_dfs)



### Item Fit ----------------------------------------------------------------


# Item fit and p values to add to model DFs
fits <- map(models, ~ {
  item_fit <- item.fit(.x)
  out <- bind_cols(
    names(item_fit$Tobs),
    item_fit$Tobs,
    item_fit$p.values
  ) %>% 
    setNames(c('REBL Item', 'Tobs', 'P')) %>% 
    mutate(
      Tobs = round(Tobs, 3),
      P = format(round(P, 3), nsmall = 3)) %>% 
    arrange(P)
})
fits



### Tables ------------------------------------------------------------------


# Combine diff and mean with fits
model_dfs <- map2(item_dfs, fits, ~ {
  inner_join(.x, .y, by = 'REBL Item')
})
model_dfs
  
walk2(model_dfs, c('og', 'val'), ~ {
  long_name <- ifelse(.y == 'og', 'Study 1', 'Study 2')
  .x %>% 
    print_xtable(
      paste0(table_dir, .y, '_rebl_pars.tex'),
      caption = paste(
        'Unconstrained Rasch model outputs for', long_name, '.',
        'Difficulty represents the estimate of the latent variable.',
        'The mean is the proportion of respondents who performed the pro-environmental behavior.',
        'The item fit is represented by Tobs, while the p-value for item fit is shown as P.',
        'The discrimination for all items is 0.812.'
      ),
      label = paste0(.y, '_rebl_pars'),
      font_size = 10,
      spacing = 10,
      digits = 3,
      rotate = FALSE
    )  
})



## LR and Mloef ------------------------------------------------------------


# models <- readRDS('5_objects/models.rds')[['uncon_models']]
# map(models, get_str)
# unidimTest(models$s3)



# Mplus  ------------------------------------------------------------------


# Reporting the parameter estimate table from Mplus validation data
# CSV was manually entered because Mplus is absurdly inconvenient


## Parameters --------------------------------------------------------------


# Load parameter estimate csv
raw_pars <- read.csv('6_outputs/2025-01-02_mplus_val_parameters.csv')

# Clean up
pars <- raw_pars %>%
  mutate(op = str_remove(op, "`"))
pars  

# Get latex table
pars %>% 
  print_xtable(
    paste0(table_dir, 'mplus_val_parameters.tex'),
    caption = paste(
      'Parameter estimates from the LMS-cat model in Mplus using the MLR estimator.',
      parameter_caption,
      'Log Likelihood = -6691.994.',
      'AIC = 13531.988.',
      'BIC = 13834.745.'
    ),
    label = 'mplus_val_parameters',
    font_size = 10,
    spacing = 8,
    rotate = FALSE
  )


# Res Cov -----------------------------------------------------------------


# Also reporting residual covariance matrix for appendix

