# EFA
# 2024-12-21

# Running EFA on split from survey 2a


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  lavaan,
  tibble,
  xtable
)

# EFA split from survey 2a
dat <- readRDS('2_clean/efa_df.rds')

# Convenience functions
source('3_functions/find_factors.R')
source('3_functions/my_kbl.R')
source('3_functions/print_xtable.R')



# EFA ---------------------------------------------------------------------


# Get just the efa questions
efa_qs <- dat %>% 
  select(matches('[a-z]\\d$'), -matches('atsp|glow'))
get_str(efa_qs)

find_factors(efa_qs)
# Scree hits 0 at 7, curves after 4 or 5
# MAP and PA say 6. Going with 6

# Run efa with 6 factors, promax rotation (oblique, allows correlations between
# factors)
fit <- efa(
  data = efa_qs,
  nfactors = 6,
  rotation = "promax"
)

fit
summary(fit)
# About same as last time, except this would lead me to want eewe125 instead
# of 135 as I did originally. Everything else is about the same.

## Save a scree plot for paper
png(
  filename = '../ewe_paper/figures/scree.png',
  width = 5,
  height = 4,
  units = 'in',
  res = 100
)
scree(efa_qs)
dev.off()



# Table -------------------------------------------------------------------


# Get clean DF from loadings
fit_df <- fit$loadings %>% 
  as.data.frame() %>% 
  rownames_to_column('item') %>% 
  mutate(
    item = item %>% 
      str_replace('enviroID', 'eid') %>% 
      str_replace('efficacy', 'eff') %>% 
      str_replace('ccBelief', 'ccb') %>% 
      str_replace('eweExpGen', 'eewe') %>% 
      str_replace('psychDist', 'pd') %>% 
      str_replace('worry', 'wor') %>% 
      str_replace('attributionGen', 'att') %>% 
      str_replace('socialNorms', 'norm')
  ) %>% 
  mutate(
    across(where(is.numeric), ~ round(.x, 3)),
    across(where(is.numeric), ~ case_when(
      abs(.x) < 0.2 ~ '',
      abs(.x) > 0.2 & abs(.) < 0.32 ~ '.',
      abs(.x) >= 0.32 ~ format(round(., 3), nsmall = 3),
      .default = 'XXX'
    ))
  ) %>% 
  setNames(c(str_to_sentence(names(.))))
fit_df

# Xtable to latex
cap <- paste(
  'Loadings from exploratory factor analysis on half the respondents from original survey (n = 405).',
  'Environmental identity = eid; efficacy = eff; climate change belief = ccb;',
  'experience with extreme weather events = eewe; attribution = att; psychological distance = pd;',
  'climate worry = wor; social norms = norm.',
  'Loadings < 0.32 are represented by dots, while loadings < 0.2 are not shown.'
)
fit_df %>% 
  print_xtable(
    '../ewe_paper/tables/efa.tex',
    caption = cap,
    font_size = 8,
    digits = 0,
    label = 'efa',
    spacing = 8
  )

clear_data()
