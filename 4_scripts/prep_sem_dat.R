# Prepping SEM data
# 12-21-2024



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  semTools,
  stringr,
  purrr,
  skimr,
  readr
)

# data frames with rebl scores
dat <- readRDS('2_clean/rebl_score_dfs.rds')
map(dat, get_str)

# REBL items to remove before we save Mplus data
rebl_items <- readRDS('2_clean/rebl_items.rds')



# Product Indicators ------------------------------------------------------


# Now product indicators. Not standardizing. Just double mean center the prod
# for S2 we use att 435
dat$s2 <- indProd(
  data = dat$s2,
  var1 = c('eewe1', 'eewe5', 'eewe3'),
  var2 = c('att4', 'att3', 'att5'),
  match = TRUE,
  doubleMC = TRUE,
  namesProd = c('eewe1.att4', 'eewe5.att3', 'eewe3.att5')
)

# For S3 we use att 432
dat$s3 <- indProd(
  data = dat$s3,
  var1 = c('eewe1', 'eewe5', 'eewe3'),
  var2 = c('att4', 'att3', 'att2'),
  match = TRUE,
  doubleMC = TRUE,
  namesProd = c('eewe1.att4', 'eewe5.att3', 'eewe3.att2')
)



# Save and Clear ----------------------------------------------------------


# Save as RDS for modeling with lavaan
saveRDS(dat, '2_clean/sem_dat.rds')

# Also convert to two csv to send to Mplus, with and without headings
# Names of variables we want
vars <- paste0(
  c(
    '^att[2-4]$',
    '^eewe[1,3,5]$',
    '^pd[1,2,5]$',
    '^pol$',
    '^uncon_rebl'
  ),
  collapse = '|'
)

dat %>% 
  iwalk( ~ {
    df <- .x %>% 
      select(matches(vars)) %>% 
      select(order(colnames(.)))
    write_csv(
      df,
      paste0('2_clean/mplus_data/', .y, '_with_names.csv'),
      col_names = TRUE
    )
    write_csv(
      df,
      paste0('2_clean/mplus_data/', .y, '_no_names.csv'),
      col_names = FALSE
    )
  })

clear_data()
