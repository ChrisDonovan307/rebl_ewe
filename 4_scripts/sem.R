# SEM
# 12-21-24

# Adapted from sem_two_pl


# Packages and Data -------------------------------------------------------


pacman::p_load(
  dplyr,
  lavaan,
  semTools,
  lavaanExtra,
  stringr,
  lavaanPlot,
  DiagrammeRsvg,
  rsvg,
  purrr,
  SEMsens
)

# Load prepped sem data. Using unconstrained rasch - rename it REBL for ease
dat <- readRDS('2_clean/sem_dat.rds')
dat <- map(dat, ~ rename(.x, rebl = uncon_rebl))
map(dat, get_str)

# Rename each survey for ease below
og <- dat$s2
val <- dat$s3

# Functions to prove interaction and bootstrap confidence intervals
source('3_functions/semtools_helpers.R')
source('3_functions/nicer_fit.R')

# List for results
results <- list()



# OG ----------------------------------------------------------------------
## Main Effects ------------------------------------------------


og_main <- 
  '
# specifying measurement model portion
eewe =~ eewe1 + eewe5 + eewe3
att =~ att4 + att3 + att5
pd =~ pd2 + pd1 + pd5

# specifying structural model portion
pd ~ a*eewe + att
rebl ~ b*pd + dir*eewe

# correlation based on theory
att ~~ pol

# Indirect Effects
direct := dir
indirect := a * b
total := direct + indirect
'

fit_og_main <- sem(
  og_main,
  data = og,
  estimator = "mlr",
  meanstructure = TRUE
)

summary(fit_og_main, fit.measure = TRUE, rsquare = TRUE, std = TRUE)
lavResiduals(fit_og_main)

# For main effect model, we just want to save model fit
results$og_main_fit <- nicer_fit(fit_og_main, model.labels = 'original')



## OG CFA ---------------------------------------------------------------------


og_main_cfa <- 
  '
# Measurement Model
eewe =~ eewe1 + eewe5 + eewe3
att =~ att4 + att3 + att5
pd =~ pd2 + pd1 + pd5
'

fit_og_main_cfa <- cfa(
  og_main_cfa,
  data = og,
  estimator = "mlr",
  meanstructure = TRUE
)

summary(fit_og_main_cfa,
        fit.measure = TRUE,
        rsquare = TRUE)
lavResiduals(fit_og_main_cfa)

# Compare fit to interaction sem model
nice_fit(list(fit_og_main, fit_og_main_cfa), model.labels = c('sem', 'cfa'))
#' Shows that most of the problems in fit are coming from CFA. This is good. It 
#' means the structural part isn't the problem, which is really the part we care
#' about.


# Save just this CFA fit
results$og_cfa_main_fit <- nice_fit(list(fit_og_main, fit_og_main_cfa),
                                    model.labels = c('original main', 'original cfa')) %>% 
  rename('ci.lower' = 'rmsea.ci.lower',
         'ci.upper' = 'rmsea.ci.upper')
results$og_cfa_main_fit



## Interaction -------------------------------------------------------------


# This is where we pull estimates and outputs

og_int <- 
  '
# Measurement Model
eewe =~ eewe1 + eewe5 + eewe3
att =~ att4 + att3 + att5
pd =~ pd2 + pd1 + pd5
int =~ eewe1.att4 + eewe3.att5 + eewe5.att3

# Structural Model
pd ~ eewe + att + int
rebl ~ a*pd + b*eewe

# Correlations
att ~~ pol

# Fix covariance between interaction, predictor, and moderator to 0
# Jorgensen does this, but not Cortina... not sure
int ~~ 0*eewe + 0*att

# Set first indicator vars intercepts to 0
eewe1 + att4 + pd2 + eewe1.att4 ~ 0*1

# Allow latent var means to be freely estimated
eewe + att + int + pd ~ NA*1
'

# Robust MLM 
fit_og_int <- sem(
  og_int,
  data = og,
  estimator = "mlr",
  meanstructure = FALSE
)

# Save interaction model to results as well to make diagrams later
results$fit_og_int <- fit_og_int

summary(fit_og_int,
        fit.measure = TRUE,
        rsquare = TRUE)

# For the int model, we want parameter estimates, and vcov, but not fit indices
# get parameter estimates to report
results$og_int_parameters <- parameterEstimates(fit_og_int,
                                                ci = FALSE,
                                                standardized = FALSE,
                                                rsquare = TRUE)
results$og_int_residuals <- lavResiduals(fit_og_int)



## Slopes -----------------------------------------------------------------


# Using same interaction model specification but using ML instead of MLR
# Because slopes method is not set up for MLR
fit_og_slopes <- sem(
  og_int,
  data = og,
  estimator = 'ml',
  meanstructure = TRUE
)

summary(fit_og_slopes,
        fit.measure = TRUE,
        rsquare = TRUE)



## Probe Interaction -------------------------------------------------------


# This is simple slopes with p values, also indirect and total effect for model
# but not CIs for indirect and total effects! That is below.
results$og_probe <- probe_interaction(
  slope_fit = fit_og_slopes,
  int_fit = fit_og_int,
  which_survey = 'og',
  folder = '../ewe_paper/figures/'
)



## Bootstrap CIs -----------------------------------------------------------


results$og_cis <- bootstrap_cis(
  fit_og_slopes,
  fit_og_int,
  which_survey = 'og',
  n_boot = 1000,
  seed = 42,
  pd_to_rebl = 'a',
  eewe_to_rebl = 'b',
  group = NA
)

get_str(results$og_cis)
results$og_cis



# Val ----------------------------------------------------------------------
## Main Effects  -----------------------------------------------------------


val_main <- 
  '
# Measurement Model
eewe =~ eewe1 + eewe5 + eewe3
att =~ att4 + att3 + att2
pd =~ pd2 + pd1 + pd5

# specifying structural model portion
pd ~ eewe + att
rebl ~ pd + eewe

# correlation based on theory
att ~~ pol
'

fit_val_main <- sem(
  val_main,
  data = val,
  estimator = "mlr",
  meanstructure = FALSE
)

summary(fit_val_main, fit.measure = TRUE, rsquare = TRUE)
lavResiduals(fit_val_main)

# Save model fit only
results$val_main_fit <- nicer_fit(fit_val_main, model.labels = 'validation')



## Val CFA -----------------------------------------------------------------


val_main_cfa <- 
  '
# Measurement Model
eewe =~ eewe1 + eewe5 + eewe3
att =~ att4 + att3 + att2
pd =~ pd2 + pd1 + pd5
'

fit_val_main_cfa <- sem(
  val_main_cfa,
  data = val,
  estimator = "ml",
  meanstructure = FALSE
)

summary(fit_val_main_cfa, fit.measure = TRUE, rsquare = TRUE)

# Save model fit only
results$val_cfa_main_fit <- nicer_fit(fit_val_main_cfa, 
                                      model.labels = 'validation cfa') %>% 
  select(-matches('ci.'))
results$val_cfa_main_fit



## Interaction -------------------------------------------------------------


# This is where we pull estimates and outputs

val_int <- 
  '
# Measurement Model
eewe =~ eewe1 + eewe5 + eewe3
att =~ att4 + att3 + att2
pd =~ pd2 + pd1 + pd5
int =~ eewe1.att4 + eewe5.att3 + eewe3.att2

# Structural Model
pd ~ eewe + att + int
rebl ~ a*pd + b*eewe

# Correlations 
att ~~ pol

# Fix covariance between interaction, predictor, and moderator to 0
# Jorgensen does this, but not Cortina... not sure
int ~~ 0*eewe + 0*att

# Set first indicator vars intercepts to 0
eewe1 + att4 + pd2 + eewe1.att4 ~ 0*1

# Allow latent var means to be freely estimated
eewe + att + int + pd ~ NA*1
'

# Robust MLM 
fit_val_int <- sem(
  val_int,
  data = val,
  estimator = "mlr",
  meanstructure = FALSE
)

summary(fit_val_int,
        fit.measure = TRUE,
        rsquare = TRUE)

results$fit_val_int <- fit_val_int

# Parameters and residual vcov
results$val_int_parameters <- parameterEstimates(fit_val_int,
                                                 ci = FALSE,
                                                 standardized = FALSE,
                                                 rsquare = TRUE)
results$val_int_residuals <- lavResiduals(fit_val_int)



## Slopes -----------------------------------------------------------------


# Same as interaction syntax, but with ML instead of MLR
fit_val_slopes <- sem(
  val_int,
  data = val,
  estimator = 'ml',
  meanstructure = TRUE
)

summary(fit_val_slopes,
        fit.measure = TRUE,
        rsquare = TRUE)



## Probe Interaction -------------------------------------------------------


# This is simple slopes with p values, also indirect and total effect for model
# but not CIs for indirect and total effects! That is below.
results$val_probe <- probe_interaction(
  slope_fit = fit_val_slopes,
  int_fit = fit_val_int,
  which_survey = 'val',
  folder = '../ewe_paper/figures/'
)



## Bootstrap CIs -----------------------------------------------------------


results$val_cis <- bootstrap_cis(
  fit_val_slopes,
  fit_val_int,
  which_survey = 'val',
  n_boot = 1000,
  seed = 42,
  pd_to_rebl = 'a',
  eewe_to_rebl = 'b',
  group = NA
)



# LRT ---------------------------------------------------------------------


# manual LRT to compare two Mplus models
# https://stats.oarc.ucla.edu/mplus/faq/how-can-i-compute-a-chi-square-test-for-nested-models-with-the-mlr-or-mlm-estimators/

# log likelihoods for null (no int) and restricted (with int)
loglik_0 <- -6692.226
loglik_1 <- -6691.994

# scaling correction factors
cor_0 <- 1.0209
cor_1 <- 1.0218

# parameters
par_0 <- 73
par_1 <- 74

cd = (par_0 * cor_0 - par_1 * cor_1) / (par_0 - par_1)
TRd = -2 * (loglik_0 - loglik_1) / cd
df <- par_1 - par_0

1 - pchisq(TRd, df = df)
# p = 0.514
# Not significantly different



# Power -------------------------------------------------------------------
## For RMSEA ---------------------------------------------------------------


#Computation of minimum sample size for test of fit

rmsea0 <- 0.05 #null hypothesized RMSEA
rmseaa <- 0.03 #alternative hypothesized RMSEA
d <- 72 #degrees of freedom
alpha <- 0.05 #alpha level
desired <- 0.8 #desired power

#Code below need not be changed by user
#initialize values
pow <- 0.0
n <- 0
#begin loop for finding initial level of n
while (pow<desired) {
  n <- n+100
  ncp0 <- (n-1)*d*rmsea0^2
  ncpa <- (n-1)*d*rmseaa^2
  #compute power
  if(rmsea0<rmseaa) {
    cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
    pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  else {
    cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
    pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
}

#begin loop for interval halving
foo <- -1
newn <- n
interval <- 200
powdiff <- pow - desired
while (powdiff>.001) {
  interval <- interval*.5
  newn <- newn + foo*interval*.5
  ncp0 <- (newn-1)*d*rmsea0^2
  ncpa <- (newn-1)*d*rmseaa^2
  #compute power
  if(rmsea0<rmseaa) {
    cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
    pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  else {
    cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
    pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  powdiff <- abs(pow-desired)
  if (pow<desired) {
    foo <- 1
  }
  if (pow>desired) {
    foo <- -1
  }
}

minn <- newn
print(minn)
# 396 needed for power of 0.08 to detect RMSEA of 0.3 compared to 0.5 null



## Sensitivity -------------------------------------------------------------


# Sensitivity analysis for main effects model
val_main <- 
  '
# Measurement Model
eewe =~ eewe1 + eewe5 + eewe3
att =~ att4 + att3 + att2
pd =~ pd2 + pd1 + pd5

# specifying structural model portion
pd ~ eewe + att
rebl ~ pd + eewe

# correlation based on theory
att ~~ pol
'

val_main_sens <- 
  '
# Measurement Model
eewe =~ eewe1 + eewe5 + eewe3
att =~ att4 + att3 + att2
pd =~ pd2 + pd1 + pd5

# specifying structural model portion
pd ~ eewe + att
rebl ~ pd + eewe

# correlation based on theory
att ~~ pol

# Phantom variables
eewe ~ phantom1*phantom
att ~ phantom2*phantom
pd ~ phantom3*phantom
rebl ~ phantom4*phantom
pol ~ phantom5*phantom

# Set mean and variance of phantom
phantom =~ 0
phantom ~~ 1*phantom
'

# Set paths of interest
paths <- '
pd ~ eewe + att
rebl ~ eewe + pd
'

my.sa <- sa.aco(
  val,
  model = val_main,
  sens.model = val_main_sens,
  paths = paths,
  opt.fun = 1, # optimization is difference between old est and sens est
  seed = 42,
  estimator = 'MLR'
)

(tables <- sens.tables(my.sa))

# Get % change in estimates. Over 10% means it is sensitive (Kolenikov 2011)
perc_diff <- tables$sens.summary %>% 
  mutate(perc_diff = abs((mean.est.sens - model.est) / model.est * 100))
perc_diff
# Shows that rebl~eewe is sensitive, other paths are not

# Add to results to save
results$sensitivity <- tables



# Save and Clear ----------------------------------------------------------


names(results)
get_str(results)

# Save outputs so we can build diagrams and tables later
saveRDS(results, '6_outputs/sem_outputs.rds')

clear_data()


