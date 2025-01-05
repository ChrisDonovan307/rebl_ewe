vis_mis_by_demo <- function(df, var){
  
  # enquo for some reason
  var <- enquo(var)
  # education
  a <- ggplot(df, 
         aes(x = !!var, 
             y = education, 
             color = education_NA)) + 
    geom_jitter()
  
  # race
  b <- ggplot(df, 
         aes(x = !!var, 
             y = race, 
             color = race_NA)) + 
    geom_jitter()
  
  # gender
  c <- ggplot(df, 
         aes(x = !!var,
             y = gender, 
             color = gender_NA)) + 
    geom_jitter()
  
  # income
  d <- ggplot(df, 
         aes(x = !!var,
             y = income, 
             color = income_NA)) + 
    geom_jitter()
  
  plots <- list(a, b, c, d)
  plots
}