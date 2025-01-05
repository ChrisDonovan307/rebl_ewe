# Fix Lumped Scores
# 2024-12-27


# Description -------------------------------------------------------------

#' Outputs of ltm::factor.scores are provided for each unique set of responses,
#' which means respondents are lumped and need to be put back together to 
#' match every respondent with a score. This function takes a DF of responses
#' and a DF of scores and spits out a proper DF with IDs and scores for each
#' respondent.


# Dependencies ------------------------------------------------------------

pacman::p_load(
  dplyr,
  ltm
)


# Function ----------------------------------------------------------------


fix_lumped_scores <- function(df, scores, rebl_items) {
  
  # Create unique identifier in the original df
  df_id <- df %>% 
    rownames_to_column('prolificID') %>% 
    mutate(
      id = apply(.[, c(rebl_items)], 1, paste, collapse = "_")
    )
  
  # Create unique identifier in the model output df
  model_id <- scores$score.dat %>% 
    mutate(
      id = apply(.[, rebl_items], 1, paste, collapse = "_")
    )
  
  # Put the back together, just keep prolific ID and rebl scores
  rebl_scores <- merge(df_id, model_id, by = 'id', all.x = TRUE) %>% 
    select(prolificID, Exp:last_col(), -id)
  
  return(rebl_scores)
}
