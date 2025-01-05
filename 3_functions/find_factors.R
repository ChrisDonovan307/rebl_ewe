# takes EFAset (after split_data()) and figures out how many factors to extract
pacman::p_load(psych)

find_factors <- function(EFAset, n_factors = 20) {
  
  # make all numeric
  EFAset <- EFAset |> 
    mutate_all(as.numeric)
  
  # Calculate the correlation matrix
  EFAset_cor <- cor(EFAset, use = "pairwise.complete.obs")
  
  # Then use that correlation matrix to create the scree plot
  scree(EFAset_cor, factors = FALSE)
  
  fa <- fa.parallel(EFAset)
  
  print(vss(EFAset, n_factors, rotate = "promax"))
  
}


