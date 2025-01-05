# run_np_mloef
# 2024.01.22

# this doesn't work. Maybe not worth trying to fix it.


# pretty jenky and specific function built specifically for reblprep
# must have 'full' dataset, as well as input dataset which is just REBL30 items
# it preps 


run_np_mloef <- function(df, full_df, method = 'MLoef', splitcr, split) {
  
  # Find indices for NAs from splitcr
  na_indices_from_full <- which(is.na(full_df[[splitcr]]))
  
  # indices with na in any row from df
  na_indices_from_dat <- which(apply(df, 1, function(row) any(is.na(row))))
  
  # combine indices
  indices_to_remove <- sort(c(na_indices_from_full, na_indices_from_dat))
  
  # remove indices from both dat and full
  full_no_na <- full[-indices_to_remove, ]
  dat_no_na <- df[-indices_to_remove, ]
  
  # get splitcr vector from full
  splitcriterion <- case_when(full_no_na[[splitcr]] >= split ~ 1,
                              .default = 0)
  
  # make matrix
  matrix <- as.matrix(dat_no_na[, -1])
  
  # test
  print(length(splitcriterion))
  print(dim(matrix))
  
  # run test
  return(
    NPtest(matrix, 
           method = method,
           splitcr = splitcriterion)
  )
}