pacman::p_load(
  dplyr,
  xtable
)


print_xtable <- function(df, 
                         path, 
                         label,
                         digits = 0,
                         font_size = 10,
                         spacing = 10,
                         rownames = FALSE,
                         rotate = FALSE,
                         hlines = NULL,
                         align = NULL,
                         environment = 'tabular',
                         caption = NULL) {
  # Convert DF to xtable
  xt <- xtable(
    df,
    digits = digits,
    label = label,
    align = align
  )
  
  # Open the file for writing
  file_conn <- file(path, "w")
  
  # Start the table environment
  if (environment != 'longtable') {
    writeLines("\\begin{table}[h!]\n\\centering", file_conn)
    
    # Add rotatebox for rotation if needed
    if (rotate) {
      writeLines("\\rotatebox{90}{", file_conn)
    }
  }
  
  # If longtable and caption, put it on top instead of bottom
  if (!is.null(caption) & environment == 'longtable') {
    caption_output <- paste0("\\caption{", caption, "} \\")
    writeLines(caption_output, file_conn)
  }
  
  # If longtable, add horizontal lines at top manually
  if (environment == 'longtable') {
    # hlines <- c(-1, 0, nrow(df))
    hlines <- c(-1, 0)
  }

  # Capture the xtable content with custom font size and spacing
  if (environment == 'longtable' | !is.null(hlines)) {
    cat('Either longtable OR hlines')
    xtable_content <- capture.output(
      print(
        xt, 
        include.rownames = rownames,
        hline.after = hlines,
        tabular.environment = environment,
        size = paste0("\\fontsize{", font_size, "pt}{", spacing, "pt}\\selectfont"),
        floating = FALSE
      )
    )
  } else if (environment == 'tabular' & is.null(hlines)) {
    xtable_content <- capture.output(
      print(
        xt, 
        include.rownames = rownames,
        tabular.environment = environment,
        size = paste0("\\fontsize{", font_size, "pt}{", spacing, "pt}\\selectfont"),
        floating = FALSE
      )
    )
  } else if (environment == 'tabular' & !is.null(hlines)) {
    print('Tabular output with hlines')
    xtable_content <- capture.output(
      print(
        xt, 
        include.rownames = rownames,
        hline.after = hlines,
        tabular.environment = environment,
        size = paste0("\\fontsize{", font_size, "pt}{", spacing, "pt}\\selectfont"),
        floating = FALSE
      )
    )
  } else {
    stop('Error with environment/hlines ifelse statements.')
  }
  
  
  # Write the xtable content
  writeLines(xtable_content, file_conn)
  
  # Close rotate box environment
  if (rotate) {
    writeLines("}", file_conn)
  }
  
  # Add caption at bottom if included
  if (!is.null(caption) & environment != 'longtable') {
    caption_output <- paste0("\\caption{", caption, "}")
    writeLines(caption_output, file_conn)
  }
  
  # Add label
  if (environment != 'longtable') {
    label_output <- paste0('\\label{', label, '}')
    writeLines(label_output, file_conn)
  }
  
  # Close the table environment
  if (environment != 'longtable') {
    writeLines("\\end{table}", file_conn)
  }
  
  # Close the file connection
  close(file_conn)
}


