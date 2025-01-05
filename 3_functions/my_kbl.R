# Styling for kableExtra'

pacman::p_load(
  knitr,
  kableExtra
)

options(
  knitr.table.toprule = '\\hline\\hline',
  knitr.table.bottomrule = '\\hline\\hline'
)

# Wrapper
my_kbl <- function(df, 
                   align = 'c',
                   format = 'latex',
                   font_size = 10,
                   caption = NULL,
                   label) {
  kbl(
    df, 
    format = format,
    booktabs = TRUE,
    align = align,
    caption = caption,
    label = label,
    linesep = '',
  ) %>% 
    kable_styling(
      font_size = font_size,
      latex_options = c(
        # 'hold_position',
        'repeat_header'
      )
    )
}