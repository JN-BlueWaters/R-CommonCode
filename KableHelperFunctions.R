# These are some kable package wrapper functions for my preferred styling
# 

options(kable_styling_bootstrap_options=c('striped', 'hover', 'condensed'),
        kable_styling_full_width = FALSE,
        kable_styling_position = 'left',
        knitr.kable.NA = '')

kable_default <- function(df, 
                          format = 'html', 
                          escape = TRUE,
                          digits = 0,
                          row.names = NA,
                          col.names = NA,
                          col.width = NA,
                          caption = NULL,
                          format.args = list(decimal.mark = '.', 
                                             big.mark = ',', 
                                             scientific=FALSE),
                          align = 'r',
                          full_width = FALSE,
                          font_size = 10,
                          fixed_thead = TRUE,
                          position = 'left',
                          bootstrap_options = c('striped', 'hover', 'condensed', 'responsive'),
                          .header = TRUE,
                          .summary = 0) {
  kbl <- kable(df, format = format, escape = escape, 
               digits = digits,
               row.names = row.names,
               col.names = col.names,
               caption = caption,
               format.args = format.args,
               align = align) %>%
    kable_styling(bootstrap_options = bootstrap_options, 
                  full_width = full_width,
                  font_size = font_size,
                  fixed_thead = fixed_thead,
                  position = position) %>%
    column_spec(which(!is.na(col.width)), width = col.width[!is.na(col.width)])

    if(.header) {
    kbl <- kbl %>% kable_PaintHeader()
  }
  
  if(.summary!=0) {
    kbl <- kbl %>% kable_PaintSummary(.lastrows = .summary)
  }
  return(kbl)
}

kable_PaintHeader <- function(kbl) {
  kbl %>%
    row_spec(row = 0, 
             bold = TRUE, 
             background = 'black',
             color = 'white',
             extra_css = 'border-bottom: 3px solid; border-color: black;')
}

kable_PaintSummary <- function(kbl, .lastrows=1) {
  TotRows <- sum(gregexpr('<tr>', kbl)[[1]]>0) - 1
  # how many <tr>'s are there in the kable object less 1 for the header
  kbl %>%
    row_spec(row = (TotRows-.lastrows+1):TotRows, 
             bold = TRUE,
             background = '#abbac7') %>% # Cool Gray 2 
    row_spec(row = TotRows-.lastrows+1, 
             extra_css = 'border-top: 3px solid') 
}
