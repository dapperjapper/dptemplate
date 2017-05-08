#' Data Point Kable
#'
#' This function overrides the knitr::kable function. For LaTeX documents,
#' it adds code to color the top row of the resulting tables. For other
#' document types, this function is equivalent to knitr::kable.
#' @export
dp_kable <- function(x,
                     format = NULL,
                     digits = getOption('digits'),
                     row.names = NA,
                     align = NULL,
                     caption = NULL,
                     format.args = list(),
                     escape = TRUE,
                     booktabs = TRUE,
                     latex_options,
                     ...) {

  kable_val <- knitr::kable(x = x,
                            format = format,
                            digits = digits,
                            row.names = row.names,
                            align = align,
                            caption = caption,
                            format.args = format.args,
                            escape = escape,
                            booktabs = booktabs,
                            ...)

  if (!missing(latex_options))
    kable_val <- kable_styling(kable_val, latex_options = latex_options)

  if (attr(kable_val, 'format') == 'latex') {
    my_text <- kable_val %>%
      gsub('\\\\hline', '\\\\arrayrulecolor{white}\\\\hline', .) %>%
      gsub('\\\\toprule', '', .) %>%
      gsub('\\\\midrule', '', .) %>%
      gsub('\\\\bottomrule', '', .) %>%
      strsplit('\n') %>%
      unlist()

    my_pos <- my_text %>%
      grepl('\\\\begin\\{tabular\\}', .) %>%
      which()

    my_text[my_pos] <- paste0(my_text[my_pos], '\n\\rowcolor{greenhead}')
    ret_val <- paste(my_text, collapse = '\n')

    class(ret_val) <- 'knitr_kable'
    attr(ret_val, 'format') <- 'latex'
  } else {
    ret_val <- kable_val
  }
  ret_val
}
