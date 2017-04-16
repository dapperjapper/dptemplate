#' CFPB Data Point Format
#'
#' Format for creating a CFPB Office of Research Data Point.  This will generate
#' a PDF file that is suitable for publication.
#'
#' @inheritParams rmarkdown::pdf_document
#' @param ... Arguments to \code{rmarkdown::pdf_document}
#'
#' @return R Markdown output format to pass to
#'   \code{\link[rmarkdown:render]{render}}
#' @export
datapoint <- function(...) {

  # Get the location of the template file
  tex_file <- system.file('rmarkdown/templates/dp_article/resources/template.tex',
                          package = 'dptemplate')

  ret_val <- bookdown::pdf_book(..., toc = TRUE, template = tex_file)
  ret_val$inherits <- 'pdf_book'

  ret_val$knitr$opts_chunk$highlight <- FALSE

  ret_val
}
