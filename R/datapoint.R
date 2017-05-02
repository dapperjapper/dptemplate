#' CFPB Data Point Format
#'
#' Format for creating a CFPB Office of Research Data Point.  This will generate
#' a PDF file that is suitable for publication.
#'
#' @inheritParams bookdown::pdf_book
#' @param ... Arguments to \code{bookdown::pdf_book}
#'
#' @return R Markdown output format to pass to
#'   \code{\link[rmarkdown::render]{render}}
#' @export
datapoint <- function(...) {

  # Get the location of the template file
  tex_file <- system.file('rmarkdown/templates/datapoint/resources/template.tex',
                          package = 'dptemplate')

  ret_val <- bookdown::pdf_book(...,
                                toc = TRUE,
                                #number_sections = TRUE,
                                template = tex_file)
  ret_val$inherits <- 'pdf_book'

  ret_val$knitr$opts_chunk$highlight <- FALSE

  ret_val
}

#' CFPB Data Point Format in Word
#'
#' Format for creating a CFPB Office of Research Data Point in DOCX format. This
#' will produce a docx file that is suitable for internal review. The template
#' is set up to be run off of the exact same Rmd file as will be used to generate
#' the version of the Data Point for publication.
#'
#' @inheritParams bookdown::word_document2
#' @param ... Arguments to \code{bookdown::word_document2}
#'
#' @return R Markdown output format to pass to
#'   \code{\link[rmarkdown::render]{render}}
#' @export
datapoint_word <- function(...) {
  # Create the title page docx
  # THis file will also be used as the reference document for the rest of the word
  # document
  #create_title_page()
  #create_template_file()

  #my_envir <- parent.frame()
  #cat(as.character(my_envir$yaml_front_matter))

  rd <- file.path(getwd(), 'template_file.docx')
  #rd <- system.file('rmarkdown/templates/datapoint_word/resources/template.docx',
  #                  package = 'dptemplate')

  ret_val <- bookdown::word_document2(reference_docx = rd, ...)
  ret_val$inherits <- 'word_document2'

  ret_val$post_knit <- function(yaml_front_matter, knit_input, runtime, encoding) {
    create_title_page(yaml_front_matter, knit_input)
    create_template_file(yaml_front_matter)
  }

  ret_val$on_exit <- function() {

  }
  ret_val
}
