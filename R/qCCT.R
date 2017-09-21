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
qCCT <- function(...) {

  # Get the location of the template file
  tex_file <- system.file('rmarkdown/templates/qCCT/resources/template.tex',
                          package = 'dptemplate')

  # Copy the necessary graphics files
  graph_list <- c('cfpblogo_wide.png',
                  'dataPointCoverBackground.png',
                  'cfpb_logo.png',
                  'cfpb_report_bottom.png')
  package_dir <- system.file(package = 'dptemplate')
  for(this_graph in graph_list) {
    if (!file.exists(this_graph)) {
      old_path <- sprintf('%s/rmarkdown/templates/qCCT/skeleton/%s',
                          package_dir,
                          this_graph)
      if (file.exists(old_path)) {
        file.copy(from = old_path, to = this_graph, overwrite = FALSE)
      } else {
        stop(paste0('Unable to find file:  %s', old_path))
      }
    }
  }

  ret_val <- bookdown::pdf_book(...,
                                toc = TRUE,
                                citation_package = 'biblatex',
                                #citation_package = 'none',
                                #number_sections = TRUE,
                                template = tex_file,
                                latex_engine = 'pdflatex')
  ret_val$inherits <- 'pdf_book'

  ret_val$knitr$opts_chunk$highlight <- FALSE

  ret_val
}
