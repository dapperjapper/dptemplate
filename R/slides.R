#' Beamer Format with CFPB Logo
#'
#' @export
cfpb_slides <- function(fig_caption = FALSE, ...) {
  # Get the location of the template file
  tex_file <- system.file('rmarkdown/templates/cfpb_slides/resources/template.tex',
                          package = 'dptemplate')

  # Copy the necessary graphics files
  graph_list <- c('report_bottom.png', 'cfpb_primary_logo_color_rgb.png',
                  'cfpb_report_bottom.png')
  package_dir <- system.file(package = 'dptemplate')
  for(this_graph in graph_list) {
    if (!file.exists(this_graph)) {
      old_path <- sprintf('%s/rmarkdown/templates/cfpb_slides/resources/%s',
                          package_dir,
                          this_graph)
      if (file.exists(old_path)) {
        file.copy(from = old_path, to = this_graph, overwrite = FALSE)
      } else {
        stop(sprintf('Unable to find file:  %s', old_path))
      }
    }
  }

  ret_val <- rmarkdown::beamer_presentation(...,
                                            fig_caption = fig_caption,
                                            template = tex_file)
  ret_val$inherits <- 'beamer_presentation'

  ret_val
}
