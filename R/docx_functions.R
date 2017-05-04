#' Create Title Page
#'
#' @import xml2
create_title_page <- function(yaml_front_matter, knit_input) {

  # Create a local copy of the title template file
  title_file <- system.file('rmarkdown/templates/datapoint_word/resources/template_title5.docx',
                            package = 'dptemplate')
  if (title_file == '') stop('Could not find template_title.docx file')

  # Copy the title template to the working directory
  in_file <- tools::file_path_sans_ext(knit_input)
  test <- file.copy(from = title_file,
                    to = sprintf('title_%s.docx', in_file),
                    overwrite = TRUE)
  if (!test) stop('Title template could not be copied.')

  rocx:::extract_from_docx(sprintf('title_%s.docx', in_file), keep_old = FALSE)

  # Replace the title and date in document.xml
  var_list <- extract_from_yaml(yaml_front_matter)

  replace_title_and_date('document.xml', var_list, paste0('title_', in_file))
  replace_title_and_date('footer2.xml', var_list, paste0('title_', in_file))

  if ('dp_author' %in% names(yaml_front_matter)) {
    author_list <- yaml_front_matter$dp_author
  } else {
    author_list <- ''
  }
  replace_authors(author_list, paste0('title_', in_file))

  change_target(paste0('title_', in_file))

  # Recreate the docx file
  rocx:::compress_to_docx(sprintf('title_%s.docx', in_file))
}

create_template_file <- function(yaml_front_matter) {
  template_file <- system.file('rmarkdown/templates/datapoint_word/resources/template2.docx',
                               package = 'dptemplate')
  if (template_file == '') stop('Could not file template.docx file')

  in_file <- 'template_file'
  test <- file.copy(from = template_file,
                    to = sprintf('%s/%s.docx',
                                 getwd(),
                                 in_file),
                    overwrite = TRUE)
  if (!test) stop('Template could not be copied.')

  #rocx:::extract_from_docx(sprintf('%s.docx', in_file), keep_old = FALSE)
  #var_list <- extract_from_yaml(yaml_front_matter)
  #replace_title_and_date('footer2.xml', var_list, in_file)

  # Recreate the docx file
  #rocx:::compress_to_docx(sprintf('%s.docx', in_file))
  invisible(NULL)
}

replace_authors <- function(name_list, in_file) {
  file_name <- sprintf('rocx_temp_%s_docx/word/document.xml',
                       in_file)
  template <- read_xml(file_name)
  my_xml <- xml_find_all(template, '//w:p')
  for (i in seq_along(my_xml)) {
    old_text <- xml_text(my_xml[i])
    if (grepl('\\$author\\$', old_text, ignore.case = TRUE)) {
      n <- length(name_list)
      for (j in seq(from = n, to = 1, by = -1)) {
        xml_add_sibling(my_xml[i], my_xml[i], where = 'after')
      }
      new_my_xml <- xml_find_all(template, '//w:p')
      for (j in seq(from = n, to = 1, by = -1)) {
        rocx:::replace_text(new_my_xml[i + j], name_list[[j]])
      }
      xml_remove(my_xml[i])
    }
  }
  write_xml(template, file_name)
}

replace_title_and_date <- function(file_name, var_list, in_file) {
  xml_path <- sprintf('rocx_temp_%s_docx/word/%s',
                      in_file,
                      file_name)
  template <- read_xml(xml_path)
  my_text <- xml_find_all(template, '//w:t')
  n <- length(my_text)
  i <- 1
  while(i <= n) {
    old_text <- xml_text(my_text[i])
    #if ('title' %in% names(var_list)) {
    #  if (grepl('\\$title\\$', old_text, ignore.case = TRUE)) {
    #    new_text <- gsub('\\$title\\$', var_list$title, old_text, ignore.case = TRUE)
    #    rocx:::replace_text(my_text[i], new_text)
    #  }
    #}
    if ('date' %in% names(var_list)) {
      if (grepl('\\$date\\$', old_text, ignore.case = TRUE))
        rocx:::replace_text(my_text[i], var_list$date)
    }
    i <- i + 1
  }
  write_xml(template, xml_path)
}

#' @importFrom magrittr "%>%"
change_target <- function(in_file) {
  xml_path <- sprintf('rocx_temp_%s_docx/word/_rels/document.xml.rels',
                      in_file)
  template <- read_xml(xml_path)
  my_xml <- xml_children(template)
  new_file <- sprintf('%s/Untitled.docx', getwd()) %>%
    gsub('/', ':', .)

  for (i in 1:length(my_xml)) {
    if (grepl('subDocument', xml_attr(my_xml[i], attr = 'Type'))) {
      if (xml_has_attr(my_xml[i], attr = 'Target'))
        xml_set_attr(my_xml[i], 'Target', new_file)
    }
  }
  write_xml(template, xml_path)
}

extract_from_yaml <- function(yaml_front_matter) {
  var_list <- list(title = '', date = '')
  if ('dp_title' %in% names(yaml_front_matter)) var_list$title <- yaml_front_matter$dp_title
  if ('dp_date' %in% names(yaml_front_matter)) {
    temp_date <- yaml_front_matter$dp_date
    if (grepl("`r (.*)`", temp_date)) {
      this_command <- gsub('`r (.*)`', '\\1', temp_date)
      try({
        temp_date <- as.character(eval(parse(text = this_command)))
      })
    }
    var_list$date <- temp_date
  }
  var_list
}
