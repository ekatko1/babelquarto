# Replace logical TRUE and FALSE with character "true" and "false"
# in a nested list
replace_true_false <- function(list) {
  if (is.list(list)) {
    list <- lapply(list, replace_true_false)
  } else if (is.logical(list)) {
    list <- as.character(list)
    list <- gsub("TRUE", "true", list, fixed = TRUE)
    list <- gsub("FALSE", "false", list, fixed = TRUE)
    # Set class of this character vector to 'verbatim'
    # so that yaml::write_yaml() will not add quotation marks
    class(list) <- "verbatim"
  }
  list
}

lang_code_chapter_list <- function(chapters_list, language_code) {
  chapters_list <- gsub(
    "\\.Rmd", # nolint: fixed_regex_linter
    sprintf(".%s.Rmd", language_code),
    chapters_list
  )

  gsub(
    "\\.qmd", # nolint: fixed_regex_linter
    sprintf(".%s.qmd", language_code),
    chapters_list
  )
}

trim_end <- function(config_lines) {
  # avoid more than one empty lines at the end

  if (!nzchar(utils::tail(config_lines, n = 1))) {
    reps <- rle(config_lines)[["lengths"]]

    how_many_empty <- reps[length(reps)]

    if (how_many_empty > 1) {
      config_lines <- config_lines[
        1:(length(config_lines) - how_many_empty + 1)
      ]
    }
  } else {
    config_lines <- c(config_lines, "")
  }

  return(config_lines)
}

get_config_file <- function(profile) {
  paste0("_quarto-", profile, ".yml")
}

# Return language-specific profile if it exists
lang_profile <- function(language) {
  if(file_exists( get_config_file(language) ) ) {
    return(language)
  }
  NULL
}



# Recursive merge function
merge_lists <- function(base, override) {
  merged <- base
  for (key in names(override)) {
    if (key %in% names(base) && is.list(base[[key]]) && is.list(override[[key]])) {
      # Recursively merge nested lists
      merged[[key]] <- merge_lists(base[[key]], override[[key]])
    } else {
      # Override value
      merged[[key]] <- override[[key]]
    }
  }
  return(merged)
}
