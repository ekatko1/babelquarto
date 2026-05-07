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

get_config_path <- function(profile) {
  paste0("_quarto-", profile, ".yml")
}

# Return language-specific profile if it exists
lang_profile <- function(language) {
  if(file_exists( get_config_path(language) ) ) {
    return(language)
  }
  NULL
}

# Get
get_config <- function(profile) {
  config <- file.path(path, "_quarto.yml")

  # If it exists, add default profile to config
  try(silent = T, expr = {
    config <-
      read_yaml(config) |>
      purrr::pluck("profile") |>
      purrr::pluck("default") |>
      (function(profile) path(path, get_config_path(profile)) )() |>
      c(config) |>
      rev()
  })

  if ( nzchar(Sys.getenv("QUARTO_PROFILE", unset="")) ) {
    config <- c(config, get_config_path( Sys.getenv("QUARTO_PROFILE") ) )
  }

  if(!is.null(profile)) {
    for (i in profile) {
      config <- c(config, path(path, get_config_path(profile)))
    }
  }

  return(config)
}
