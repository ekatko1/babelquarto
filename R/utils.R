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

get_config_path <- function(path, profile) {
  fs::path(path, paste0("_quarto-", profile, ".yml"))
}

# Return only profiles that exist
if_exists <- function(
  path = ".",
  profile = NULL,
  language = NULL,
  return_path = FALSE
) {
  if (is.null(profile) && is.null(language)) {
    return(NULL)
  } else if (is.null(profile) && !is.null(language)) {
    profile_name = language
  } else if (!is.null(profile) && is.null(language)) {
    profile_name = profile
  } else {
    profile_name = paste0(profile, "-", language)
  }
  config = get_config_path(path, profile_name)
  exists = purrr::map_lgl(config, file_exists)
  if (return_path) {
    return(config[exists])
  }
  profile_name[exists]
}


# Get configuration files, in order from lowest to highest priority
get_config <- function(path, profile) {
  config <- file.path(path, "_quarto.yml")

  # If it exists, add default profile to config
  try(silent = T, expr = {
    default_config = read_yaml(config) |>
      purrr::pluck("profile") |>
      purrr::pluck("default")
    config <- c(
      config,
      if_exists(path = path, profile = default_config, return_path = T)
    )
  })

  if (nzchar(Sys.getenv("QUARTO_PROFILE", unset = ""))) {
    config <- c(config, get_config_path(path, Sys.getenv("QUARTO_PROFILE")))
  }

  if (!is.null(profile)) {
    config <- c(config, get_config_path(path, profile))
  }

  return(config)
}
