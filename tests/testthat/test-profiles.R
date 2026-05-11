test_that("profile config is respected", {
  parent_dir <- fs::path_dir(withr::local_tempdir())
  project_dir <- fs::path(parent_dir, "blop")
  fs::dir_create(project_dir)
  "project:
  type: book
  output-dir: _web-book

format: html

book:
  chapters:
    - index.qmd
" |>
  brio::write_lines(path = fs::path(project_dir, "_quarto-web-book.yml"))

  r"(project:
  output-dir: _docs
  render:
    - "*.qmd"

format: html

babelquarto:
  languagecodes:
  - name: fr
    text: "Version en français"
  - name: en
    text: 'English version'
  mainlanguage: 'fr'
  languages: ['en']
)" |>
    brio::write_lines(path = fs::path(project_dir, "_quarto.yml"))

  fs::file_create(project_dir, "index.qmd")
  fs::file_create(project_dir, "exclude.qmd")


  render_book(project_path = project_dir, profile = "web-book")

  # use to check for file existance
  path_verif <- function(path = "_web-book") {
    fs::path(project_dir, path) |>
      file.exists()
  }

  expect_equal( { path_verif("_web-book")}, { TRUE } )
  expect_equal( { path_verif(fs::path("_web-book", "index.html"))}, { TRUE } )
  expect_equal( { path_verif(fs::path("_web-book", "exclude.html"))}, { FALSE } )

})



test_that("default profile assigned in _quarto.yml is respected", {
  parent_dir <- fs::path_dir(withr::local_tempdir())
  project_dir <- fs::path(parent_dir, "blop")
  fs::dir_create(project_dir)
  "project:
  type: book
  output-dir: _web-book

format: html

book:
  chapters:
    - index.qmd
" |>
    brio::write_lines(path = fs::path(project_dir, "_quarto-web-book.yml"))

  r"(project:
  output-dir: _docs
  render:
    - "*.qmd"

format: html

profile:
  default: web-book

babelquarto:
  languagecodes:
  - name: fr
    text: "Version en français"
  - name: en
    text: 'English version'
  mainlanguage: 'fr'
  languages: ['en']
)" |>
    brio::write_lines(path = fs::path(project_dir, "_quarto.yml"))

  fs::file_create(project_dir, "index.qmd")
  fs::file_create(project_dir, "exclude.qmd")


  render_book(project_path = project_dir, profile = "web-book")

  # use to check for file existance
  path_verif <- function(path = "_web-book") {
    fs::path(project_dir, path) |>
      file.exists()
  }

  expect_equal( { path_verif("_web-book")}, { TRUE } )
  expect_equal( { path_verif(fs::path("_web-book", "index.html"))}, { TRUE } )
  expect_equal( { path_verif(fs::path("_web-book", "exclude.html"))}, { FALSE } )

})

