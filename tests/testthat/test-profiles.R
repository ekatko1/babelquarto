path_exists <- function(...) {
  fs::path(...) |>
    file.exists()
}

# Eventually, Quarto will take care of throwing a warning
test_that("supplying a non-existant profile does not cause an error", {
  parent_dir <- withr::local_tempdir()
  project_dir <- fs::path(parent_dir, "blop")
  fs::dir_create(project_dir)
  "project:
  type: book

format: html

book:
  chapters:
    - index.qmd

babelquarto:
  languagecodes:
  - name: fr
    text: 'Version en français'
  - name: en
    text: 'English version'
  mainlanguage: 'fr'
  languages: 'en'
  " |>
    brio::write_lines(path = fs::path(project_dir, "_quarto.yml"))

  fs::file_create(project_dir, "index.qmd")

  render_book(project_path = project_dir, profile = "web-book")
  expect_equal( path_exists(project_dir, "_book", "index.html"), TRUE )
})


test_that("profile config is respected", {
  parent_dir <- withr::local_tempdir()
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

  expect_equal( path_exists(project_dir, "_web-book"), TRUE )
  expect_equal( path_exists(project_dir, "_web-book", "index.html"), TRUE )
  expect_equal( path_exists(project_dir, "_web-book", "exclude.html"), FALSE )

})


test_that("default profile assigned in _quarto.yml is respected", {
  parent_dir <- withr::local_tempdir()
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

  expect_equal( path_exists(project_dir, "_web-book"), TRUE )
  expect_equal(  path_exists(project_dir, "_web-book", "index.html"),  TRUE )
  expect_equal(  path_exists(project_dir, "_web-book", "exclude.html"),  FALSE )
})

