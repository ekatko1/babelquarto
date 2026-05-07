test_that("profile config is respected", {
  parent_dir <- path_dir(withr::local_tempdir())
  project_dir <- path(parent_dir, "blop")
  dir_create(project_dir)
  "project:
  type: book
  output-dir: _web-book

format: html

book:
  chapters:
    - index.qmd
" |>
  brio::write_lines(path = path(project_dir, "_quarto-web-book.yml"))

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
    brio::write_lines(path = path(project_dir, "_quarto.yml"))

  file_create(project_dir, "index.qmd")
  file_create(project_dir, "exclude.qmd")

  try(
    { render_book(project_path = project_dir, profile = "web-book") }
  )

  expect_equal(
    { path(project_dir, "_web-book") |>
        file_exists( ) |>
        unname()
    },
    { TRUE }
  )

  expect_equal(
    {  path(project_dir, "_web-book", "index.html") |>
        file_exists( ) |>
        unname()
    },
    { TRUE }
  )

  expect_equal(
    {  path(project_dir, "_web-book", "exclude.html") |>
        file_exists( ) |>
        unname()
    },
    { FALSE }
  )

})

