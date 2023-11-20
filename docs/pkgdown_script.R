## Build site

pkgdown::build_site()
pkgdown::build_site_github_pages()

# build and preview
pkgdown::build_site(preview = TRUE)

# do it in the background
job::job({pkgdown::build_site(preview = TRUE)})

## Quicker
## Only builds what has changed
## Build pkg first
pkgdown::build_site(lazy = TRUE, devel = TRUE, preview = TRUE)

# ? not sure what this does
#pkgdown::preview_site(pkg = ".", path = ".", preview = NA)

# builds top page
pkgdown::build_home(preview = TRUE)

# build a single article
pkgdown::build_article("raw-data-import")

# add a new vignette
usethis::use_vignette("name of file", "Title of vignette")


# sets up GH Actions workflow. Needs tested.
#usethis::use_pkgdown_github_pages()


