## Build site
pkgdown::build_site()

## Build site and immediately preview
pkgdown::build_site(preview = TRUE)

# just preview
pkgdown::preview_site()

# do it in the background
job::job({pkgdown::build_site(preview = TRUE)})

## Quicker
## Only builds what has changed
## Build pkg first
pkgdown::build_site(lazy = TRUE, devel = TRUE, preview = TRUE)

# builds top page
pkgdown::build_home(preview = TRUE)

# build a single article
pkgdown::build_article("calc-probs")
pkgdown::preview_page('articles/calc-probs.html')

# add a new vignette
usethis::use_vignette("intermittent_old", "Intermittent-flow respirometry: Alternative approaches")


# sets up GH Actions workflow. Needs tested.
#usethis::use_pkgdown_github_pages()


