# cneo2016

install package
:   `Rscript -e 'devtools::document();devtools::install()'`

update site
:   `Rscript -e 'pkgdown::build_site()'`

only update articles from vignettes
:   `Rscript -e 'pkgdown::build_articles()'`

serve site
:   `cd docs && python -m SimpleHTTPServer`

- open with browser [localhost:8000](http://localhost:8000)
