FROM code.chs.usgs.gov:5001/ctek/docker/r-lang/r-base:4.4

# Necessary R libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  pandoc-citeproc \
  qpdf \
  r-cran-rmarkdown \
  r-cran-knitr \
  r-cran-devtools \
  r-cran-dataretrieval \
  r-cran-testthat \
  r-cran-slider \
  r-cran-covr \
  && apt-get install -y pandoc \
  && rm -rf /var/lib/apt/lists/*




