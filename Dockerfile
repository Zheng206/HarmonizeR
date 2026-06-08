# =============================================================================
# HarmonizeR — Docker image
#
# This Dockerfile lives at the repo root. The build context is the repo itself,
# so `COPY .` installs from local source — no separate HarmonizeR/ subfolder
# needed.
#
# Base image: rocker/tidyverse:4.4
#   Ships with R 4.4, devtools, remotes, tidyverse, and key system libs
#   pre-compiled — saves ~10 min vs a bare r-ver image.
# =============================================================================
FROM --platform=linux/amd64 rocker/tidyverse:4.4

LABEL org.opencontainers.image.title="HarmonizeR"
LABEL org.opencontainers.image.description="R package test + Shiny app environment"
LABEL org.opencontainers.image.source="https://github.com/Zheng206/HarmonizeR"

# -----------------------------------------------------------------------------
# 1. System libraries
#    Required by lme4 (LAPACK/BLAS), MCMCpack, car, plotly, openxlsx, etc.
# -----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libfontconfig1-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
        libgit2-dev \
        pandoc \
    && rm -rf /var/lib/apt/lists/*

# -----------------------------------------------------------------------------
# 2. CRAN packages — split into layers so a rebuild only reinstalls what changed
#    Ncpus=4 parallelises compilation inside the container.
# -----------------------------------------------------------------------------

# 2a. Shiny UI layer
RUN Rscript -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'DT' \
), repos='https://cloud.r-project.org', Ncpus=4)"

# 2b. Tidyverse / data-wrangling (already partially present in rocker/tidyverse,
#     but pinned here so we get the minimum versions from DESCRIPTION)
RUN Rscript -e "install.packages(c( \
  'ggplot2', 'dplyr', 'tidyr', 'magrittr', 'purrr', \
  'broom', 'tibble', 'scales', 'patchwork', 'ggrepel' \
), repos='https://cloud.r-project.org', Ncpus=4)"

# 2c. Statistics / modelling
RUN Rscript -e "install.packages(c( \
  'car', 'mgcv', 'lme4', 'MASS', 'MCMCpack' \
), repos='https://cloud.r-project.org', Ncpus=4)"

# 2d. Visualisation / output
RUN Rscript -e "install.packages(c( \
  'RColorBrewer', 'plotly', 'htmlwidgets', 'openxlsx', 'zip' \
), repos='https://cloud.r-project.org', Ncpus=4)"

# 2e. ML / evaluation
RUN Rscript -e "install.packages(c( \
  'caret', 'randomForest', 'pROC' \
), repos='https://cloud.r-project.org', Ncpus=4)"

# 2f. Bayesian / testing utilities
RUN Rscript -e "install.packages(c( \
  'posterior', 'remotes', 'testthat', 'covr' \
), repos='https://cloud.r-project.org', Ncpus=4)"

# -----------------------------------------------------------------------------
# 3. GitHub packages
#    MultiComBat is listed under Remotes: Zheng206/MultiComBat in DESCRIPTION
# -----------------------------------------------------------------------------
RUN Rscript -e "remotes::install_github('Zheng206/MultiComBat', upgrade='never')"

# -----------------------------------------------------------------------------
# 4. Copy package source and install
#    The Dockerfile lives at the repo root, so `.` is the package source.
#    `dependencies=FALSE` skips re-installing what we already installed above.
# -----------------------------------------------------------------------------
COPY . /pkg/HarmonizeR/

RUN Rscript -e "remotes::install_local('/pkg/HarmonizeR', dependencies=FALSE, upgrade='never')"

# -----------------------------------------------------------------------------
# 5. Runtime defaults
# -----------------------------------------------------------------------------
WORKDIR /workspace
EXPOSE 3838
CMD ["R"]
