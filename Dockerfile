# Analysis environment for the FJF-2026 programme.
#
# Fixes three layers of the environment so that results do not depend on
# the machine that runs them:
#   1. Operating system and R   -> rocker/r-ver:4.6.1 (Ubuntu 24.04), pinned by digest
#   2. System libraries         -> the apt packages below
#   3. R packages               -> renv.lock, restored from a dated Posit Package Manager snapshot
#
# The project folder itself is mounted at /project at run time (see compose.yaml),
# so code changes never require rebuilding the image; only a change to renv.lock does.

# Multi-architecture index digest of rocker/r-ver:4.6.1 (amd64 and arm64).
FROM rocker/r-ver:4.6.1@sha256:780bc87219250a6dfffef6c812999731986fd94d8d5f7e572d1157c923d1b4bb

LABEL org.opencontainers.image.title="fjf-2026-analysis" \
      org.opencontainers.image.description="Reproducible R environment for the simulated FJF-2026 clinical programme"

# System libraries required by the locked R packages.
# List obtained from the Posit Package Manager system-requirements API for Ubuntu 24.04.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       cmake \
       curl \
       libcurl4-openssl-dev \
       libglpk-dev \
       libicu-dev \
       libpq-dev \
       libssl-dev \
       libuv1-dev \
       libxml2-dev \
       make \
       pandoc \
       zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Quarto, pinned, for protocols, SAPs and reports. Bundles Pandoc and Typst, so PDFs need no LaTeX.
ARG QUARTO_VERSION=1.11.9
ARG TARGETARCH
RUN arch="${TARGETARCH:-$(dpkg --print-architecture)}" \
    && curl -fsSL -o /tmp/quarto.deb \
       "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-${arch}.deb" \
    && apt-get update \
    && apt-get install -y --no-install-recommends /tmp/quarto.deb \
    && rm -rf /tmp/quarto.deb /var/lib/apt/lists/* \
    && quarto --version

# Keep the package library outside /project, so mounting the project does not hide it.
# Packages are copied into the library (not symlinked to the cache), so the cache can be
# deleted afterwards to keep the image small.
ENV RENV_PATHS_LIBRARY=/opt/renv/library \
    RENV_PATHS_CACHE=/opt/renv/cache \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE \
    RENV_CONFIG_PAK_ENABLED=FALSE

WORKDIR /project

# Copy only what renv needs, so this layer is rebuilt only when renv.lock changes.
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/settings.json renv/

# Restore, drop the download cache, then fail the build unless every locked package
# is installed at its locked version.
RUN R -q -e "renv::restore(prompt = FALSE)" \
    && rm -rf /opt/renv/cache \
    && R -q -e "lock <- renv::lockfile_read('renv.lock')\$Packages; \
                want <- vapply(lock, function(p) p\$Version, ''); \
                bad <- names(want)[vapply(names(want), function(p) packageVersion(p) != package_version(want[[p]]), TRUE)]; \
                if (length(bad)) stop('Version mismatch: ', paste(bad, collapse = ', ')); \
                cat(length(want), 'packages match renv.lock\n')" \
    && R -q -e "cat('R_LIBS_USER=', renv::paths\$library(), '\n', sep = '', \
                    file = file.path(R.home('etc'), 'Renviron.site'), append = TRUE)"

# The line above makes the locked library visible to every R process, including those
# started outside /project (for example by Quarto when it renders a document in a subfolder).

CMD ["R"]
