# Optional developer environment, not part of the installed R package.
# Official Rocker R 4.6.1 multi-architecture image, verified 27 October 2026.
FROM rocker/geospatial:4.6.1@sha256:37415fce2558927e057b724c12e76ba2e2f9c494387147787d434d13e7785c72

# Rocker supplies GDAL, GEOS, PROJ, Pandoc and devtools. Ensure command-line
# ZIP support for the package's archive tests without changing host tools.
USER root
RUN apt-get update \
    && apt-get install -y --no-install-recommends zip \
    && apt-get clean

# Manual development-package installation uses the container user's library.
ENV R_LIBS_USER=/home/rstudio/R/library
USER rstudio
RUN mkdir -p /home/rstudio/R/library
