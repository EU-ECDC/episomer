FROM docker.io/rocker/r-base:latest AS base

ARG EPISOMER_USER=1000

## Creating unprivilegied user for episomer and precreate directories
RUN if ! getent group "${EPISOMER_USER}" >/dev/null; then \
      groupadd --gid "${EPISOMER_USER}" epirunner; \
    fi && \
    if ! getent passwd "${EPISOMER_USER}" >/dev/null; then \
      useradd --create-home --no-log-init --uid "${EPISOMER_USER}" --gid "${EPISOMER_USER}" epirunner; \
    fi && \
    mkdir -p /episomer/episomer/java /episomer-data /opt/episomer-library && \
    chown -R "${EPISOMER_USER}:${EPISOMER_USER}" /episomer /episomer-data /opt/episomer-library

## Keep runtime libraries, Java, and the PDF toolchain in the final image.
## curl is used by the compose healthcheck; lmodern is needed by rmarkdown PDF export.
## texlive-fonts-extra is only needed in the build stage for the package manual.
RUN apt-get update && \
    apt-get install -y \
      curl openjdk-21-jre-headless \
      libfontconfig1-dev libcurl4-openssl-dev libuv1-dev libharfbuzz-dev \
      libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
      libwebp-dev libxml2-dev libudunits2-dev libabsl-dev libgdal-dev \
      libsecret-1-dev \
      texlive-latex-base texlive-fonts-recommended texlive-latex-extra \
      lmodern pandoc && \
    rm -rf /var/lib/apt/lists/*

FROM base AS build

ARG EPISOMER_USER=1000

## Build-only system dependencies
RUN apt-get update && \
    apt-get install -y apt-transport-https gnupg git openjdk-21-jdk-headless \
                       cmake gdal-bin texlive-fonts-extra && \
    rm -rf /var/lib/apt/lists/*

## Installing SBT without the repository history
RUN git clone --depth 1 --branch 1.12.x https://github.com/sbt/sbt.git /git/sbt && \
    chown -R ${EPISOMER_USER}:${EPISOMER_USER} /git
WORKDIR /git/sbt

## Installing R package dependencies

## dependant packages
USER ${EPISOMER_USER}:${EPISOMER_USER}
ENV PATH="$PATH:/git/sbt"
ENV R_LIBS_USER=/opt/episomer-library
WORKDIR /episomer
RUN R -e 'dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)'
RUN R -e 'install.packages("pak", lib = Sys.getenv("R_LIBS_USER"), repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))'
RUN R -e 'pak::pkg_install(c("devtools","remotes"), lib = Sys.getenv("R_LIBS_USER"))'

COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} ./episomer/DESCRIPTION /episomer/episomer
RUN R -e 'pak::local_install_deps("episomer", lib = Sys.getenv("R_LIBS_USER"))'

## Compiling scala code and producing JAR
COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} ./scala /episomer/scala
WORKDIR /episomer/scala
RUN sbt package
RUN mkdir -p /episomer/episomer/inst/java
RUN cp /episomer/scala/target/scala-2.13/ecdc-episomer-bundle_2.13-*.jar /tmp/


## Packaging episomer
WORKDIR /episomer
COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} . /episomer/
RUN cp /tmp/ecdc-episomer-bundle_2.13-*.jar /episomer/episomer/inst/java
RUN PACK=1 make package-step

## Installing episomer
RUN INSTALL=1 make package-step

FROM base AS final

ARG EPISOMER_USER=1000

COPY --from=build --chown=${EPISOMER_USER}:${EPISOMER_USER} /opt/episomer-library /opt/episomer-library
COPY --from=build --chown=${EPISOMER_USER}:${EPISOMER_USER} /episomer/devscripts/run.sh /episomer/devscripts/run.sh
COPY --from=build --chown=${EPISOMER_USER}:${EPISOMER_USER} /episomer/devscripts/run.R /episomer/devscripts/run.R

USER ${EPISOMER_USER}:${EPISOMER_USER}
ENV R_LIBS_USER=/opt/episomer-library
WORKDIR /episomer
ENTRYPOINT ["bash", "/episomer/devscripts/run.sh"]
