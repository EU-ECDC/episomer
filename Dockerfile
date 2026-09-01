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

## Runtime system dependencies (curl is used by the compose healthcheck)
RUN apt-get update && \
    apt-get install -y curl && \
    apt-get clean

## Installing JAVA runtime (the JDK is only needed to compile Scala, see build stage)
RUN apt-get update && \
    apt-get install -y openjdk-21-jre-headless && \
    apt-get clean

## deb dependencies
RUN apt-get update && \
    apt-get install -y libfontconfig1-dev libcurl4-openssl-dev libuv1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev && \
    apt-get install -y libjpeg-dev libwebp-dev libxml2-dev libudunits2-dev libabsl-dev libgdal-dev libsecret-1-dev && \
    apt-get clean

## Runtime documentation dependencies: the dashboard exports a PDF via rmarkdown.
## lmodern is required by the default rmarkdown pdf_document template.
## texlive-fonts-extra is only needed to build the package manual, see build stage.
RUN apt-get update && \
    apt-get install -y texlive-latex-base texlive-fonts-recommended texlive-latex-extra lmodern pandoc && \
    apt-get clean

FROM base AS build

ARG EPISOMER_USER=1000

## Build-only system dependencies
RUN apt-get update && \
    apt-get install -y apt-transport-https gnupg git openjdk-21-jdk-headless \
                       cmake gdal-bin texlive-fonts-extra && \
    apt-get clean

## Installing SBT
RUN mkdir -p /git
WORKDIR /git
RUN git clone https://github.com/sbt/sbt.git
WORKDIR /git/sbt
RUN git fetch origin 1.12.x
RUN git checkout origin/1.12.x
RUN chown -R ${EPISOMER_USER}:${EPISOMER_USER} /git

## Installing R package dependencids

## dependant packages
USER ${EPISOMER_USER}:${EPISOMER_USER}
ENV PATH="$PATH:/git/sbt"
ENV R_LIBS_USER=/opt/episomer-library
WORKDIR /episomer
RUN R -e 'dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)'
RUN R -e 'install.packages(c("devtools", "remotes"), lib = Sys.getenv("R_LIBS_USER"))'

COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} ./episomer/DESCRIPTION /episomer/episomer
RUN R -e 'devtools::install_deps("episomer")'

## Compiling scala code and producing JAR
COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} ./scala /episomer/scala
WORKDIR /episomer/scala
RUN sbt package
RUN mkdir -p /episomer/episomer/inst/java
RUN cp /episomer/scala/target/scala-2.13/ecdc-episomer-bundle_2.13-1.0.jar /tmp/ecdc-episomer-bundle_2.13-1.0.jar


## Packaging episomer
WORKDIR /episomer
COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} . /episomer/
RUN cp /tmp/ecdc-episomer-bundle_2.13-1.0.jar /episomer/episomer/inst/java
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
