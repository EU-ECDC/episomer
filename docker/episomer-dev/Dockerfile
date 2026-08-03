FROM rocker/r-base:latest

## Installing System dependencies
RUN apt-get update && \
    apt-get install -y apt-transport-https curl gnupg git&& \
    apt-get clean

## Installing JAVA
RUN apt-get update && \
    apt-get install -y openjdk-21-jdk && \
    apt-get clean

## Creating unprivilegied user for episomer
ARG EPISOMER_USER=1000
RUN if id "${EPISOMER_USER}" >/dev/null 2>&1; then \
    echo 'user exists already found, reusing it'; \
else \
    RUN groupadd -g "${EPISOMER_USER}" epirunner \
      && useradd --create-home --no-log-init -u "${EPISOMER_USER}" -g "${EPISOMER_USER}" epirunner; \
fi

## Installing SBT
RUN mkdir /git
WORKDIR /git
RUN git clone https://github.com/sbt/sbt.git
WORKDIR /git/sbt
RUN git fetch origin 1.12.x
RUN git checkout origin/1.12.x
RUN chown -R ${EPISOMER_USER}:${EPISOMER_USER} /git


## Creating episomer code folders 
RUN mkdir -p /episomer/episomer/java
RUN chown -R ${EPISOMER_USER}:${EPISOMER_USER} /episomer

## Installing R package dependencids
## deb dependencies
RUN apt-get update && \
    apt-get install -y libfontconfig1-dev libcurl4-openssl-dev libuv1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev && \
    apt-get install -y libjpeg-dev libwebp-dev libxml2-dev libudunits2-dev libabsl-dev libgdal-dev cmake gdal-bin libsecret-1-dev && \
    apt-get clean

RUN apt-get update && \
    apt-get install -y texlive-latex-base texlive-fonts-recommended texlive-fonts-extra texlive-latex-extra pandoc && \
    apt-get clean

## dependant packages
USER ${EPISOMER_USER}:${EPISOMER_USER}
ENV PATH="$PATH:/git/sbt"
WORKDIR /episomer
RUN R -e 'dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)'
RUN R -e 'install.packages(c("devtools", "remotes"), lib = Sys.getenv("R_LIBS_USER"))'

COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} --from=src ./episomer/DESCRIPTION /episomer/episomer
RUN R -e 'devtools::install_deps("episomer")'

## Compiling scala code and producing JAR
COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} --from=src ./scala /episomer/scala
WORKDIR /episomer/scala
RUN sbt package
RUN mkdir -p /episomer/episomer/inst/java
RUN cp /episomer/scala/target/scala-2.13/ecdc-episomer-bundle_2.13-1.0.jar /tmp/ecdc-episomer-bundle_2.13-1.0.jar


## Packaging episomer
WORKDIR /episomer
COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} --from=src . /episomer/
RUN cp /tmp/ecdc-episomer-bundle_2.13-1.0.jar /episomer/episomer/inst/java
RUN PACK=1 make package-step

## Installing episomer
RUN INSTALL=1 make package-step

## Creating the data folder
user root
RUN mkdir -p "/episomer-data/jobs"
RUN chown -R ${EPISOMER_USER}:${EPISOMER_USER} /episomer-data
USER ${EPISOMER_USER}:${EPISOMER_USER}

## Entry point
ENTRYPOINT ["Rscript", "/episomer/devscripts/run.R"]
