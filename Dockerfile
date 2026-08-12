FROM rocker/r-base:latest

ARG EPISOMER_USER=1000
ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="$PATH:/git/sbt"

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      apt-transport-https \
      curl \
      gnupg \
      git \
      openjdk-21-jdk \
      libfontconfig1-dev \
      libcurl4-openssl-dev \
      libuv1-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libfreetype6-dev \
      libpng-dev \
      libtiff5-dev \
      libjpeg-dev \
      libwebp-dev \
      libxml2-dev \
      libudunits2-dev \
      libabsl-dev \
      libgdal-dev \
      cmake \
      gdal-bin \
      libsecret-1-dev \
      texlive-latex-base \
      texlive-fonts-recommended \
      texlive-fonts-extra \
      texlive-latex-extra \
      pandoc && \
    rm -rf /var/lib/apt/lists/*

RUN if id "${EPISOMER_USER}" >/dev/null 2>&1; then \
    echo 'user exists already found, reusing it'; \
else \
    RUN groupadd -g "${EPISOMER_USER}" epirunner \
      && useradd --create-home --no-log-init -u "${EPISOMER_USER}" -g "${EPISOMER_USER}" epirunner; \
fi

RUN mkdir -p /git /episomer/episomer/java /episomer-data && \
    chown -R ${EPISOMER_USER}:${EPISOMER_USER} /git /episomer /episomer-data

WORKDIR /git
RUN git clone https://github.com/sbt/sbt.git /git/sbt && \
    cd /git/sbt && \
    git fetch origin 1.12.x && \
    git checkout origin/1.12.x && \
    chown -R ${EPISOMER_USER}:${EPISOMER_USER} /git

WORKDIR /episomer
COPY . /episomer/
RUN chown -R ${EPISOMER_USER}:${EPISOMER_USER} /episomer

USER ${EPISOMER_USER}:${EPISOMER_USER}
RUN R -e 'dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)' && \
    R -e 'install.packages(c("devtools", "remotes"), lib = Sys.getenv("R_LIBS_USER"))'

RUN R -e 'devtools::install_deps("episomer")'

WORKDIR /episomer/scala
RUN sbt package
RUN mkdir -p /episomer/episomer/inst/java && \
    cp /episomer/scala/target/scala-2.13/ecdc-episomer-bundle_2.13-1.0.jar /tmp/ecdc-episomer-bundle_2.13-1.0.jar

WORKDIR /episomer
RUN make package

EXPOSE 8080
ENTRYPOINT ["bash", "/episomer/devscripts/run.sh"]