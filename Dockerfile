FROM rocker/r-base:latest

# ============================================================
# System dependencies
# ============================================================

RUN apt-get update && \
    apt-get install -y \
        apt-transport-https \
        curl \
        gnupg \
        git && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# ============================================================
# Java
# ============================================================

RUN apt-get update && \
    apt-get install -y openjdk-21-jdk && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# ============================================================
# Debian dependencies
# ============================================================

RUN apt-get update && \
    apt-get install -y \
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
        libsecret-1-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# ============================================================
# Documentation dependencies
# ============================================================

RUN apt-get update && \
    apt-get install -y \
        texlive-latex-base \
        texlive-fonts-recommended \
        texlive-fonts-extra \
        texlive-latex-extra \
        pandoc && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# ============================================================
# Create unprivileged user
# ============================================================

ARG EPISOMER_UID=1000
RUN if id "${EPISOMER_USER}" >/dev/null 2>&1; then \
    echo 'user exists already found, reusing it'; \
else \
    RUN groupadd -g "${EPISOMER_USER}" epirunner \
      && useradd --create-home --no-log-init -u "${EPISOMER_USER}" -g "${EPISOMER_USER}" epirunner; \
fi


# ============================================================
# Install SBT
# ============================================================

RUN mkdir -p /git

WORKDIR /git

RUN git clone https://github.com/sbt/sbt.git

WORKDIR /git/sbt

RUN git fetch origin 1.12.x && \
    git checkout origin/1.12.x

RUN chown -R "${EPISOMER_USER}:${EPISOMER_USER}" /git

# ============================================================
# Create episomer directories
# ============================================================

RUN mkdir -p /episomer/episomer/java && \
    chown -R "${EPISOMER_USER}:${EPISOMER_USER}" /episomer

RUN mkdir -p /episomer-data && \
    chown -R "${EPISOMER_USER}:${EPISOMER_USER}" /episomer-data

# ============================================================
# Run application build as unprivileged user
# ============================================================

USER ${EPISOMER_USER}:${EPISOMER_USER}

ENV PATH="${PATH}:/git/sbt"

WORKDIR /episomer

RUN R -e 'dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)'

RUN R -e 'install.packages(c("devtools", "remotes"), lib = Sys.getenv("R_LIBS_USER"))'

# ============================================================
# Install R dependencies
# ============================================================

COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} \
    ./episomer/DESCRIPTION \
    /episomer/episomer/

RUN R -e 'devtools::install_deps("episomer")'

# ============================================================
# Compile Scala code
# ============================================================

COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} \
    ./scala \
    /episomer/scala

WORKDIR /episomer/scala

RUN sbt package

RUN mkdir -p /episomer/episomer/inst/java

RUN cp \
    /episomer/scala/target/scala-2.13/ecdc-episomer-bundle_2.13-1.0.jar \
    /tmp/ecdc-episomer-bundle_2.13-1.0.jar

# ============================================================
# Package episomer
# ============================================================

WORKDIR /episomer

COPY --chown=${EPISOMER_USER}:${EPISOMER_USER} \
    . \
    /episomer/

RUN cp \
    /tmp/ecdc-episomer-bundle_2.13-1.0.jar \
    /episomer/episomer/inst/java/

RUN PACK=1 make package-step

# ============================================================
# Install episomer
# ============================================================

RUN INSTALL=1 make package-step

# ============================================================
# Entry point
# ============================================================

ENTRYPOINT ["bash", "/episomer/devscripts/run.sh"]
