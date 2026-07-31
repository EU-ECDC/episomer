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
ARG USER=1000
RUN if id "${USER}" >/dev/null 2>&1; then \
    echo 'user exists already found, reusing it'; \
else \
    RUN groupadd -g "${USER}" epirunner \
      && useradd --create-home --no-log-init -u "${USER}" -g "${USER}" epirunner; \
fi

## Installing SBT
RUN mkdir /git
WORKDIR /git
RUN git clone https://github.com/sbt/sbt.git
WORKDIR /git/sbt
RUN git fetch origin 1.12.x
RUN git checkout origin/1.12.x
RUN chown -R ${USER}:${USER} /git


## Creating episomer code folders 
RUN mkdir -p /episomer/episomer/java
RUN chown -R ${USER}:${USER} /episomer

## Installing R package dependencids
## deb dependencies
RUN apt-get update && \
    apt-get install -y libfontconfig1-dev libcurl4-openssl-dev libuv1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev && \
    apt-get install -y libjpeg-dev libwebp-dev libxml2-dev libudunits2-dev libabsl-dev libgdal-dev cmake gdal-bin libsecret-1-dev && \
    apt-get clean

## dependant packages
USER ${USER}:${USER}
ENV PATH="$PATH:/git/sbt"
WORKDIR /episomer
COPY --chown=${USER}:${USER} --from=src ./episomer/DESCRIPTION /episomer/episomer
COPY --chown=${USER}:${USER} --from=src ./Makefile /episomer/Makefile
COPY --chown=${USER}:${USER} --from=src ./devscripts /episomer/devscripts
RUN DEVT=1 make package-step
RUN DEPS=1 make package-step

## Compiling scala code and producing JAE 
COPY --chown=${USER}:${USER} --from=src ./scala /episomer/scala
RUN JAR=1 make package-step

## Packaging episomer
USER root
RUN apt-get update && \
    apt-get install -y texlive-latex-base texlive-fonts-recommended texlive-fonts-extra texlive-latex-extra pandoc && \
    apt-get clean
USER ${USER}:${USER}
COPY --chown=${USER}:${USER} --from=src . /episomer/
RUN PACK=1 make package-step

## Installing episomer
RUN INSTALL=1 make package-step

## Entry point
ENTRYPOINT ["sleep", "infinity"]
