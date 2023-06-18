FROM rocker/rstudio:4.2.2

LABEL "about" = "A Docker container for the cognate-begininings study" \
    "author" = "Gonzalo Garcia-Castro <gongarciacastro@gmail.com>"\
    "github" = "https://github.com/gongcastro/cognate-beginnings" \
    "osf" = "https://osf.io/hy984/" \
    "source"="https://github.com/gongcastro/cognate-beginnings/blob/main/Dockerfile"

# add C++ dependencies
USER root
RUN apt-get update && \
    apt-get install -y libxml2-dev \
    libglpk-dev \
    libgmp3-dev \
    build-essential \
    gfortran \
    gcc \
    g++ \
    make\
    libssl-dev \
    libzmq3-dev \
    libpng-dev \
    libsodium-dev \
    ssh-askpass \
    ssh-askpass-gnome \
    default-libmysqlclient-dev \
    libmagick++-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libicu-dev

# copy the whole directory to /rstudio (working directory in Posit Cloud)
COPY . '/home/rstudio/'


# set repos
RUN Rscript -e 'options( \
    repos = c(CRAN = "https://cloud.r-project.org",\
    gongcastro = "https://gongcastro.r-universe.dev",\
    stan = "https://mc-stan.org/r-packages/"))'

# install basic R dependencies
RUN Rscript -e 'install.packages("cli")'
RUN Rscript -e 'install.packages("remotes")'
RUN Rscript -e 'install.packages("targets")'

# install and configure renv
ENV RENV_PATHS_ROOT=/home/rstudio/renv
ENV RENV_PATHS_LIBRARY=/home/rstudio/renv/library
RUN echo "" >> ${R_HOME}/etc/.Renviron
RUN echo "RENV_PATHS_ROOT=${RENV_PATHS_ROOT}" >> $/home/rstudio/.Renviron
RUN echo "RENV_PATHS_LIBRARY=${RENV_PATHS_LIBRARY}" >> /home/rstudio/.Renviron

RUN Rscript -e 'remotes::install_github("rstudio/renv@0.15.4")'
COPY renv ./renv/
RUN R -e "renv::restore(lockfile='/home/rstudio/renv.lock', library='/home/rstudio/renv/library/R-4.0/x86_64-pc-linux-gnu')"

# expose RStudio IDE on this port
# http://localhost:8787
EXPOSE 8787
