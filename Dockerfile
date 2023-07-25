FROM rocker/rstudio:4.2.2

LABEL "about" = "A Docker container for the cognate-begininings study" \
    "author" = "Gonzalo Garcia-Castro <gongarciacastro@gmail.com>"\
    "github" = "https://github.com/gongcastro/cognate-beginnings" \
    "osf" = "https://osf.io/hy984/" \
    "source"="https://github.com/gongcastro/cognate-beginnings/blob/main/Dockerfile"

# add C++ dependencies
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
#RUN mkdir /cognate-beginnings/ && chown -c rstudio /cognate-beginnings/
COPY . /home/rstudio/
#RUN cd /cognate-beginnings/
WORKDIR /home/rstudio/

# install and configure renv
ENV RENV_VERSION 1.0.0
RUN R -e 'install.packages("remotes", repos = c(CRAN = "https://cloud.r-project.org"))'
RUN R -e 'remotes::install_version("rstudio/renv", version = "1.0.0")'
ENV RENV_PATHS_LIBRARY renv/library
RUN Rscript -e 'renv::restore()'

# install basic R dependencies
RUN Rscript -e 'Sys.setenv(R_INSTALL_STAGED = FALSE)'
RUN Rscript -e 'remotes::install_version("cli", version = "3.6.1")'
RUN Rscript -e 'remotes::install_version("targets", version = "1.2.0")'

# expose RStudio IDE on this port
# http://localhost:8787
EXPOSE 8787
