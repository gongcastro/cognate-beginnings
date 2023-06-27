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
RUN mkdir /cognate-beginnings/ && chown -c rstudio /cognate-beginnings/
COPY . '/home/rstudio/cognate-beginnings'
WORKDIR '/home/rstudio/cognate-beginnings'
# install basic R dependencies
RUN Rscript -e 'install.packages("cli")'
RUN Rscript -e 'install.packages("remotes")'
RUN Rscript -e 'install.packages("targets")'

# install and configure renv
RUN Rscript -e 'remotes::install_github("rstudio/renv@0.15.4")'
RUN Rscript -e 'renv::restore()'
RUN Rscript -e 'renv::restore()'


# expose RStudio IDE on this port
# http://localhost:8787
EXPOSE 8787
