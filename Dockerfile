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
    libfribidi-dev

# copy the whole directory to /rstudio (working directory in Posit Cloud)
COPY . '/home/rstudio/'


# set repos
RUN Rscript -e 'options( \
    repos = c(CRAN = "https://cloud.r-project.org",\
    gongcastro = "https://gongcastro.r-universe.dev",\
    stan = "https://mc-stan.org/r-packages/"),\
    renv.cache.linkable = TRUE, \
    renv.config.cache.symlinks = TRUE)'

# install cli
RUN Rscript -e 'install.packages("cli")'

# install remotes
RUN Rscript -e 'install.packages("remotes")'

# install targets
RUN Rscript -e 'install.packages("targets")'

# install renv
RUN Rscript -e 'install.packages("renv")'
ENV RENV_PATHS_LIBRARY renv/library
RUN cd /home/rstudio/ && Rscript -e 'renv::activate(); renv::restore();'

# expose RStudio IDE on this port
# http://localhost:8787
EXPOSE 8787
