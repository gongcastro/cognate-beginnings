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
RUN R -e 'install.packages("remotes", repos = c(CRAN = "https://cloud.r-project.org"))'
RUN R -e 'remotes::install_version("renv", version = "1.0.0", repos = c("https://rstudio.r-universe.dev", "https://cloud.r-project.org"))'
ENV RENV_PATHS_LIBRARY renv/library
RUN Rscript -e 'renv::restore()'

# install basic R dependencies
RUN Rscript -e 'Sys.setenv(R_INSTALL_STAGED = FALSE)'
RUN Rscript -e 'install.packages("cli", repos = c("https://r-lib.r-universe.dev", "https://cloud.r-project.org"))'
RUN Rscript -e 'install.packages("targets", repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))'

# install Quarto
RUN mkdir -p /opt/quarto/v1.3.433
RUN curl -o quarto.tar.gz -L \
    'https://github.com/quarto-dev/quarto-cli/releases/download/v1.3.433/quarto-1.3.433-linux-arm64.deb'
RUN tar -zxvf quarto.tar.gz \
    -C '/opt/quarto/v1.3.433' \
    --strip-components=1
RUN rm quarto.tar.gz

# expose RStudio IDE on this port
# http://localhost:8787
EXPOSE 8787
