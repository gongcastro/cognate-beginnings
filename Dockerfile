FROM rocker/rstudio:latest
LABEL "about" = "A Docker container for the cognate-begininings study" \
    "author" = "Gonzalo Garcia-Castro <gongarciacastro@gmail.com>"\
    "github" = "https://github.com/gongcastro/cognate-beginnings" \
    "osf" = "https://osf.io/hy984/"

# copy the whole directory to /rstudio (working directory in Posit Cloud)
USER root
ENV EDITOR_FOCUS_DIR "/home/rstudio/cognate-beginnings"
RUN mkdir -p '$EDITOR_FOCUS_DIR'
COPY . '$EDITOR_FOCUS_DIR'
WORKDIR '$EDITOR_FOCUS_DIR'

# add C++ dependencies
RUN apt update && apt-get install -y libxml2-dev
RUN apt update && apt-get install -y libglpk-dev
RUN apt update && apt-get install -y libgmp3-dev
RUN apt update && apt-get install -y build-essential gfortran gcc g++ make
RUN apt update && apt-get install -y libssl-dev
RUN apt update && apt-get install -y ssh-askpass ssh-askpass-gnome
# set repos
RUN Rscript -e 'options(repos = c(CRAN = "https://cloud.r-project.org",\
    gongcastro = "https://gongcastro.r-universe.dev",\
    stan = "https://mc-stan.org/r-packages/"))'

# install cli
#RUN Rscript -e 'install.packages("cli")'

# install remotes
#RUN R -e 'install.packages("remotes")'

# install igraph (complicated dependency)
#RUN R -e 'remotes::install_github("igraph/rigraph")'

# install targets
#RUN R -e 'install.packages("targets")'

# install renv
ENV RENV_VERSION 0.17.3
RUN R -e 'install.packages("renv")'

# expose RStudio IDE on this port
# http://localhost:8787
EXPOSE 8787
