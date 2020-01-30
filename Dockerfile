FROM rocker/r-base

LABEL maintainer "Are Edvardsen <are.edvardsen@helse-nord.no>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libxml2-dev \
    default-jdk \
    libssl-dev \
    libmariadbclient-dev 

# basic R functionality
RUN R -e "install.packages(c('remotes'), repos='https://cloud.r-project.org/')"

# install rapRegTemplate app
RUN R -e "remotes::install_github('Rapporteket/rapRegTemplate')"

# copy the app to the image
RUN mkdir /root/rapRegTemplate
COPY inst/shinyApps/app1 /root/rapRegTemplate

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/rapRegTemplate')"]
