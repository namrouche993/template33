  FROM rocker/shiny-verse:latest

# system libraries of general use

 RUN apt-get update && apt-get install -y \
    sudo \
   pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 
    
RUN apt-get install r-cran-ggplot2

RUN R -e "paste('le GETWDDDDDDDDDDDDDDDDD  EGALEEE   A  : ',getwd() )"

#COPY highcharter_0.8.2.tar.gz /srv/shiny-server/highcharter_0.8.2.tar.gz

#RUN R -e "install.packages('rgdal')"


RUN R -e "install.packages('highcharter')"

COPY app.R /srv/shiny-server/app.R


COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]
