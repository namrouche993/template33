FROM rocker/shiny-verse:latest

 RUN apt-get update && apt-get install -y  \
 sudo \
 pandoc \
  pandoc-citeproc \
  libicu-dev \
  libglpk-dev \
  libgmp3-dev \
  libxml2-dev \
  libcurl4-openssl-dev \
  libcairo2-dev \
  libxt-dev \
  libssl-dev \
  libssh2-1-dev \
  zlib1g-dev \
  libgdal-dev \
  libproj-dev \
  gdal-bin
  

RUN R -e "install.packages('httr',dependencies = TRUE)"

COPY app.R /srv/shiny-server/app.R

COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

EXPOSE 8282

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]
