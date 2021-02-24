FROM rocker/shiny-verse:latest

 RUN apt-get update && apt-get install -y  \
 sudo \
 pandoc \
  pandoc-citeproc \
  python \
  libicu-dev \
  libudunits2-dev \
  libglpk-dev \
  libgmp3-dev \
  libxml2-dev \
  libcurl4-openssl-dev \
  libcairo2-dev \
  libxt-dev \
  libssl-dev \
  libprotobuf-dev \
  protobuf-compiler \
  software-properties-common \
  libgeos-dev \
  libudunits2-dev \
  libv8-dev \
  libssh2-1-dev \
  libpng-dev \
  zlib1g-dev \
  libgdal-dev \
  libproj-dev \
  gdal-bin

RUN apt-add-repository -y ppa:ubuntugis/ubuntugis-unstable


RUN R -e "paste(installed.packages()[,1])"
RUN R -e "install.packages('reticulate')"

#RUN R -e "install.packages('fresh')"
#RUN R -e "install.packages('leaflet')"
#RUN R -e "install.packages('leaflet.extras')"




#RUN R -e "remotes::install_github('cran/rgdal', dependencies = TRUE)   "
#RUN R -e "install.packages('rgdal')      "
#RUN R -e "install.packages('sp')      "
#RUN R -e "remotes::install_github('Swechhya/excelR')   "
#RUN R -e " install.packages('rmapshaper')   "


#RUN R -e " install.packages('shinymanager')   "

RUN R -e "paste(installed.packages()[,1])"

#COPY polbnda_dza.json /srv/shiny-server/polbnda_dza.json


COPY app.R /srv/shiny-server/app.R



COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]
