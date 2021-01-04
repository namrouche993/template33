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

RUN R -e "install.packages('devtools', repos='http://cran.rstudio.com/')"

RUN R -e "install.packages('igraph')"
RUN R -e "install.packages('zoo')"
RUN R -e "install.packages('highcharter')"
RUN R -e "install.packages('farver', repos='http://cran.rstudio.com/')"


RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readxl', repos='http://cran.rstudio.com/')"

RUN R -e "install.packages('fresh', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyBS', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet.extras', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rgdal', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sp', repos='http://cran.rstudio.com/')"

RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('excelR', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reactable', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"


COPY app.R /srv/shiny-server/app.R
COPY livraison_wilayas.xlsx /srv/shiny-server/livraison_wilayas.xlsx

COPY livraison_wilayas.xlsx /srv/shiny-server/livraison_wilayas.xlsx
COPY equip0.xlsx /srv/shiny-server/equip0.xlsx
COPY Estimation_Population_TOL_Parc_par_Wilaya.xlsx /srv/shiny-server/Estimation_Population_TOL_Parc_par_Wilaya.xlsx
COPY pos.xlsx /srv/shiny-server/pos.xlsx
COPY sitphy.xlsx /srv/shiny-server/sitphy.xlsx

COPY sitfin.xlsx /srv/shiny-server/sitfin.xlsx

COPY sitpro.xlsx /srv/shiny-server/sitpro.xlsx

COPY zones2.xlsx /srv/shiny-server/zones2.xlsx
COPY polbnda_dza.json /srv/shiny-server/polbnda_dza.json


COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]
