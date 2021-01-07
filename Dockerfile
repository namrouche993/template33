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
  zlib1g-dev


## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
    


# libcurl4-openssl-dev \
#  libcurl4-gnutls-dev \



# Base image https://hub.docker.com/u/rocker/
#FROM rocker/shiny:latest 

# system libraries of general use
## install debian packages
# RUN apt-get update -qq && apt-get -y --no-install-recommends install \
#    libicu-dev \
#    libglpk-dev \
#    libgmp3-dev \
#    libxml2-dev \
#    libcairo2-dev \
#    libsqlite3-dev \
#    libmariadbd-dev \
#    libpq-dev \
#    libssh2-1-dev \
#    unixodbc-dev \
#    libcurl4-openssl-dev \
#    libssl-dev \
#    zlib1g-dev



#apt-get install -y libicu-dev
#apt-get install -y libglpk-dev
#apt-get install -y libgmp3-dev
#apt-get install -y libxml2-dev
#apt-get install -y libcurl4-openssl-dev
#apt-get install -y libssl-dev
######apt-get install -y zlib1g-dev



## update system libraries
#RUN apt-get update && \
#    apt-get upgrade -y && \
#    apt-get clean
    
    






# sudo \
#   gdebi-core \
#   pandoc \
#   pandoc-citeproc \
#   libcurl4-gnutls-dev \
#   libcairo2-dev \
#   libxt-dev \
#   xtail \
#    wget \
#    libudunits2-dev \
#    libgdal-dev


RUN R -e "install.packages('shiny', dependencies = TRUE)"
 #RUN R -e "install.packages('rlist', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('zoo', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('xts', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('quantmod', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('igraph', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('devtools', repos='http://cran.rstudio.com/')"
 RUN R -e "install.packages('remotes', dependencies = TRUE)"


# RUN R -e "install.packages('assertthat', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('broom', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('htmltools', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('htmlwidgets', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('jsonlite', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('purrr', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('rjson', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('rlang', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('stringr', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('tibble', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('tidyr', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('yaml', repos='http://cran.rstudio.com/')"


# RUN R -e "install.packages('knitr', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('rmarkdown', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('survival', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('viridisLite', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('gapminder', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('forecast', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('protolite', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('jqr', repos='http://cran.rstudio.com/')"


 #RUN R -e "install.packages('geojsonio', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('testthat', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('covr', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('spelling', repos='http://cran.rstudio.com/')"


# RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reactable', repos='http://cran.rstudio.com/')"


#RUN R -e "install.packages('highcharter', dependencies = TRUE)"

RUN R -e "remotes::install_github('jbkunst/highcharter',dependencies = TRUE)"
RUN R -e "library(highcharter)"
RUN R -e "ab='USMISTEO'"
RUN R -e "paste(ab)"

#RUN R -e "install.packages('https://packagemanager.rstudio.com/all/latest/src/contrib/Archive/highcharter/highcharter_0.7.0.tar.gz')"


COPY app.R /srv/shiny-server/app.R


COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]
