  FROM rocker/shiny-verse:latest

 RUN apt-get update && apt-get upgrade -y && apt-get install -y  \
     sudo \
    pandoc \
     pandoc-citeproc \
     libcurl4-gnutls-dev \
     libcairo2-dev \
     libxt-dev \
     libssl-dev \
     libssh2-1-dev

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


RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rlist', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('zoo', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('xts', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('quantmod', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('igraph', repos='http://cran.rstudio.com/')"


RUN R -e "install.packages('https://packagemanager.rstudio.com/all/latest/src/contrib/Archive/highcharter/highcharter_0.7.0.tar.gz')"


COPY app.R /srv/shiny-server/app.R


COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]
