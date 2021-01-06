  FROM rocker/shiny-verse:latest

 Install Ubuntu packages
 RUN apt-get update && apt-get upgrade -y && apt-get install -y  \
     sudo \
    pandoc \
     pandoc-citeproc \
     libcurl4-gnutls-dev \
     libcairo2-dev \
     libxt-dev \
     libssl-dev \
     libssh2-1-dev \   
     wget \
     libudunits2-dev \
     libgdal-dev

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





#  FROM rocker/shiny-verse:latest

# system libraries of general use

# RUN apt-get update && apt-get upgrade -y && apt-get install -y  \
#     sudo \
#    pandoc \
#     pandoc-citeproc \
#     libcurl4-gnutls-dev \
#     libcairo2-dev \
#     libxt-dev \
#     libssl-dev \
#     libssh2-1-dev 



RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"

RUN R -e "install.packages('highcharter')"


COPY app.R /srv/shiny-server/app.R


COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]
