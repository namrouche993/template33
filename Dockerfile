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
RUN R -e "install.packages('devtools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"


RUN R -e "install.packages('assertthat', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('broom', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('htmltools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('htmlwidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('jsonlite', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('purrr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rjson', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rlang', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stringr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tibble', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('yaml', repos='http://cran.rstudio.com/')"


RUN R -e "install.packages('knitr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rmarkdown', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('survival', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('viridisLite', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gapminder', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('forecast', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('geojsonio', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('testthat', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('covr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('spelling', repos='http://cran.rstudio.com/')"


RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reactable', repos='http://cran.rstudio.com/')"


RUN R -e "install.packages('https://cran.r-project.org/src/contrib/highcharter_0.8.2.tar.gz')"

#RUN R -e "install.packages('https://packagemanager.rstudio.com/all/latest/src/contrib/Archive/highcharter/highcharter_0.7.0.tar.gz')"


COPY app.R /srv/shiny-server/app.R


COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]
