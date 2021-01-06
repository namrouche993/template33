FROM rocker/tidyverse:3.4
MAINTAINER Jaehyeon Kim <dottami@gmail.com>

#### build from https://github.com/rocker-org/shiny in production
#### need to build httpuv from source when installing shiny from GitHub
#### procps for monitoring
RUN apt-get update \
  && apt-get install -y autoconf automake libtool procps

#### promises and some necessary packages
#### https://rstudio.github.io/promises/index.html
RUN R -e 'devtools::install_github("rstudio/shiny")' \
  && R -e 'devtools::install_github("rstudio/rmarkdown")' \
  && R -e 'devtools::install_github("ramnathv/htmlwidgets@async")' \
  && R -e 'devtools::install_github("jcheng5/plotly@joe/feature/async")' \
  && R -e 'devtools::install_github("rstudio/DT@async")' \
  && R -e 'install.packages("highcharter")' \
  && R -e 'install.packages("future")'

    
RUN R -e "paste('le GETWDDDDDDDDDDDDDDDDD  EGALEEE   A  : ',getwd() )"

COPY highcharter_0.8.2.tar.gz /srv/shiny-server/highcharter_0.8.2.tar.gz

#RUN R -e "install.packages('rgdal')"


RUN R -e "install.packages('devtools')"

#RUN R -e "install.packages('igraph')"
RUN R -e "install.packages('zoo')"
RUN R -e "install.packages('remotes')"
RUN R -e "install.packages('farver')"

RUN R -e "install.packages('rlist')"
RUN R -e "install.packages('xts')"
RUN R -e "install.packages('quantmod')"
RUN R -e "install.packages('rjson')"

RUN R -e "install.packages(c('vctrs','rlang','backports','data.table','jsonlite','broom','htmlwidgets'))"


#RUN R -e "install.packages('https://cran.r-project.org/bin/macosx/contrib/4.0/highcharter_0.8.2.tgz')"



RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"


# RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')"
# RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('readxl', repos='http://cran.rstudio.com/')"

 #RUN R -e "install.packages('fresh', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('shinyBS', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('leaflet.extras', repos='http://cran.rstudio.com/')"
 #RUN R -e "install.packages('sp', repos='http://cran.rstudio.com/')"

#RUN R -e "install.packages('https://packagemanager.rstudio.com/all/__linux__/focal/latest/src/contrib/highcharter_0.8.2.tar.gz')"


#RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"


#RUN R -e "install.packages('excelR', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('reactable', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"



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

COPY app.R /srv/shiny-server/app.R


COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]
