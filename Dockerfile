# Install R version 3.6
FROM r-base:3.6.0

# Install Ubuntu packages
RUN apt-get update && apt-get upgrade -y && apt-get install -y  \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    libudunits2-dev \
    libgdal-dev

# Install R packages that are required
RUN R -e "install.packages(c('shiny'), repos='http://cran.rstudio.com/')"
#RUN R -e "remotes::install_github('nik01010/dashboardthemes')"

# Download and install ShinyServer (latest version)
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/

# Make the ShinyApp available at port 80
EXPOSE 8080

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
