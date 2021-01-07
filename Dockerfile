FROM saagie/shiny4saagie

# Install R packages required by your Shiny app
RUN R -e 'install.packages("shiny", repos="http://cran.rstudio.com")'

# Copy your Shiny app to /srv/shiny-server/myapp
COPY app.R /srv/shiny-server/myapp

# Launch Shiny Server
CMD ["/usr/bin/shiny-server.sh"]
