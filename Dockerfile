FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    git

RUN R -e "install.packages(c('devtools', 'shiny', 'bslib', 'tidyverse', 'eiatools', 'DT', 'rlang', 'rhandsontable', 'plotly', 'shinyalert', 'lubridate', 'tidyquant', 'facmodCS', 'moments'), dependencies = TRUE, repos = 'https://packagemanager.rstudio.com/cran/latest')"
# RUN R -e "devtools::install_github('https://github.com/jspowley/eiatools')"

RUN git clone https://github.com/jspowley/bond_app.git /srv/shiny-server/bond_app
RUN chown -R shiny:shiny /srv/shiny-server/bond_app

EXPOSE 3838

#CMD ["/init"]
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/bond_app/', host = '0.0.0.0', port = 3838)"]
