FROM rocker/shiny:3.4.3

RUN R -e "install.packages('ggplot2')"

COPY *.R /srv/shiny-server/TimeDetective/

RUN chmod -R 755 /srv/shiny-server/

EXPOSE 3838
