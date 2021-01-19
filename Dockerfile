FROM openanalytics/r-base

MAINTAINER Jeffery Painter "jeff@jivecast.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the apps
RUN R -e "install.packages('ggplot2', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('dplyr', repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/appletrees

# our application is in the ./app directory relative to this docker file
COPY appletrees /root/appletrees

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/appletrees')"]
