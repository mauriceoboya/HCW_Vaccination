# Use the rocker/shiny:4 image as the base image
FROM rocker/shiny:4

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinymanager', 'shinyjs', 'dplyr', 'DT', 'sf', 'ggplot2', 'leaflet'), repos='https://cloud.r-project.org/')"

# Create a directory for the app
WORKDIR /srv/shiny-server

# Copy the app files into the container
COPY ./ken_adm_iebc_20191031_shp /srv/shiny-server/ken_adm_iebc_20191031_shp
COPY ./HCWDashboard.rds /srv/shiny-server/
COPY ./app.R /srv/shiny-server/

# Expose port 3838 for Shiny Server
EXPOSE 3838

# Run the Shiny Server
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', port = 3838, host = '0.0.0.0')"]

