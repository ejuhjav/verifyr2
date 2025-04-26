FROM rocker/r-ver:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    && apt-get clean

# Install devtools and covr
RUN R -e "install.packages(c('devtools', 'covr', 'remotes'), repos = 'https://cloud.r-project.org')"

WORKDIR /app

COPY . /app

# Install R package dependencies
RUN R -e "devtools::install_deps(dependencies = TRUE)"

CMD ["R"]
