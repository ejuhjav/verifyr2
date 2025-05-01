# Use a prebuilt R image with devtools, rmarkdown, etc.
FROM rocker/verse:4.4.1

# Install only covr (not included in verse)
RUN Rscript -e 'install.packages("covr", repos = "https://cloud.r-project.org")'

# Install covr + system libraries for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpoppler-cpp-dev \
    libmagick++-dev \
    libmagickwand-dev \
    && rm -rf /var/lib/apt/lists/*

# Explicitly install the following packages
RUN Rscript -e 'install.packages(c("magick", "pdftools", "htmltools", "DT", "striprtf", "lintr"), repos = "https://cloud.r-project.org")'

# Set working directory inside the container
WORKDIR /workdir


