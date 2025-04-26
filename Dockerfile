# Use a prebuilt R image with devtools, rmarkdown, etc.
FROM rocker/verse:4.4.1

# Install only covr (not included in verse)
RUN Rscript -e 'install.packages("covr", repos = "https://cloud.r-project.org")'

# System libraries for R packages that need compilation (optional: only if needed)
#RUN apt-get update && apt-get install -y \
    #libcurl4-openssl-dev \
    #libssl-dev \
    #libxml2-dev \
    #&& rm -rf /var/lib/apt/lists/*

# Set working directory inside the container
WORKDIR /workdir


