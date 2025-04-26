FROM rocker/verse:4.4.1

# Install covr package (if not already installed)
RUN Rscript -e 'install.packages("covr", repos = "https://cloud.r-project.org")'

# Set working directory (optional)
WORKDIR /workdir

