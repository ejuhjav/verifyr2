FROM rocker/verse:4.4.1

# (optional) Install coverage tool if not included
RUN install.packages(c("covr"), repos = "https://cloud.r-project.org")

# Set working directory (optional)
WORKDIR /workdir

