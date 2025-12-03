FROM rstudio/plumber

RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libpng-dev \
    libxml2-dev \
    pandoc \
    && apt-get clean

RUN R -e "install.packages(c('tidyverse', 'tidymodels', 'janitor', 'ggplot2'))"

COPY api.R api.R
COPY data data

EXPOSE 8000

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=8000)"]
