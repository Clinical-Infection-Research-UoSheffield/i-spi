# Use an official R runtime as a parent image
FROM rocker/tidyverse:4.4.1
LABEL org.opencontainers.image.source=https://github.com/hoenlab/i-spi

# Install any needed packages specified in requirements.txt
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    vim \
    jags 


RUN apt-get install libpq-dev -y

# Install Shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment

RUN R -e "install.packages(c('plotly', 'shiny', 'shinyjs', 'shinyalert', 'shinydashboard', 'shinyWidgets', 'shinyFiles', 'shinybusy','shinyBS'))"
RUN R -e "install.packages(c('readxl', 'openxlsx', 'RPostgres', 'glue', 'DBI', 'DT', 'sqldf', 'datamods', 'data.table', 'stringi', 'stringr', 'tidyverse'))"
RUN R -e "install.packages(c('tidyr', 'plyr', 'modelr', 'robustbase', 'broom', 'rhandsontable', 'sendmailR', 'reactable', 'gt', 'gtsummary', 'gtExtras'))"
RUN R -e "install.packages(c('grid', 'gridExtra', 'gtable', 'gtools', 'httr2', 'auth0', 'janitor', 'visNetwork','pdp','ggsci','bslib','lazyeval', 'pdp'))"

RUN apt-get install libgmp-dev -y

RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('biolabntua/moach')"


RUN R -e "install.packages(c('ks', 'scales', 'sp'))"


# Install system-level dependencies
RUN apt-get update && apt-get install -y \
    tcl-dev \
    tk-dev


# Install R packages from CRAN
RUN R -e "install.packages('aplpack')"

# Install R packages from GitHub
RUN R -e "remotes::install_github('hardikguptadartmouth/weird-package')"
RUN R -e "remotes::install_github('Sayani07/gghdr')"

RUN R -e "install.packages('cgam')"
RUN R -e "install.packages('mgcv')"
RUN R -e "install.packages('extras')"
RUN R -e "install.packages('Polychrome')"

RUN R -e "install.packages(c('socviz', 'magrittr', 'car', 'nlraa', 'nlstools', 'shinyWidgets', 'formattable', 'drda', 'future', 'promises'), repos='http://cran.rstudio.com/')"


RUN R -e "remotes::install_github('hardikguptadartmouth/shinyjqui')"

RUN apt-get install rustc -y
RUN apt-get install cargo -y

RUN R -e "install.packages(c('progressr', 'fracture'))"

RUN R -e "install.packages(c('labelled', 'flexmix', 'factoextra', 'cluster'))"

RUN R -e "install.packages('dendextend', repos='https://cloud.r-project.org/')"

RUN R -e "install.packages('heatmaply')"

RUN R -e "install.packages(c('data.tree', 'DiagrammeR', 'tidyr'))"

RUN R -e "remotes::install_github('dreamRs/datamods')"


RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/b64/b64_0.1.3.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('downloadthis', repos = 'https://cran.r-project.org')"

RUN R -e "install.packages('shinyFeedback')"

RUN R -e "install.packages('later')"
RUN R -e "install.packages('tmvtnorm')"

RUN R -e "install.packages(c('httr2', 'jose', 'openssl', 'jsonlite', 'urltools'))"
RUN R -e "install.packages(c('strex', 'purrr'))"
RUN R -e "install.packages('shiny.destroy')"
RUN R -e "install.packages(c('viridis', 'htmltools'))"
RUN R -e "install.packages(c('minpack.lm', 'Deriv'))"
RUN R -e "install.packages('nls.multstart')"
RUN R -e "install.packages(c('ggrepel', 'cowplot'))"
RUN R -e "install.packages('patchwork')"
RUN R -e "install.packages('digest')"
RUN R -e "install.packages('rjags')"
RUN R -e "install.packages('bit64')"
RUN R -e "install.packages('shinycssloaders')"


# Install stanassay from public GitHub (immunoplex/stanassay)
RUN R -e "remotes::install_github('immunoplex/stanassay', upgrade='never')"

RUN rm -rf /srv/shiny-server/*

# Copy the app directory into the image
COPY ./src/ /srv/shiny-server/

WORKDIR /srv/shiny-server

# Set environment and start Shiny server
CMD ["bash", "-c", "env > /srv/shiny-server/.Renviron && /usr/bin/shiny-server"]

