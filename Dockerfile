FROM r-base

RUN apt-get update -y && apt-get install zsh -y
RUN PATH="$PATH:/usr/bin/zsh"

RUN mkdir /code
WORKDIR /code

RUN apt update
RUN apt-get install -y libcurl4-openssl-dev

RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('scales')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('knitr')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('ggrepel')"
RUN R -e "install.packages('forecast')"
RUN R -e "remotes::install_github('shane-kercheval/rtools')"
RUN R -e "install.packages('languageserver')"
