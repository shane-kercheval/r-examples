FROM r-base

RUN apt-get update -y && apt-get install zsh -y
RUN PATH="$PATH:/usr/bin/zsh"

RUN mkdir /code
WORKDIR /code

RUN R -e "install.packages('devtools', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('dplyr', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('tidyverse', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('scales', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('lubridate', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('tidyverse', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggplot2', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('knitr', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('stringr', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggrepel', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('forecast', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "remotes::install_github('shane-kercheval/rtools')"
RUN R -e "install.packages('languageserver', dependencies=TRUE, repos='https://cloud.r-project.org/')"
