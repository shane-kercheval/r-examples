FROM r-base

RUN apt-get update -y && apt-get install zsh -y
RUN PATH="$PATH:/usr/bin/zsh"

RUN mkdir /code
WORKDIR /code

RUN apt update
RUN apt-get install -y libcurl4-openssl-dev

COPY requirements.R .
RUN Rscript requirements.R
