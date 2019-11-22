FROM fpco/stack-build-small

RUN mkdir -p /app


COPY . /app

WORKDIR /app

RUN apt-get update && \
  apt-get install libpq-dev lzma-dev libpq-dev -yy

RUN stack build --only-dependencies

RUN stack build 

CMD stack run
