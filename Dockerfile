FROM haskell:latest

RUN groupadd --gid 1000 foo \
  && useradd --uid 1000 --gid foo --shell /bin/bash --create-home foo

USER 1000

WORKDIR /home/foo/project
