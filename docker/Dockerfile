FROM jemstep/docker-scala
MAINTAINER Jemstep Dev <dev@jemstep.com>

VOLUME ["/code/zio-example"]
ENTRYPOINT ["/bin/bash"]

RUN locale-gen en_US.UTF-8
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

WORKDIR /code/zio-example
