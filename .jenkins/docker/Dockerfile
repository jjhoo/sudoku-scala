FROM debian:bullseye-slim

ENV DEBIAN_FRONTEND noninteractive
ENV LAST_UPDATED 2021-11-27-001

COPY 42local.conf /etc/apt/apt.conf.d/42local.conf

RUN apt-get update
RUN apt-get install -y \
    ca-certificates \
    curl \
    git \
    openjdk-11-jdk-headless \
    python3-pip

COPY sbt.list /etc/apt/sources.list.d/sbt.list
COPY scalasbt-release.gpg /etc/apt/trusted.gpg.d/scalasbt-release.gpg
RUN apt-get update
RUN apt-get install -y \
    sbt

RUN useradd -u 1001 jenkins -m
RUN mkdir /home/jenkins/.sbt

USER jenkins
RUN pip3 install --user codecov

