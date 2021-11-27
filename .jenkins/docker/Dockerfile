FROM debian:bullseye-slim

COPY sbt.list /etc/apt/sources.list.d/sbt.list
COPY scalasbt-release.gpg /etc/apt/trusted.gpg.d/scalasbt-release.gpg

RUN apt-get update
RUN apt-get install -y \
    curl \
    git \
    openjdk-11-jdk-headless \
    python3-pip \
    sbt

RUN useradd -u 1001 jenkins -m
RUN mkdir /home/jenkins/.sbt

USER jenkins
RUN pip3 install --user codecov

