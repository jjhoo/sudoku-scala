FROM debian:bullseye-slim

ARG JENKINS_UID=1001
ENV DEBIAN_FRONTEND teletype
ENV LAST_UPDATED 2022-08-01-001

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

RUN useradd -u $JENKINS_UID jenkins -m

USER jenkins
WORKDIR /home/jenkins
RUN mkdir /home/jenkins/.sbt
RUN pip3 install --user codecov
