#!/bin/echo docker build . -f
# -*- coding: utf-8 -*-
#
# SPDX-License: ISC
# SPDX-License-URL: https://spdx.org/licenses/ISC.txt

FROM httpd:2.4
LABEL maintainer="Philippe Coval (https://purl.org/rzr)"

ENV DEBIAN_FRONTEND noninteractive
ENV LC_ALL en_US.UTF-8
ENV LANG ${LC_ALL}

RUN echo "#log: Configuring locales" \
 && set -x \
 && apt-get update \
 && apt-get install -y locales \
 && echo "${LC_ALL} UTF-8" | tee /etc/locale.gen \
 && locale-gen ${LC_ALL} \
 && dpkg-reconfigure locales \
 && sync

RUN echo "#log: Preparing system" \
 && set -x \
 && apt-get update -y \
 && apt-get install -y \
  make \
  sudo \
  emacs \
  wget \
  git \
  unzip \
  # EOL
 && sync

ENV project org-reveal
ENV workdir /usr/local/apache2/htdocs/
ADD Makefile ${workdir}/
WORKDIR ${workdir}

RUN echo "#log: Setup ${project}" \
 && set -x \
 && make help \
 && make setup/debian sudo="" \
 && make setup \
 && sync

ADD . ${workdir}/
WORKDIR ${workdir}

RUN echo "#log: Building ${project}" \
 && set -x \
 && make download all \
 && sync
