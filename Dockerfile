FROM madnificent/lisp-webservice:0.6.0

RUN apt-get update; apt-get upgrade -y; apt-get install -y curl gcc

# TODO: Install right version depending on $TARGETPLATFORM
RUN echo "BUILDING FOR AMD64"; cd /tmp/ && wget https://github.com/watchexec/watchexec/releases/download/v2.3.2/watchexec-2.3.2-x86_64-unknown-linux-gnu.deb && dpkg -i watchexec-2.3.2-x86_64-unknown-linux-gnu.deb

RUN curl -L https://qlot.tech/installer | sh

COPY ./launch-odrl-parser.sh /

COPY . /app

# qlot commands must be run in folder with .qlfile
WORKDIR /app
RUN qlot install

ENV BOOT=odrl-parser

RUN qlot exec sbcl --load /usr/src/load.lisp

WORKDIR /
CMD ["/launch-odrl-parser.sh"]
