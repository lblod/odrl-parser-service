FROM madnificent/lisp-webservice:0.6.0

RUN apt-get update; apt-get upgrade -y; apt-get install -y curl


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
