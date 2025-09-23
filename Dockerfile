FROM madnificent/lisp-webservice:0.6.0

RUN apt-get update; apt-get upgrade -y; apt-get install -y curl


RUN echo "Installing qlot"; \
    cd /tmp/ && wget https://github.com/fukamachi/qlot/releases/download/1.7.2/qlot-1.7.2.tar.gz && tar xfz qlot-1.7.2.tar.gz && cd qlot && scripts/setup.sh && scripts/install.sh

COPY ./launch-odrl-parser.sh /

COPY . /app

# qlot commands must be run in folder with .qlfile
WORKDIR /app
RUN qlot install

ENV BOOT=odrl-parser

RUN qlot exec sbcl --load /usr/src/load.lisp

WORKDIR /
CMD ["/launch-odrl-parser.sh"]
