FROM madnificent/lisp-webservice:0.6.0

RUN apt-get update; apt-get upgrade -y; apt-get install -y curl gcc

ARG TARGETPLATFORM
RUN if [ "$TARGETPLATFORM" = "linux/amd64" ]; then \
      echo "BUILDING FOR AMD64 through $TARGETPLATFORM"; cd /tmp/ && wget https://github.com/watchexec/watchexec/releases/download/v2.3.2/watchexec-2.3.2-x86_64-unknown-linux-gnu.deb && dpkg -i watchexec-2.3.2-x86_64-unknown-linux-gnu.deb; \
    elif [ "$TARGETPLATFORM" = "linux/arm64" ]; then \
      echo "BUILDING FOR ARM64" && cd /tmp/ && wget https://github.com/watchexec/watchexec/releases/download/v2.3.2/watchexec-2.3.2-aarch64-unknown-linux-gnu.deb && dpkg -i watchexec-2.3.2-aarch64-unknown-linux-gnu.deb; \
    elif [ "$TARGETPLATFORM" = "" ]; then \
      echo "BUILDING FOR AMD64 through $TARGETPLATFORM"; cd /tmp/ && wget https://github.com/watchexec/watchexec/releases/download/v2.3.2/watchexec-2.3.2-x86_64-unknown-linux-gnu.deb && dpkg -i watchexec-2.3.2-x86_64-unknown-linux-gnu.deb; \
    else \
      echo "Target platform \"$TARGETPLATFORM\" not supported, check watchexec" ; \
      exit 1; \
    fi

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
