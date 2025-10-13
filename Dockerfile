FROM madnificent/lisp-webservice:0.6.0

RUN apt-get update; apt-get upgrade -y; apt-get install -y curl


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
