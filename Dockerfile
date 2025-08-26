FROM fukamachi/qlot AS qlot-env

WORKDIR /app
# TODO: make asd filename configurable
COPY qlfile* odrl-parser.asd ./
RUN qlot bundle

FROM madnificent/lisp-webservice:0.6.0

RUN apt-get update; apt-get upgrade -y; apt-get install -y curl

# Copy .bundle-libs directory
COPY --from=qlot-env /app/.bundle-libs /app/.bundle-libs

COPY . /app
ENV BOOT=odrl-parser

RUN sbcl --load .bundle-libs/setup.lisp --load /usr/src/load.lisp

# TODO add setup.lisp loading?
CMD ["/usr/src/startup.sh"]
