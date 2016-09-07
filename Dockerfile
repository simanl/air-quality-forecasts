FROM r-base:3.2.1
MAINTAINER Roberto Quintanilla <vov@icalialabs.com>

# Explicitly set user/group IDs
RUN groupadd -r pronosticos --gid=999 \
  && useradd -r -g pronosticos --uid=999 pronosticos

# Grab gosu for easy step-down from root
RUN set -x \
  && export GOSU_VERSION=1.7 \
	&& wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$(dpkg --print-architecture)" \
	&& wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$(dpkg --print-architecture).asc" \
	&& export GNUPGHOME="$(mktemp -d)" \
	&& gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
	&& gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
	&& rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc \
	&& chmod +x /usr/local/bin/gosu \
	&& gosu nobody true

ADD . /app
WORKDIR /app
ENV SPCA_VERSION=1.0.0.rc1 SPCA_HOME=/app PATH=/app/bin:$PATH

# Run the Packrat script:
RUN set -x \
  && Rscript start.R -- --version --bootstrap-packrat \
  && chmod 700 /app \
  && chown -R pronosticos:pronosticos /app

# Expose the RServe port:
EXPOSE 6311

# Set /app/tablas as a volume:
VOLUME /app/tablas

ENTRYPOINT ["/app/entrypoint.sh"]

CMD ["Rscript", "start.R"]
