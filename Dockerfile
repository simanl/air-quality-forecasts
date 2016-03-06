FROM r-base:3.2.1
MAINTAINER Roberto Quintanilla <vov@icalialabs.com>

ADD . /app
WORKDIR /app
ENV SPCA_HOME=/app

# Run the Packrat script:
RUN Rscript start.R -- --version --bootstrap-packrat

# Expose the RServe port:
EXPOSE 6311

# Set /app/tablas as a volume:
VOLUME /app/tablas

CMD [ "Rscript", "start.R" ]
