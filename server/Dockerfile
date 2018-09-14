FROM fedora
RUN mkdir -p /opt/myapp/
#ENV dbConnStr="host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres"
ENV secret=rkjnt4ld453kd98fjdk4f
WORKDIR /opt/myapp
RUN dnf -y install \
  libpq-devel

COPY build/poker-server-exe /opt/myapp

#RUN curl -sSL https://get.haskellstack.org/ | sh
#RUN export dbConnStr="host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres"
#CMD ["stack", "build"]
ENV dbConnStr="\"host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres\""
CMD ["./poker-server-exe"]
