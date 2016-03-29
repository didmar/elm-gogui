FROM node:0.12.4

RUN npm install -g elm
ENV ELM_HOME /usr/local/lib/node_modules/elm/share
RUN apt-get update && apt-get install -y \
         locales
RUN dpkg-reconfigure locales && \
         locale-gen C.UTF-8 && \
         update-locale LANG=C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update \
    && apt-get install -y git \
    && rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/didmar/elm-gogui.git /opt/elm-gogui

WORKDIR /opt/elm-gogui
RUN elm-package install -y
RUN elm-make go.elm

EXPOSE 8000
CMD ["elm-reactor"]
