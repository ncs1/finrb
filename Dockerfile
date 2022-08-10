FROM ruby:3.1.2 AS development

LABEL maintainer="nadircs11@gmail.co.il"

RUN dpkg --add-architecture i386

SHELL [ "/bin/bash", "-c" ]

RUN apt update -yqq && \
  apt install --no-install-recommends -yqq nano apt-utils locales && \
  apt clean && \
  rm -rf /var/lib/apt/lists/*

RUN gem install bundler
RUN bundle config --global jobs 16

RUN mkdir -pv /app
WORKDIR /app

RUN mkdir -pv ./lib/
COPY *.gemspec ./
COPY Gemfile* ./
RUN bundle install

WORKDIR /app

COPY . .

ENTRYPOINT ["/bin/bash", "-c", "/bin/bash"]

FROM development AS testing

RUN bundle exec rake || exit 0

ENTRYPOINT ["/bin/bash", "-c", "/bin/bash"]
