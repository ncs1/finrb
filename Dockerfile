ARG RUBY_VER=4.0

FROM ruby:${RUBY_VER} AS development

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

RUN bundle exec rake

ENTRYPOINT ["/bin/bash", "-c", "/bin/bash"]

FROM development AS solver-validation

RUN apt update -yqq && \
  apt install --no-install-recommends -yqq python3 python3-venv && \
  apt clean && \
  rm -rf /var/lib/apt/lists/*

RUN python3 -m venv /opt/finrb-solver-validation && \
  /opt/finrb-solver-validation/bin/python -m pip install --no-cache-dir \
    --requirement script/requirements-cross-validation.txt

ENV PATH="/opt/finrb-solver-validation/bin:${PATH}"
ENV PYTHON="/opt/finrb-solver-validation/bin/python"

ENTRYPOINT []
CMD ["bundle", "exec", "rake", "oracle:cross_validate"]
