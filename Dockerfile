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

COPY *.gemspec ./
COPY Gemfile* ./
COPY lib/finrb/version.rb ./lib/finrb/version.rb
RUN bundle install

WORKDIR /app

COPY . .

ENTRYPOINT ["/bin/bash", "-c", "/bin/bash"]

FROM development AS testing

RUN ruby -v && ruby -e 'puts RUBY_DESCRIPTION; puts RUBY_PLATFORM' && bundle exec rake quality

ENTRYPOINT ["/bin/bash", "-c", "/bin/bash"]

FROM development AS solver-verification

RUN apt update -yqq && \
  apt install --no-install-recommends -yqq python3 python3-venv && \
  apt clean && \
  rm -rf /var/lib/apt/lists/*

RUN python3 -m venv /opt/finrb-solver-verification && \
  /opt/finrb-solver-verification/bin/python -m pip install --no-cache-dir \
    --requirement script/requirements-solver-verification.txt

ENV PATH="/opt/finrb-solver-verification/bin:${PATH}"
ENV PYTHON="/opt/finrb-solver-verification/bin/python"

ENTRYPOINT []
CMD ["bundle", "exec", "rake", "solver:verify"]
