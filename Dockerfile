FROM erlang:28

WORKDIR /app

RUN apt-get update && apt-get install -y git && rm -rf /var/lib/apt/lists/*

COPY . .

COPY config/docker.sys.config ./config/sys.config

RUN rebar3 release

EXPOSE 8080

CMD ["_build/default/rel/task_mng/bin/task_mng","foreground"]
