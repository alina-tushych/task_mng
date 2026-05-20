# task_mng

--------
### Project Status

This project is under active development.

The API structure, database schema, and internal components may change during development.
Not recommended for production use yet.

--------
![photo_5312094270309647352_y](https://user-images.githubusercontent.com/46717969/231493630-1093fc7d-5f01-4100-97fd-175842bc48ab.jpg)

--------
task_mng - automated task distribution system.

--------
Programming language: Erlang (OTP 28)

--------
### Building via rebar3:
```
$ git clone https://github.com/alina-tushych/task_mng.git
$ cd task_mng
$ rm -rf _build deps
$ rebar3 unlock --all
$ rebar3 get-deps
$ rebar3 compile
$ rebar3 release
$ _build/default/rel/task_mng/bin/task_mng console
```
--------
### Examples:

#### API

curl http://localhost:8080/task_mng/registration -d '{"login":"Alex","password":"my_password", "role":"admin"}' -H "Content-Type: application/json"

curl http://localhost:8080/task_mng/login -d '{"login":"Alex","password":"my_password"}' -H "Content-Type: application/json"

curl http://localhost:8080/task_mng/logout -d '{"user_id":3}' -H "Content-Type: application/json"

curl http://localhost:8080/task_mng/delete -d '{"user_id":1, "role":"admin"}' -H "Content-Type: application/json"

--------
### Contacts:

alinatushych@gmail.com

--------
