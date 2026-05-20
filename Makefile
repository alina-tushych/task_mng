PROJECT = task_mng
PROJECT_DESCRIPTION = task system for support or teams
PROJECT_VERSION = $(PROJECT_BUILD_TAG)

ERLC_OPTS += "+{parse_transform}"

GIT_PATH = git https://github.com

REBAR_GIT = https://github.com/rebar/rebar.git

DEPS = jsone cowboy epgsql

dep_jsone                         = $(GIT_PATH)/sile/jsone.git                 1.9.0
dep_cowboy                        = $(GIT_PATH)/ninenines/cowboy.git           2.14.0
dep_epgsql                        = $(GIT_PATH)/epgsql/epgsql.git              4.8.0

ENV ?= devel

ifeq ($(ENV),devel)
ERLC_OPTS += +debug_info
endif

RELX_OUTPUT_DIR ?= ./_rel/$(ENV)

include erlang.mk
