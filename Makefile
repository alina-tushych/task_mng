PROJECT = task_mng
PROJECT_DESCRIPTION = task system for support or teams
PROJECT_VERSION = $(PROJECT_BUILD_TAG)

# Compiler options.
ERLC_OPTS += "+{parse_transform}"

GIT_PATH = git https://github.com

REBAR_GIT = https://github.com/rebar/rebar.git

DEPS = jsone cowboy

dep_jsone                         = $(GIT_PATH)/sile/jsone.git                 master
dep_cowboy                        = $(GIT_PATH)/ninenines/cowboy               1.0.4

ENV ?= devel

ifeq ($(ENV),devel)
ERLC_OPTS += +debug_info
endif

RELX_OUTPUT_DIR ?= ./_rel/$(ENV)


include erlang.mk

# Generating rebar.config
#app:: rebar.config

.PHONY: debug

debug: app rel
	./_rel/devel/$(PROJECT)/bin/$(PROJECT) console

CT_LOGS_DIR = log/ct_log
CT_OPTS += -erl_args -boot start_sasl -sname task_mng
