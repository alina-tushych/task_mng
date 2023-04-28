CREATE DATABASE task_mng;

CREATE TABLE public.users
(
    user_id bigint NOT NULL PRIMARY KEY,
    login character varying(128) COLLATE pg_catalog."default",
    password character varying(128) COLLATE pg_catalog."default",
    role character varying(128) COLLATE pg_catalog."default",
    work_status character varying(128) COLLATE pg_catalog."default",
    last_login bigint NOT NULL,
    deleted character varying(128) COLLATE pg_catalog."default"
);

CREATE TABLE public.activities
(
    user_id bigint NOT NULL REFERENCES users (user_id),
    login_dt bigint NOT NULL,
    logout_dt bigint NOT NULL
);
