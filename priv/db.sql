CREATE DATABASE task_mng;

CREATE TABLE public.users
(
    user_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    login character varying(128) COLLATE pg_catalog."default",
    password character varying(128) COLLATE pg_catalog."default",
    role character varying(128) COLLATE pg_catalog."default",
    work_status character varying(128) COLLATE pg_catalog."default",
    last_login timestamp DEFAULT NULL,
    deleted character varying(128) COLLATE pg_catalog."default"
);

CREATE TABLE public.activities
(
    user_id bigint NOT NULL REFERENCES users (user_id),
    login_dt timestamp DEFAULT NULL,
    logout_dt timestamp DEFAULT NULL
);
