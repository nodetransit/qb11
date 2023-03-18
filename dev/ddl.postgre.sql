BEGIN;


DROP TABLE IF EXISTS public.t_users CASCADE;

CREATE TABLE IF NOT EXISTS public.t_users
(
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY,
    level_id integer NOT NULL,
    email character varying(128) NOT NULL,
    registered timestamp without time zone NOT NULL,
    deleted timestamp without time zone,
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS public.t_user_infos CASCADE;

CREATE TABLE IF NOT EXISTS public.t_user_infos
(
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY,
    user_id integer NOT NULL,
    name character varying(24) NOT NULL,
    country character varying(52),
    address character varying(52),
    telephone character varying(18),
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS public.t_jobs CASCADE;

CREATE TABLE IF NOT EXISTS public.t_jobs
(
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY,
    job_type_id integer NOT NULL,
    user_id integer NOT NULL,
    date timestamp without time zone NOT NULL,
    successful boolean,
    retries integer,
    cancelled timestamp without time zone,
    cancel_reason character varying(128)[],
    failed timestamp without time zone,
    fail_reason character varying(128)[],
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS public.t_tags CASCADE;

CREATE TABLE IF NOT EXISTS public.t_tags
(
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY,
    name character varying(12) NOT NULL,
    description text,
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS public.m_levels;

CREATE TABLE IF NOT EXISTS public.m_levels
(
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY,
    name character varying(18) NOT NULL,
    description text,
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS public.m_job_types CASCADE;

CREATE TABLE IF NOT EXISTS public.m_job_types
(
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY,
    name character varying(18) NOT NULL,
    description text,
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS public.t_jobs_tags CASCADE;

CREATE TABLE IF NOT EXISTS public.t_jobs_tags
(
    id integer NOT NULL GENERATED ALWAYS AS IDENTITY,
    job_id integer NOT NULL,
    tag_id integer NOT NULL,
    PRIMARY KEY (id)
);

ALTER TABLE IF EXISTS public.t_users
    ADD FOREIGN KEY (level_id)
    REFERENCES public.m_levels (id) MATCH SIMPLE
    ON UPDATE CASCADE
    ON DELETE RESTRICT
    NOT VALID;
CREATE INDEX IF NOT EXISTS t_users_m_levels_fk_index
    ON public.t_users(level_id);


ALTER TABLE IF EXISTS public.t_user_infos
    ADD FOREIGN KEY (user_id)
    REFERENCES public.t_users (id) MATCH SIMPLE
    ON UPDATE CASCADE
    ON DELETE CASCADE
    NOT VALID;
CREATE INDEX IF NOT EXISTS t_users_t_user_infos_fk_index
    ON public.t_user_infos(user_id);


ALTER TABLE IF EXISTS public.t_jobs
    ADD FOREIGN KEY (user_id)
    REFERENCES public.t_users (id) MATCH SIMPLE
    ON UPDATE CASCADE
    ON DELETE CASCADE
    NOT VALID;
CREATE INDEX IF NOT EXISTS t_jobs_t_users_fk_index
    ON public.t_jobs(user_id);


ALTER TABLE IF EXISTS public.t_jobs
    ADD FOREIGN KEY (job_type_id)
    REFERENCES public.m_job_types (id) MATCH SIMPLE
    ON UPDATE CASCADE
    ON DELETE RESTRICT
    NOT VALID;
CREATE INDEX IF NOT EXISTS t_jobs_m_jobs_types_fk_index
    ON public.t_jobs(job_type_id);


ALTER TABLE IF EXISTS public.t_jobs_tags
    ADD FOREIGN KEY (job_id)
    REFERENCES public.t_jobs (id) MATCH SIMPLE
    ON UPDATE CASCADE
    ON DELETE CASCADE
    NOT VALID;
CREATE INDEX IF NOT EXISTS t_jobs_t_jobs_tags_fk_index
    ON public.t_jobs_tags(job_id);


ALTER TABLE IF EXISTS public.t_jobs_tags
    ADD FOREIGN KEY (job_id)
    REFERENCES public.t_tags (id) MATCH SIMPLE
    ON UPDATE CASCADE
    ON DELETE CASCADE
    NOT VALID;
CREATE INDEX IF NOT EXISTS t_job_tags_t_jobs_fk_index
    ON public.t_jobs_tags(job_id);

END;
