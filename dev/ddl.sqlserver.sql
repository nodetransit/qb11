BEGIN TRANSACTION

ALTER TABLE t_users DROP CONSTRAINT IF EXISTS t_users_m_levels_fk;
ALTER TABLE t_user_infos DROP CONSTRAINT IF EXISTS t_user_infos_t_users_fk;
ALTER TABLE t_jobs DROP CONSTRAINT IF EXISTS t_jobs_t_users_fk;
ALTER TABLE t_jobs DROP CONSTRAINT IF EXISTS t_jobs_m_job_types_fk;
ALTER TABLE t_job_tags DROP CONSTRAINT IF EXISTS t_jobs_t_jobs_tags_fk;
ALTER TABLE t_job_tags DROP CONSTRAINT IF EXISTS t_job_tags_t_jobs_fk;

DROP TABLE IF EXISTS t_users;

CREATE TABLE t_users
(
    id integer NOT NULL IDENTITY(1, 1),
    level_id integer NOT NULL,
    email character varying(128) NOT NULL,
    registered datetime2(6) NOT NULL,
    deleted datetime2(6),
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS t_user_infos;

CREATE TABLE t_user_infos
(
    id integer NOT NULL IDENTITY(1, 1),
    user_id integer NOT NULL,
    name character varying(24) NOT NULL,
    country character varying(52),
    address character varying(52),
    telephone character varying(18),
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS t_jobs;

CREATE TABLE t_jobs
(
    id integer NOT NULL IDENTITY(1, 1),
    job_type_id integer NOT NULL,
    user_id integer NOT NULL,
    date datetime2(6) NOT NULL,
    successful bit,
    retries integer,
    cancelled datetime2(6),
    cancel_reason varchar(128),
    failed datetime2(6),
    fail_reason varchar(128),
    PRIMARY KEY (id)
);


DROP TABLE IF EXISTS t_tags;

CREATE TABLE t_tags
(
    id integer NOT NULL IDENTITY(1, 1),
    name varchar(12) NOT NULL,
    description text,
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS m_levels;

CREATE TABLE m_levels
(
    id integer NOT NULL,
    name varchar(18) NOT NULL,
    description text,
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS m_job_types;

CREATE TABLE m_job_types
(
    id integer NOT NULL,
    name varchar(18) NOT NULL,
    description text,
    PRIMARY KEY (id)
);

DROP TABLE IF EXISTS t_job_tags;

CREATE TABLE t_job_tags
(
    id integer NOT NULL IDENTITY(1, 1),
    job_id integer NOT NULL,
    tag_id integer NOT NULL,
    PRIMARY KEY (id)
);

ALTER TABLE t_users
    ADD CONSTRAINT t_users_m_levels_fk FOREIGN KEY (level_id)
    REFERENCES m_levels(id)
    ON UPDATE CASCADE
    ON DELETE NO ACTION;
CREATE INDEX t_users_m_levels_fk_index
	ON t_users(level_id);

ALTER TABLE t_user_infos
    ADD CONSTRAINT t_user_infos_t_users_fk FOREIGN KEY (user_id)
    REFERENCES t_users(id)
    ON UPDATE CASCADE
    ON DELETE NO ACTION;
CREATE INDEX  t_user_infos_t_users_fk_index
	ON t_user_infos(user_id);

ALTER TABLE t_jobs
    ADD CONSTRAINT t_jobs_t_users_fk FOREIGN KEY (user_id)
    REFERENCES t_users(id)
    ON UPDATE CASCADE
    ON DELETE NO ACTION;
CREATE INDEX  t_jobs_t_users_fk_index
	ON t_jobs(user_id);

ALTER TABLE t_jobs
    ADD CONSTRAINT t_jobs_m_job_types_fk FOREIGN KEY (job_type_id)
    REFERENCES m_job_types(id)
    ON UPDATE CASCADE
    ON DELETE NO ACTION;
CREATE INDEX  t_jobs_m_job_types_fk_index
	ON t_jobs(job_type_id);

ALTER TABLE t_job_tags
    ADD CONSTRAINT t_jobs_t_jobs_tags_fk FOREIGN KEY (job_id)
    REFERENCES t_jobs(id)
    ON UPDATE CASCADE
    ON DELETE NO ACTION;
CREATE INDEX  t_jobs_t_jobs_tags_fk_index
	ON t_job_tags(job_id);

ALTER TABLE t_job_tags
    ADD CONSTRAINT t_job_tags_t_jobs_fk FOREIGN KEY (tag_id)
    REFERENCES t_tags(id)
    ON UPDATE CASCADE
    ON DELETE NO ACTION;
CREATE INDEX  t_job_tags_t_jobs_fk_index
	ON t_job_tags(tag_id);

COMMIT TRANSACTION
