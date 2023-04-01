GRANT ALL PRIVILEGES ON DATABASE qb11 TO testdata;
GRANT ALL PRIVILEGES ON ALL TABLES in SCHEMA public TO testdata;
ALTER TABLE m_levels OWNER TO testdata;
ALTER TABLE m_job_types OWNER TO testdata;
ALTER TABLE t_users OWNER TO testdata;
ALTER TABLE t_user_infos OWNER TO testdata;
ALTER TABLE t_jobs OWNER TO testdata;
ALTER TABLE t_job_tags OWNER TO testdata;
ALTER TABLE t_tags OWNER TO testdata;

-- SELECT SETVAL(pg_get_serial_sequence('t_user_infos', 'id'), (SELECT MAX(id) FROM t_user_infos));
alter table t_users alter column id restart with 100
alter table t_user_infos alter column id restart with 100
alter table t_jobs alter column id restart with 100
alter table t_job_tags alter column id restart with 100
alter table t_tags alter column id restart with 100
alter table m_levels alter column id restart with 100
alter table m_job_types alter column id restart with 100
