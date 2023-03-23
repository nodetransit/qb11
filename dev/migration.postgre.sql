INSERT
  INTO m_levels
    ("name", "description")
  VALUES
    ('guest', null),
    ('member', null),
    ('prime member', null),
    ('staff', null),
    ('admin', null),
    ('developer', null),
    ('tester', null);

UPDATE m_levels SET "description" = 'member with additional privileges' WHERE "name" = 'prime member';
UPDATE m_levels SET "description" = 'full access for developers'        WHERE "name" = 'developer';
UPDATE m_levels SET "description" = 'read only access for testers'      WHERE "name" = 'tester';

INSERT
  INTO m_job_types
    ("name", "description")
  VALUES
    ('start', null),
    ('stop', null),
    ('restart', null),
    ('send', null),
    ('resend', null),
    ('receive', null),
    ('verify', null),
    ('clear', null),
    ('review', null),
    ('abort', null),
    ('retry', null),
    ('terminate', null);

-- INSERT INTO t_users ("level_id", "email", "registered") VALUES (1, 'test@email.com', current_timestamp);
-- INSERT INTO t_user_infos ("user_id", "name") VALUES (2, 'test');

GRANT ALL PRIVILEGES ON DATABASE qb11 TO testdata;
GRANT ALL PRIVILEGES ON ALL TABLES in SCHEMA public TO testdata;
