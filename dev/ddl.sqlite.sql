DROP TABLE `t_job_tags`;

DROP TABLE `t_jobs`;

DROP TABLE `t_user_infos`;

DROP TABLE `t_users`;

DROP TABLE `m_job_types`;

DROP TABLE `m_levels`;

DROP TABLE `t_tags`;

CREATE TABLE `t_tags` (
  `id` INTEGER  NOT NULL  ,
  `name` VARCHAR(12)  NOT NULL  ,
  `description` TEXT      ,
PRIMARY KEY(`id`));




CREATE TABLE `m_levels` (
  `id` INTEGER  NOT NULL  ,
  `name` VARCHAR(18)  NOT NULL  ,
  `description` TEXT      ,
PRIMARY KEY(`id`));




CREATE TABLE `m_job_types` (
  `id` INTEGER  NOT NULL  ,
  `name` VARCHAR(18)  NOT NULL  ,
  `description` TEXT      ,
PRIMARY KEY(`id`));




CREATE TABLE `t_users` (
  `id` INTEGER  NOT NULL  ,
  `level_id` INTEGER  NOT NULL  ,
  `email` VARCHAR(128)  NOT NULL  ,
  `registered` DATE  NOT NULL  ,
  `deleted` DATE      ,
PRIMARY KEY(`id`)  ,
  FOREIGN KEY(`level_id`)
    REFERENCES `m_levels`(`id`)
      ON DELETE RESTRICT
      ON UPDATE CASCADE);


CREATE INDEX `t_users_FKIndex1` ON `t_users` (`level_id`);


CREATE INDEX IFK_`Rel_03` ON `t_users` (`level_id`);


CREATE TABLE `t_user_infos` (
  `id` INTEGER  NOT NULL  ,
  `user_id` INTEGER  NOT NULL  ,
  `name` VARCHAR(24)  NOT NULL  ,
  `country` VARCHAR(52)    ,
  `address` VARCHAR(52)    ,
  `telephone` VARCHAR(18)      ,
PRIMARY KEY(`id`)  ,
  FOREIGN KEY(`user_id`)
    REFERENCES `t_users`(`id`)
      ON DELETE CASCADE
      ON UPDATE CASCADE);


CREATE INDEX `user_infos_FKIndex1` ON `t_user_infos` (`user_id`);


CREATE INDEX IFK_`Rel_01` ON `t_user_infos` (`user_id`);


CREATE TABLE `t_jobs` (
  `id` INTEGER  NOT NULL  ,
  `job_type_id` INTEGER  NOT NULL  ,
  `uid` INTEGER  NOT NULL  ,
  `date` DATE  NOT NULL  ,
  `successful` BOOL    ,
  `retries` INTEGER    ,
  `canceled` DATE    ,
  `cancel_reason` VARCHAR(128)    ,
  `failed` DATE    ,
  `fail_reason` VARCHAR(128)      ,
PRIMARY KEY(`id`)    ,
  FOREIGN KEY(`uid`)
    REFERENCES `t_users`(`id`)
      ON DELETE CASCADE
      ON UPDATE CASCADE,
  FOREIGN KEY(`job_type_id`)
    REFERENCES `m_job_types`(`id`)
      ON DELETE RESTRICT
      ON UPDATE CASCADE);


CREATE INDEX `transactions_FKIndex1` ON `t_jobs` (`uid`);
CREATE INDEX `t_transactions_FKIndex2` ON `t_jobs` (`job_type_id`);


CREATE INDEX IFK_`Rel_02` ON `t_jobs` (`uid`);
CREATE INDEX IFK_`Rel_04` ON `t_jobs` (`job_type_id`);


CREATE TABLE `t_job_tags` (
  `id` INTEGER  NOT NULL  ,
  `tag_id` INTEGER  NOT NULL  ,
  `job_id` INTEGER  NOT NULL    ,
PRIMARY KEY(`id`)    ,
  FOREIGN KEY(`job_id`)
    REFERENCES `t_jobs`(`id`)
      ON DELETE CASCADE
      ON UPDATE CASCADE,
  FOREIGN KEY(`tag_id`)
    REFERENCES `t_tags`(`id`)
      ON DELETE CASCADE
      ON UPDATE CASCADE);


CREATE INDEX `t_transaction_tags_FKIndex1` ON `t_job_tags` (`job_id`);
CREATE INDEX `t_transaction_tags_FKIndex2` ON `t_job_tags` (`tag_id`);


CREATE INDEX IFK_`Rel_05` ON `t_job_tags` (`job_id`);
CREATE INDEX IFK_`Rel_06` ON `t_job_tags` (`tag_id`);


