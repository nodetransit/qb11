SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='ONLY_FULL_GROUP_BY,STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';

-- -----------------------------------------------------
-- Schema mydb
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Table `t_tags`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `t_tags` ;

CREATE TABLE IF NOT EXISTS `t_tags` (
  `id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(12) NOT NULL,
  `description` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`id`));


-- -----------------------------------------------------
-- Table `m_levels`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `m_levels` ;

CREATE TABLE IF NOT EXISTS `m_levels` (
  `id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(18) NOT NULL,
  `description` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`id`));


-- -----------------------------------------------------
-- Table `m_job_types`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `m_job_types` ;

CREATE TABLE IF NOT EXISTS `m_job_types` (
  `id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(18) NOT NULL,
  `description` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`id`));


-- -----------------------------------------------------
-- Table `t_users`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `t_users` ;

CREATE TABLE IF NOT EXISTS `t_users` (
  `id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `lid` INT UNSIGNED NOT NULL,
  `email` VARCHAR(128) NOT NULL,
  `registered` DATE NOT NULL,
  `deleted` DATE NULL DEFAULT NULL,
  PRIMARY KEY (`id`),
  INDEX `t_users_FKIndex1` (`lid` ASC) VISIBLE,
  CONSTRAINT ``
    FOREIGN KEY (`lid`)
    REFERENCES `m_levels` (`id`)
    ON DELETE RESTRICT
    ON UPDATE CASCADE);


-- -----------------------------------------------------
-- Table `t_user_infos`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `t_user_infos` ;

CREATE TABLE IF NOT EXISTS `t_user_infos` (
  `id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `uid` INT UNSIGNED NOT NULL,
  `name` VARCHAR(24) NOT NULL,
  `country` VARCHAR(52) NULL DEFAULT NULL,
  `address` VARCHAR(52) NULL DEFAULT NULL,
  `telephone` VARCHAR(18) NULL DEFAULT NULL,
  PRIMARY KEY (`id`),
  INDEX `user_infos_FKIndex1` (`uid` ASC) VISIBLE,
  CONSTRAINT ``
    FOREIGN KEY (`uid`)
    REFERENCES `t_users` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);


-- -----------------------------------------------------
-- Table `t_jobs`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `t_jobs` ;

CREATE TABLE IF NOT EXISTS `t_jobs` (
  `id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `jtid` INT UNSIGNED NOT NULL,
  `uid` INT UNSIGNED NOT NULL,
  `date` DATE NOT NULL,
  `successful` TINYINT NULL DEFAULT NULL,
  `retries` INT UNSIGNED NULL DEFAULT NULL,
  `canceled` DATE NULL DEFAULT NULL,
  `cancel_reason` VARCHAR(128) NULL DEFAULT NULL,
  `failed` DATE NULL DEFAULT NULL,
  `fail_reason` VARCHAR(128) NULL DEFAULT NULL,
  PRIMARY KEY (`id`),
  INDEX `transactions_FKIndex1` (`uid` ASC) VISIBLE,
  INDEX `t_transactions_FKIndex2` (`jtid` ASC) VISIBLE,
  CONSTRAINT ``
    FOREIGN KEY (`uid`)
    REFERENCES `t_users` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT ``
    FOREIGN KEY (`jtid`)
    REFERENCES `m_job_types` (`id`)
    ON DELETE RESTRICT
    ON UPDATE CASCADE);


-- -----------------------------------------------------
-- Table `t_job_tags`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `t_job_tags` ;

CREATE TABLE IF NOT EXISTS `t_job_tags` (
  `id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `tid` INT UNSIGNED NOT NULL,
  `jid` INT UNSIGNED NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `t_transaction_tags_FKIndex1` (`jid` ASC) VISIBLE,
  INDEX `t_transaction_tags_FKIndex2` (`tid` ASC) VISIBLE,
  CONSTRAINT ``
    FOREIGN KEY (`jid`)
    REFERENCES `t_jobs` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT ``
    FOREIGN KEY (`tid`)
    REFERENCES `t_tags` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
