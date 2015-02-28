CREATE DATABASE IF NOT EXISTS sockftpd;

CREATE TABLE IF NOT EXISTS `sockftpd`.`files` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` char(255) NOT NULL DEFAULT '',
  `type` char(32) NOT NULL DEFAULT '',
  `url` char(255) NOT NULL DEFAULT '',
  `user` char(32) NOT NULL DEFAULT '',
  `size` bigint(20) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

