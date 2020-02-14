DROP TABLE IF EXISTS `mqtt_acl`;
CREATE TABLE `mqtt_acl` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `allow` int(1) DEFAULT NULL COMMENT '0: deny, 1: allow',
  `ipaddr` varchar(60) DEFAULT NULL COMMENT 'IpAddress',
  `username` varchar(100) DEFAULT NULL COMMENT 'Username',
  `clientid` varchar(100) DEFAULT NULL COMMENT 'ClientId',
  `access` int(2) NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',
  `topic` varchar(100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

INSERT INTO `mqtt_acl` VALUES ('1', '1', null, '$all', null, '2', '#');
INSERT INTO `mqtt_acl` VALUES ('2', '0', null, '$all', null, '1', '$SYS/#');
INSERT INTO `mqtt_acl` VALUES ('3', '0', null, '$all', null, '1', 'eq #');
INSERT INTO `mqtt_acl` VALUES ('4', '1', '127.0.0.1', null, null, '2', '$SYS/#');
INSERT INTO `mqtt_acl` VALUES ('5', '1', '127.0.0.1', null, null, '2', '#');
INSERT INTO `mqtt_acl` VALUES ('6', '1', null, 'dashboard', null, '1', '$SYS/#');

DROP TABLE IF EXISTS `mqtt_device`;
CREATE TABLE `mqtt_device` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(100) DEFAULT NULL,
  `password` varchar(100) DEFAULT NULL,
  `salt` varchar(35) DEFAULT NULL,
  `is_superuser` tinyint(1) DEFAULT '0',
  `is_online` tinyint(1) DEFAULT '0',
  `online_at` datetime DEFAULT NULL,
  `offline_at` datetime DEFAULT NULL,
  `create_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_devicename` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `mqtt_msg`;
CREATE TABLE `mqtt_msg` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `mid` varchar(60) DEFAULT NULL,
  `topic` varchar(100) NOT NULL,
  `sender` varchar(100) DEFAULT NULL,
  `node` varchar(60) DEFAULT NULL,
  `qos` tinyint(1) NOT NULL DEFAULT '0',
  `retain` tinyint(1) DEFAULT NULL,
  `payload` blob,
  `create_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
