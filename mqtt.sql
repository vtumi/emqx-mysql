DROP TABLE IF EXISTS `mqtt_acl`;
CREATE TABLE `mqtt_acl`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `allow` tinyint(0) UNSIGNED NULL DEFAULT NULL COMMENT '0: deny, 1: allow',
  `ipaddr` varchar(60) DEFAULT NULL COMMENT 'IpAddress',
  `username` varchar(100) DEFAULT NULL COMMENT 'Username',
  `access` tinyint(0) UNSIGNED NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',
  `topic` varchar(100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB DEFAULT CHARSET=utf8;

INSERT INTO `mqtt_acl` VALUES (1, 1, NULL, '$all', 2, '#');
INSERT INTO `mqtt_acl` VALUES (2, 0, NULL, '$all', 1, '$SYS/#');
INSERT INTO `mqtt_acl` VALUES (3, 0, NULL, '$all', 1, 'eq #');
INSERT INTO `mqtt_acl` VALUES (4, 1, '127.0.0.1', NULL, 2, '$SYS/#');
INSERT INTO `mqtt_acl` VALUES (5, 1, '127.0.0.1', NULL, 2, '#');
INSERT INTO `mqtt_acl` VALUES (6, 1, NULL, 'dashboard', 1, '$SYS/#');

DROP TABLE IF EXISTS `mqtt_device`;
CREATE TABLE `mqtt_device`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `username` varchar(100) DEFAULT NULL,
  `password` varchar(100) DEFAULT NULL,
  `is_superuser` tinyint(0) UNSIGNED NULL DEFAULT 0,
  `state` tinyint(0) UNSIGNED NULL DEFAULT 0,
  `node` varchar(60) DEFAULT NULL,
  `ipaddr` varchar(60) DEFAULT NULL,
  `online_at` datetime(0) NULL DEFAULT NULL,
  `offline_at` datetime(0) NULL DEFAULT NULL,
  `create_at` timestamp(0) NULL DEFAULT CURRENT_TIMESTAMP(0),
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `mqtt_devicename`(`username`) USING BTREE
) ENGINE = InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `mqtt_msg`;
CREATE TABLE `mqtt_msg`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `mid` varchar(60) DEFAULT NULL,
  `topic` varchar(100) NOT NULL,
  `sender` varchar(100) DEFAULT NULL,
  `node` varchar(60) DEFAULT NULL,
  `ipaddr` varchar(60) DEFAULT NULL,
  `qos` tinyint(0) UNSIGNED NOT NULL DEFAULT 0,
  `retain` tinyint(0) UNSIGNED NULL DEFAULT 0,
  `payload` blob NULL,
  `create_at` timestamp(0) NULL DEFAULT CURRENT_TIMESTAMP(0),
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB DEFAULT CHARSET=utf8;
