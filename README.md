emqx_mysql
===============

Datastore with MySQL.

Notice: changed mysql driver to [mysql-otp](https://github.com/mysql-otp/mysql-otp).

Build Plugin
-------------

make && make tests

Configure Plugin
----------------

File: etc/emqx_mysql.conf

Import mqtt.sql
---------------

Import mqtt.sql into your database.

Load Plugin
-----------

./bin/emqx_ctl plugins load emqx_mysql

License
-------

Apache License Version 2.0

Author
------

Tumi.
