Estuary
=======
Estuary is an application for relaying Avro datum consumed from a RabbitMQ queue,
and storing them in Avro container files locally or in Amazon S3.

.. image:: https://img.shields.io/travis/gmr/estuary.svg
    :target: https://travis-ci.org/gmr/estuary
.. image:: https://img.shields.io/github/release/gmr/estuary.svg
    :target: https://github.com/gmr/estuary/releases

Sending and Storing Events
--------------------------
Events processed by Estuary should be published as individual `Avro <http://avro.apache.org>`_
datum, not container files. The AMQP messages must include two properties:
``content-type`` and ``type``. ``content-type`` must be set to
``application/vnd.apache.avro.datum`` for Estuary to process it. The ``type``
attribute should be set to the Avro record name.

Estuary will automatically accumulate the Avro datum by type into Avro container files
that are stored locally or in Amazon S3. Each message ``type`` will get its own
container file. Container files are created after a configured interval or record
count.

Avro Schemas
^^^^^^^^^^^^
For example, a RabbitMQ message type property value of ``LongList`` will attempt to
load ``LongList.avsc`` from the configured ``avro_schema`` storage location. ``LongList.avsc``
could resemble this schema from the
`Avro documentation <http://avro.apache.org/docs/1.7.7/spec.html#schemas>`_:

.. code:: json

    {
      "type": "record",
      "name": "LongList",
      "fields" : [
        {"name": "value", "type": "long"},
        {"name": "next", "type": ["null", "LongList"]}
      ]
    }

Configuration
-------------
Estuary will look for its configuration file, ``estuary.yaml`` in the current
working directory, ``/etc``, and ``/etc/estuary/``.

The following configuration file will try and fetch Avro schema files from the
``/etc/estuary/schema/`` directory and if it's not found there, it will attempt
to retrieve it from a `Consul <https://consul.io>`_ KV database. Container files
will be stored both locally and in Amazon S3. The storage paths include escape
sequences for creating a dynamic path with elements of the current time, message
type, and hostname. Container files themselves will be stored as ``<epoch>.avro``
where ``<epoch>`` is the UNIX timestamp of when the container file is created.

.. code:: yaml

    %YAML 1.2
    ---
    # Where to look for Avro schema files
    avro_schema:
      path: /etc/estuary/schema
      consul:
        scheme: http
        host: localhost
        port: 8500
        path: /schema/

    # RabbitMQ Configuration
    rabbitmq:
      host: localhost
      port: 5672
      vhost: /
      user: guest
      password: guest
      queue: estuary
      heartbeat: 30
      qty: 2
      prefetch_count: 100

    storage:
      batch_size: 100000
      interval: 900
      filesystem:
        path: /var/lib/estuary/%Y/%m/%d/%k/%{type}/%{host}
      s3:
        access_key_id: <AWS ACCESS KEY VALUE>
        secret_access_key: <AWS SECRET ACCESS KEY VALUE>
        bucket: <BUCKET NAME>
        path: /%Y/%m/%d/%k/%{type}/%{host}

Storage Path Formatting
^^^^^^^^^^^^^^^^^^^^^^^
The following are the escape sequences supported for storage paths:

+--------------+---------------------------------------------------+
| Variable     | Description                                       |
+==============+===================================================+
| ``%y``       | Two digit representation of the year              |
+--------------+---------------------------------------------------+
| ``%Y``       | Four digit representation of the year             |
+--------------+---------------------------------------------------+
| ``%m``       | Two digit representation of the month             |
+--------------+---------------------------------------------------+
| ``%d``       | Two-digit day of the month (with leading zeros)   |
+--------------+---------------------------------------------------+
| ``%k``       | Two digit representation of the hour in 24-hour   |
|              | format, with a space preceding single digits      |
+--------------+---------------------------------------------------+
| ``%{type}``  | The Avro event type                               |
+--------------+---------------------------------------------------+
| ``%{host}``  | The operating system hostname                     |
+--------------+---------------------------------------------------+
