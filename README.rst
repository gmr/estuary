Estuary
=======
Estuary is an application for relaying Avro datum consumed from a RabbitMQ queue,
and storing them in Avro container files locally or in Amazon S3.

Example Configuration
---------------------
.. code:: yaml

    %YAML 1.2
    ---
    # AWS Configuration for S3 uploads
    aws:
      access_key_id: <AWS ACCESS KEY VALUE>
      secret_access_key: <AWS SECRET ACCESS KEY VALUE>
      s3bucket: <BUCKET NAME>

    # If target is "directory", then specify the path for
    # the directory for the container files
    directory: /var/lib/estuary/avro_containers/

    # RabbitMQ Configuration
    rabbitmq:
      host: localhost
      port: 5672
      vhost: /
      user: guest
      password: guest
      queue: estuary

    # The path for the schema files
    schema_path: schema

    # Target should be one of s3 or directory
    target: s3
