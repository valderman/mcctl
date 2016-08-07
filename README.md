mcctl
=====
A program to control Minecraft servers in a robust and simple way.


Dependencies
------------

mcctl depends on wget, bzip2, systemd and any recent version of Java.


Installation
------------

If you are running Debian or any of its derivatives, you should create a
Debian package of mcctl by running `./builddeb.sh`, and then install it using
dpkg as usual: `sudo dpkg -i ../mcctl_0.1-1_amd64.deb`.

Otherwise, run `install.sh` to install and `uninstall.sh` to uninstall.


Quickstart guide
----------------

Start mcctl using systemd:

    # systemctl start mcctl

This will start the mcctl daemon and the default instance installed in
`/usr/lib/mcctl/instances/default.yaml`. You may want to inspect that file
and change any settings that are not to your liking before starting mcctl.

To import an already existing world into mcctl, copy the entire world directory
to `/usr/lib/mcctl/default` and overwrite the `server-properties` field of
`/usr/lib/mcctl/instances/default.yaml` with the contents of your old
`server.properties` file. Then, give ownership of your world directory to
mcctl:

    # chown -R mcctl:mcctl /usr/lib/mcctl/default

For more detailed usage information, consult `mcctl --help`.


Instance files
--------------

Instance files are simple YAML files with the following fields:

  * `server-directory`: this instance's data directory; contains the world file,
    logs and all other Minecraft server data.
    This field is mandatory.

  * `backup-directory`: this instance's backup directory; `mcctl backup` will
    create its backups here, if configured.
    Not configured by default.

  * `server-jar`: path to Minecraft server JAR file, either absolute or relative
    to `serverDirectory`.
    Defaults to `minecraft_server.1.10.2.jar`

  * `autostart`: start this instance when mcctl starts? `true` or `false`.
    Defaults to `true`.

  * `restart`: restart this instance if it crashes?
    Defaults to `true`.

  * `restart-cooldown`: don't restart instance if it was already restarted less
    than `n` seconds ago. Only meaningful if `restart` is `true`.
    Defaults to 3 seconds.

  * `heap-size`: initial heap size of the Minecraft server Java process, in
    megabytes.
    Defaults to 1024 MB.

  * `max-heap-size`: maximum heap size of the Minecraft server Java process, in
    megabytes.
    Defaults to whatever `heap-size` is set to.

  * `server-properties`: contents for the instance's `server.properties` file.
    No checking is done of the syntax of this property; it's copied verbatim
    into place.
    Defaults to the default Minecraft `server.properties` file.

See `default.yaml` for more information.
