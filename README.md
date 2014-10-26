mcctl
=====
A program to control Minecraft servers in a robust and simple way.


Dependencies
------------

mcctl depends on wget, systemd and any recent version of Java.


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

That's all there is to it.


More about usage
----------------

To start the mcctl daemon manually:

    # mcctl init -c your-config

This will start the daemon as well as all instances with the `autostart`
property. mcctl will look for instance files in `your-config` if it is a
directory, or treat it as an instance configuration file if it is a file.

To start a particular instance:

    # mcctl start your-instance

To stop an instance:

    # mcctl stop your-instance

To send a command to the server running in a particular instance:

    # mcctl time set 0 -i your-instance

This will set the in-game time to 0 in `your-instance`.

To view the last `n` items of an instance:

    # mcctl log n your-instance

To shut down the mcctl daemon and all running instances:

    # mcctl shutdown

Any command which allows you to select a particular instance will affect *all*
applicable instances if no instance name is given.


Instance files
--------------

Instance files are simple YAML files with the following fields:

  * `server-directory`: this instance's data directory; contains the world file,
    logs and all other Minecraft server data.
    This field is mandatory.

  * `server-jar`: path to Minecraft server JAR file, either absolute or relative
    to `serverDirectory`.
    Defaults to `minecraft_server.1.8.jar`

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
