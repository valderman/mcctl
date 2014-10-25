mcctl
=====
A program to control Minecraft servers in a robust and simple way.


Installation
------------

Since mcctl uses DBus for its communication, you will want to allow root to run
the mcctl daemon and send it commands. To do this, copy `cc.ekblad.mcctl.conf`
to `/etc/dbus-1/system.d/`. To allow a user other than root to control mcctl,
change `user="root"` to `user="mcctl-user"` in
`cc.ekblad.mcctl.conf`.

In order to have mcctl start and stop cleanly on
system boot/shutdown, assuming that you are using a distribution with
`systemd`, copy `mcctl.service` to `/etc/systemd/system/` and run
`sudo systemctl install mcctl`. If mcctl is not installed in `/usr/bin`,
you will want to modify `mcctl.service` to point to wherever your mcctl binary
resides. You may also want to add a `-c` flag, if you don't want your config
to be read from `/etc/mcctl.yaml`.


Usage
-----

To start the mcctl daemon:

    # mcctl init -c your-config-directory

This will start the daemon as well as all instances with the `autostart`
property. mcctl will look for instance files in `your-config-directory`.

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

Instance files are simple YAML files with the following properties:

  * `server-directory`: this instance's data directory; contains the world file,
    logs and all other Minecraft server data. This field is mandatory.

  * `server-jar`: path to Minecraft server JAR file, either absolute or relative
    to `serverDirectory`. Defaults to `minecraft_server.jar`

  * `autostart`: start this instance when mcctl starts? `true` or `false`.
    Defaults to `true`.

  * `server-properties`: contents for the instance's `server.properties` file.
    No checking is done of the syntax of this property; it's copied verbatim
    into place. Defaults to the default Minecraft `server.properties` file.

See `sample-instance.yaml` for more information.
