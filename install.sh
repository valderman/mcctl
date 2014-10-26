#!/bin/sh

# Build mcctl
cabal update
cabal install -j --only-dependencies
cabal configure && cabal build

# Add group and account for mcctl
sudo addgroup --system mcctl
sudo adduser --system mcctl --home /usr/lib/mcctl --ingroup mcctl

# Install files
sudo mkdir -p /usr/lib/mcctl/instances
sudo mkdir -p /usr/lib/mcctl/backups
sudo mkdir -p /usr/lib/mcctl/worlddata
sudo cp dist/build/mcctl/mcctl /usr/bin/
sudo cp cc.ekblad.mcctl.conf   /etc/dbus-1/system.d/
sudo cp mcctl.service          /lib/systemd/system/
sudo cp default.yaml           /usr/lib/mcctl/instances/
sudo chown -R mcctl:mcctl /usr/lib/mcctl

# Start mcctl on system boot
sudo systemctl enable mcctl

# Download Minecraft server JAR
sudo wget -nc -O /usr/lib/mcctl/minecraft_server.1.8.jar https://s3.amazonaws.com/Minecraft.Download/versions/1.8/minecraft_server.1.8.jar
