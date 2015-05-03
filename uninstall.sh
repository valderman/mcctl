#!/bin/sh

# Stop running services
sudo systemctl stop mcctl

# Disable service
sudo systemctl disable mcctl

# Remove files
sudo rm /usr/bin/mcctl
sudo rm /etc/dbus-1/system.d/cc.ekblad.mcctl.conf
sudo rm /lib/systemd/system/mcctl.service
sudo rm -r /usr/lib/mcctl
sudo rm /usr/share/man/man1/mcctl.1
sudo mandb

# Remove group and account
sudo deluser mcctl
sudo delgroup mcctl
