#!/bin/sh
cabal configure && cabal build && strip -s dist/build/mcctl/mcctl && debuild -us -uc -b
