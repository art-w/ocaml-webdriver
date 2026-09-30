#!/bin/sh
set -eu

VERSION=0.37.1

wget "https://github.com/mozilla/geckodriver/releases/download/v$VERSION/geckodriver-v$VERSION-linux64.tar.gz"
tar xzf "geckodriver-v$VERSION-linux64.tar.gz"
rm "geckodriver-v$VERSION-linux64.tar.gz"
