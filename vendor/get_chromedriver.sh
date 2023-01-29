#!/bin/sh
set -eu

VERSION=$(google-chrome --version | sed -E 's/^Google Chrome ([^ ]*) *$/\1/')

wget "https://chromedriver.storage.googleapis.com/$VERSION/chromedriver_linux64.zip"
unzip chromedriver_linux64.zip
rm chromedriver_linux64.zip
