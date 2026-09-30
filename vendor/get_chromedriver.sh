#!/bin/sh
set -eu

VERSION=$(google-chrome --version | sed -E 's/^Google Chrome ([^ ]*) *$/\1/')

wget "https://storage.googleapis.com/chrome-for-testing-public/$VERSION/linux64/chromedriver-linux64.zip"
unzip -o chromedriver-linux64.zip
mv chromedriver-linux64/chromedriver chromedriver
rm -r chromedriver-linux64 chromedriver-linux64.zip
