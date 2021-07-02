#!/bin/sh
set -eu

wget 'https://github.com/mozilla/geckodriver/releases/download/v0.29.1/geckodriver-v0.29.1-linux64.tar.gz'
tar xzf geckodriver-v0.29.1-linux64.tar.gz
rm geckodriver-v0.29.1-linux64.tar.gz
