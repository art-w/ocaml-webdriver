#!/bin/sh

java -Dwebdriver.chrome.driver="chromedriver" \
     -Dwebdriver.gecko.driver="geckodriver" \
     -jar selenium-server-standalone-3.141.59.jar \
     -role standalone
