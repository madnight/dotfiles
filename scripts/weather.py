#!/bin/python

import os
import subprocess
import urllib.request, json

city = "Bremen"
# this api key is intentionally exposed to public on github since
# openwathermap api keys can be generated freely for everyone
pub_api_key = "27dea0433cf24e341ea548fdd013403f"
units = "Metric"
unit_key = "C"

try:
    weather = eval(str(urllib.request.urlopen("http://api.openweathermap.org/data/2.5/weather?q={}&APPID={}&units={}".format(city, pub_api_key, units)).read())[2:-1])
    info = weather["weather"][0]["main"].capitalize()
    temp = int(float(weather["main"]["temp"]))
    print("%s, %i °%s" % (info, temp, unit_key))
except:
    bashCommand = "bash " + os.environ['HOME'] + "/scripts/weather.bash"
    process = subprocess.Popen(bashCommand.split(), stdout=subprocess.PIPE)
    output, error = process.communicate()
    print(output)
