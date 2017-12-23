curl -s "http://query.yahooapis.com/v1/public/yql" \
   -d q="select * from weather.forecast where woeid=641142  and u='c'" \
   -d format=json | jq -r '.query.results.channel.item.condition.text, .query.results.channel.item.condition.temp' | awk '{key=$0; getline; print key ", " $0" °C";}'
