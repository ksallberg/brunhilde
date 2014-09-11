import json
import requests
data = {'temperature':'24.3'}
data_json = json.dumps(data)
payload = {'json_playload': data_json, 'apikey': 'YOUR_API_KEY_HERE'}
r = requests.get('http://localhost:2222/emoncms2/api/post', data=payload)
print r.url, r.text, r.content, r.raw
