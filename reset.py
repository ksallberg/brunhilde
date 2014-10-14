# file to test the battleship game...

import json
import requests

def call_rest(obj, addr):
    r = requests.post(addr,
                      data=json.dumps(obj))
    print "Now looking at the reply from: ", addr
    print r.url
    print r.text
    print "\n"

host       = 'http://localhost:28251'

reset_json  = {'password': 'pretty please'}

# Reset the entire game... all players will be deleted
call_rest(reset_json, host + '/battleship/reset/')
