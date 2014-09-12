import json
import requests

def call_rest(obj, addr):
    r = requests.get(addr,
                     data=json.dumps(obj))
    print "Now looking at the reply from: ", addr
    print r.url
    print r.text
    print "\n"

reg_json   = {'player_name': 'berra'}
shoot_json = {'player_name': 'berra', 'shoot_at': [1,2]}
radar_json = {'player_name': 'berra'}
info_json  = {}
reset_json = {'password': 'pretty please'}

call_rest(reg_json,   'http://localhost:2222/battleship/register/')
call_rest(shoot_json, 'http://localhost:2222/battleship/shoot/')
call_rest(radar_json, 'http://localhost:2222/battleship/radar/')
call_rest(info_json,  'http://localhost:2222/battleship/info/')
call_rest(reset_json, 'http://localhost:2222/battleship/reset/')
